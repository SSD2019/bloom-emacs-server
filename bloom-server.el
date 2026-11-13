;;; bloom-server.el --- HTTP/JSON server for the Bloom Android Org-mode app -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bloom contributors
;; Package-Requires: ((emacs "27.1"))
;; Keywords: org, outlines, network

;;; Commentary:
;;
;; A lightweight, zero-dependency HTTP/JSON server running inside Emacs so the
;; Bloom Android app can read/write `.org` files, stream binary attachments
;; (including `.svgz` and `org-attach` files), and sync live Org-mode settings
;; (`org-todo-keywords`, `org-capture-templates`, `org-default-priority`,
;; `org-archive-location`, `org-deadline-warning-days`, etc.) over WireGuard.
;;
;; Usage:
;;   (require 'bloom-server)
;;   (setq bloom-server-host "0.0.0.0") ;; or your WireGuard IP, e.g. "10.0.0.1"
;;   (setq bloom-server-port 8765)
;;   (bloom-server-start)

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url-parse)
(require 'url-util)

(defgroup bloom-server nil
  "HTTP/JSON server for Bloom Android client."
  :group 'org
  :prefix "bloom-server-")

(defcustom bloom-server-host "127.0.0.1"
  "IP address or host to bind the Bloom HTTP server to.
Set this to your WireGuard interface IP (or \"0.0.0.0\") for remote access."
  :type 'string
  :group 'bloom-server)

(defcustom bloom-server-port 8765
  "TCP port for the Bloom HTTP server."
  :type 'integer
  :group 'bloom-server)

(defcustom bloom-server-directory nil
  "Root directory exposed to Bloom as the vault.
When nil, `org-directory' is used."
  :type '(choice (const :tag "Use org-directory" nil) directory)
  :group 'bloom-server)

(defcustom bloom-server-allowed-hosts '("127.0.0.1")
  "List of whitelisted client IP addresses or hostnames allowed to connect.
Connections from any peer not matching this list are immediately closed.
For WireGuard, add your phone's WireGuard IP, e.g. `'(\"127.0.0.1\" \"10.0.0.2\")'."
  :type '(repeat string)
  :group 'bloom-server)

(defvar bloom-server--process nil
  "The active network server process, or nil when stopped.")

(defconst bloom-server--version "1.0.0")

;; ---------------------------------------------------------------------------
;; Path & Directory Helpers
;; ---------------------------------------------------------------------------

(defun bloom-server--root-dir ()
  "Return the expanded, trailing-slash-free vault root directory."
  (let ((dir (or bloom-server-directory
                 (and (boundp 'org-directory) org-directory)
                 "~/org")))
    (directory-file-name (expand-file-name dir))))

(defun bloom-server--resolve-path (rel-path)
  "Resolve REL-PATH inside `bloom-server--root-dir'.
Signals an error if REL-PATH attempts path traversal outside the root."
  (let* ((root (file-name-as-directory (bloom-server--root-dir)))
         (clean (string-remove-prefix "/" (or rel-path "")))
         (abs (expand-file-name clean root)))
    (unless (or (string= (directory-file-name abs) (directory-file-name root))
                (string-prefix-p root abs))
      (error "Path escapes vault root: %s" rel-path))
    abs))

(defun bloom-server--relative-path (abs-path)
  "Return ABS-PATH relative to `bloom-server--root-dir' using `/` separators."
  (let* ((root (file-name-as-directory (bloom-server--root-dir)))
         (expanded (expand-file-name abs-path)))
    (if (string-prefix-p root expanded)
        (string-remove-prefix root expanded)
      (file-relative-name expanded root))))

(defun bloom-server--is-attach-shard-p (name)
  "Return non-nil if NAME looks like an `org-attach' shard directory."
  (or (string= name "__")
      (string-match-p "\\`[0-9a-fA-F]\\{2\\}\\'" name)
      (string-match-p "\\`[0-9]\\{6\\}\\'" name)))

(defun bloom-server--should-skip-dir-p (rel-dir-parts)
  "Return non-nil if directory components REL-DIR-PARTS should be skipped in vault list."
  (let ((leaf (car (last rel-dir-parts))))
    (or (string-prefix-p "." leaf)
        (and (= (length rel-dir-parts) 2)
             (string= (car rel-dir-parts) "data")
             (bloom-server--is-attach-shard-p leaf)))))

(defun bloom-server--file-entry (abs-path rel-path)
  "Build an alist representing `FileEntry' for ABS-PATH at REL-PATH."
  (let* ((attrs (file-attributes abs-path 'integer))
         (mtime-sec (float-time (file-attribute-modification-time attrs)))
         (mtime-ms (truncate (* mtime-sec 1000)))
         (size (or (file-attribute-size attrs) 0)))
    `((name . ,rel-path)
      (lastModified . ,mtime-ms)
      (size . ,size))))

(defun bloom-server--walk-vault (dir rel-parts acc)
  "Recursively collect non-hidden files under DIR into ACC."
  (dolist (entry (directory-files-and-attributes dir nil nil t))
    (let* ((name (car entry))
           (attrs (cdr entry))
           (is-dir (eq t (file-attribute-type attrs))))
      (unless (member name '("." ".."))
        (let* ((abs (expand-file-name name dir))
               (next-parts (append rel-parts (list name)))
               (rel (string-join next-parts "/")))
          (cond
           (is-dir
            (unless (bloom-server--should-skip-dir-p next-parts)
              (setq acc (bloom-server--walk-vault abs next-parts acc))))
           ((not (string-prefix-p "." name))
            (push (bloom-server--file-entry abs rel) acc)))))))
  acc)

(defun bloom-server--prune-empty-dirs (dir)
  "Remove DIR and empty ancestor directories up to `bloom-server--root-dir'."
  (let ((root (directory-file-name (bloom-server--root-dir)))
        (cur (directory-file-name (expand-file-name dir))))
    (while (and (not (string= cur root))
                (string-prefix-p (file-name-as-directory root)
                                 (file-name-as-directory cur))
                (file-directory-p cur)
                (null (directory-files cur nil "\\`[^.]" t))
                (null (cl-remove-if (lambda (f) (member f '("." "..")))
                                    (directory-files cur nil nil t))))
      (delete-directory cur)
      (setq cur (directory-file-name (file-name-directory cur))))))

;; ---------------------------------------------------------------------------
;; Buffer-Aware File Operations
;; ---------------------------------------------------------------------------

(defun bloom-server--read-file-text (abs-path)
  "Read ABS-PATH as UTF-8 text, preferring a live visiting Emacs buffer if present."
  (let ((buf (find-buffer-visiting abs-path)))
    (if (and buf (buffer-live-p buf))
        (with-current-buffer buf
          (buffer-substring-no-properties (point-min) (point-max)))
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8))
          (insert-file-contents abs-path))
        (buffer-string)))))

(defun bloom-server--write-file-text (abs-path content)
  "Write UTF-8 CONTENT to ABS-PATH and refresh any visiting Emacs buffer."
  (make-directory (file-name-directory abs-path) t)
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region content nil abs-path nil 'silent))
  (let ((buf (find-buffer-visiting abs-path)))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (pt (point)))
          (revert-buffer t t t)
          (goto-char (min pt (point-max))))))))

;; ---------------------------------------------------------------------------
;; Org Configuration Serialization
;; ---------------------------------------------------------------------------

(defun bloom-server--clean-todo-keyword (kw)
  "Strip Emacs fast-access/logging decorators like `(t)', `@', `!' from KW."
  (if (string-match "\\`\\([^(]+\\)(" kw)
      (match-string 1 kw)
    kw))

(defun bloom-server--serialize-todo-keywords ()
  "Serialize `org-todo-keywords' into Bloom's `TODO ... | DONE ...' string."
  (let ((kws (and (boundp 'org-todo-keywords) org-todo-keywords)))
    (if (null kws)
        "TODO | DONE"
      (let ((active '())
            (done '()))
        (dolist (seq kws)
          (let* ((items (if (and (listp seq) (symbolp (car seq)))
                            (cdr seq)
                          (if (listp seq) seq (list seq))))
                 (seen-bar nil))
            (dolist (item items)
              (cond
               ((string= item "|") (setq seen-bar t))
               ((stringp item)
                (let ((clean (string-trim (bloom-server--clean-todo-keyword item))))
                  (unless (string-empty-p clean)
                    (if seen-bar
                        (unless (member clean done) (push clean done))
                      (unless (member clean active) (push clean active))))))))))
        (let ((a (nreverse active))
              (d (nreverse done)))
          (if (and (null a) (null d))
              "TODO | DONE"
            (string-trim
             (concat (string-join a " ")
                     (when d (concat " | " (string-join d " ")))))))))))

(defun bloom-server--resolve-target-file (file-spec)
  "Resolve FILE-SPEC from an `org-capture-templates' target to a vault-relative path."
  (let* ((evaluated (cond
                     ((stringp file-spec) file-spec)
                     ((symbolp file-spec) (symbol-value file-spec))
                     ((listp file-spec) (eval file-spec t))
                     (t (format "%s" file-spec))))
         (expanded (expand-file-name evaluated (bloom-server--root-dir))))
    (bloom-server--relative-path expanded)))

(defun bloom-server--serialize-capture-target (target plist)
  "Serialize an `org-capture-templates' TARGET and PLIST."
  (let* ((tree-type-sym (plist-get plist :tree-type))
         (tree-type (pcase tree-type-sym
                      ('week "week")
                      ('month "month")
                      (_ "day"))))
    (pcase target
      (`(file ,f)
       `((type . "file")
         (file . ,(bloom-server--resolve-target-file f))))
      (`(file+headline ,f ,h)
       `((type . "file+headline")
         (file . ,(bloom-server--resolve-target-file f))
         (path . ,(vector h))))
      (`(file+olp ,f . ,olp)
       `((type . "file+olp")
         (file . ,(bloom-server--resolve-target-file f))
         (path . ,(vconcat olp))))
      (`(file+datetree ,f)
       `((type . "file+datetree")
         (file . ,(bloom-server--resolve-target-file f))
         (treeType . ,tree-type)))
      (`(file+olp+datetree ,f . ,olp)
       `((type . "file+olp+datetree")
         (file . ,(bloom-server--resolve-target-file f))
         (path . ,(vconcat olp))
         (treeType . ,tree-type)))
      (`(id ,id)
       `((type . "id")
         (id . ,id)))
      (_ nil))))

(defun bloom-server--serialize-capture-templates ()
  "Serialize `org-capture-templates' to a vector of template alists."
  (let ((templates (and (boundp 'org-capture-templates) org-capture-templates))
        (result '()))
    (dolist (entry templates)
      (when (and (listp entry) (>= (length entry) 4))
        (let* ((key (nth 0 entry))
               (desc (nth 1 entry))
               (type (nth 2 entry))
               (target (nth 3 entry))
               (tmpl (or (nth 4 entry) ""))
               (plist (nthcdr 5 entry))
               (prepend (if (plist-get plist :prepend) t :json-false))
               (serialized-target (ignore-errors
                                    (bloom-server--serialize-capture-target target plist))))
          (when (and (stringp key)
                     (stringp desc)
                     (memq type '(entry item checkitem plain nil))
                     serialized-target)
            (push `((key . ,key)
                    (name . ,desc)
                    (entryType . ,(symbol-name (or type 'entry)))
                    (target . ,serialized-target)
                    (template . ,(if (stringp tmpl) tmpl ""))
                    (prepend . ,prepend))
                  result)))))
    (vconcat (nreverse result))))

(defun bloom-server--serialize-archive-location ()
  "Parse `org-archive-location' into `(file . ...)' and `(headingPath . [...])'."
  (let ((loc (and (boundp 'org-archive-location) org-archive-location)))
    (if (and (stringp loc) (string-match "\\`\\(.*?\\)::\\(.*\\)\\'" loc))
        (let* ((file-part (string-trim (match-string 1 loc)))
               (heading-part (string-trim (string-remove-prefix "*" (string-trim (match-string 2 loc)))))
               (rel-file (unless (string-empty-p file-part)
                           (ignore-errors (bloom-server--resolve-target-file file-part)))))
          `((file . ,rel-file)
            (headingPath . ,(if (string-empty-p heading-part) "" heading-part))))
      `((file . nil)
        (headingPath . "")))))

(defun bloom-server--serialize-saved-tags ()
  "Collect saved tag names from `org-tag-alist' and `org-tag-persistent-alist'."
  (let ((sources (list (and (boundp 'org-tag-alist) org-tag-alist)
                       (and (boundp 'org-tag-persistent-alist) org-tag-persistent-alist)))
        (tags '()))
    (dolist (alist sources)
      (dolist (item alist)
        (let ((tag (cond
                    ((stringp item) item)
                    ((and (consp item) (stringp (car item))) (car item))
                    (t nil))))
          (when (and tag (not (string-empty-p tag)) (not (member tag tags)))
            (push tag tags)))))
    (vconcat (nreverse tags))))

(defun bloom-server--build-config ()
  "Collect live Org-mode configuration into an alist."
  (let* ((def-prio (and (boundp 'org-default-priority)
                        (char-to-string org-default-priority)))
         (warn-days (or (and (boundp 'org-deadline-warning-days)
                             (numberp org-deadline-warning-days)
                             (abs org-deadline-warning-days))
                        14))
         (attach-dir (or (and (boundp 'org-attach-id-dir)
                              (stringp org-attach-id-dir)
                              (ignore-errors (bloom-server--resolve-target-file org-attach-id-dir)))
                         "data"))
         (roam-dir (when (and (boundp 'org-roam-directory) (stringp org-roam-directory))
                     (ignore-errors (bloom-server--resolve-target-file org-roam-directory))))
         (dailies-dir (when (and (boundp 'org-roam-dailies-directory) (stringp org-roam-dailies-directory))
                        (string-remove-suffix "/" org-roam-dailies-directory))))
    `((version . ,bloom-server--version)
      (orgDirectory . ,(bloom-server--root-dir))
      (todoKeywords . ,(bloom-server--serialize-todo-keywords))
      (defaultPriority . ,def-prio)
      (deadlineWarningDays . ,warn-days)
      (archiveLocation . ,(bloom-server--serialize-archive-location))
      (attachIdDir . ,attach-dir)
      (roamDirectory . ,roam-dir)
      (dailiesDirectory . ,dailies-dir)
      (savedTags . ,(bloom-server--serialize-saved-tags))
      (captureTemplates . ,(bloom-server--serialize-capture-templates)))))

;; ---------------------------------------------------------------------------
;; HTTP Parsing & Response Helpers
;; ---------------------------------------------------------------------------

(defun bloom-server--mime-for-path (path)
  "Guess Content-Type for PATH."
  (pcase (downcase (or (file-name-extension path) ""))
    ("org" "text/plain; charset=utf-8")
    ("json" "application/json; charset=utf-8")
    ("png" "image/png")
    ((or "jpg" "jpeg") "image/jpeg")
    ("gif" "image/gif")
    ("webp" "image/webp")
    ("svg" "image/svg+xml")
    ("svgz" "application/octet-stream")
    ("pdf" "application/pdf")
    (_ "application/octet-stream")))

(defun bloom-server--send-response (proc status content-type body-bytes)
  "Send an HTTP/1.1 response on PROC with STATUS, CONTENT-TYPE, and unibyte BODY-BYTES."
  (when (process-live-p proc)
    (let* ((status-text (pcase status
                          (200 "200 OK")
                          (201 "201 Created")
                          (204 "204 No Content")
                          (400 "400 Bad Request")
                          (403 "403 Forbidden")
                          (404 "404 Not Found")
                          (409 "409 Conflict")
                          (_ "500 Internal Server Error")))
           (payload (if (multibyte-string-p body-bytes)
                        (encode-coding-string body-bytes 'utf-8)
                      body-bytes))
           (headers (format "HTTP/1.1 %s\r\nContent-Type: %s\r\nContent-Length: %d\r\nConnection: close\r\n\r\n"
                            status-text content-type (string-bytes payload))))
      (process-send-string proc (concat (encode-coding-string headers 'raw-text) payload))
      (delete-process proc))))

(defun bloom-server--send-json (proc status alist)
  "Send ALIST encoded as UTF-8 JSON on PROC with HTTP STATUS."
  (let ((json-encoding-pretty-print nil))
    (bloom-server--send-response
     proc status "application/json; charset=utf-8"
     (encode-coding-string (json-encode alist) 'utf-8))))

(defun bloom-server--send-error (proc status message)
  "Send a JSON error payload with STATUS and MESSAGE on PROC."
  (bloom-server--send-json proc status `((error . ,message))))

(defun bloom-server--parse-query (query-str)
  "Parse URL QUERY-STR into an alist of `(key . value)' strings."
  (when (and query-str (not (string-empty-p query-str)))
    (mapcar (lambda (pair)
              (let ((parts (split-string pair "=")))
                (cons (url-unhex-string (car parts) t)
                      (url-unhex-string (or (cadr parts) "") t))))
            (split-string query-str "&" t))))

;; ---------------------------------------------------------------------------
;; Route Dispatch
;; ---------------------------------------------------------------------------

(defun bloom-server--handle-request (proc method raw-target _headers body)
  "Dispatch one HTTP request on PROC."
  (condition-case err
      (let* ((url (url-generic-parse-url raw-target))
             (path (url-unhex-string (or (url-filename url) "/")))
             (q-idx (string-match "\\?" path))
             (route (if q-idx (substring path 0 q-idx) path))
             (query-str (when q-idx (substring path (1+ q-idx))))
             (params (bloom-server--parse-query query-str))
             (q-path (cdr (assoc "path" params)))
             (q-dir (cdr (assoc "dir" params))))
        (cond
         ;; Health check
         ((and (string= method "GET") (string= route "/v1/ping"))
          (bloom-server--send-json
           proc 200
           `((status . "ok")
             (version . ,bloom-server--version)
             (orgDirectory . ,(bloom-server--root-dir)))))

         ;; Live Org configuration
         ((and (string= method "GET") (string= route "/v1/config"))
          (bloom-server--send-json proc 200 (bloom-server--build-config)))

         ;; List vault files recursively
         ((and (string= method "GET") (string= route "/v1/files"))
          (let* ((root (bloom-server--root-dir))
                 (entries (if (file-directory-p root)
                              (nreverse (bloom-server--walk-vault root nil nil))
                            '())))
            (bloom-server--send-json proc 200 `((files . ,(vconcat entries))))))

         ;; List single directory (non-recursive)
         ((and (string= method "GET") (string= route "/v1/files/dir"))
          (let* ((dir (bloom-server--resolve-path (or q-dir "")))
                 (prefix (let ((trimmed (string-trim (or q-dir "") "/+" "/+")))
                           (if (string-empty-p trimmed) "" (concat trimmed "/"))))
                 (entries '()))
            (when (file-directory-p dir)
              (dolist (entry (directory-files-and-attributes dir nil nil t))
                (let* ((name (car entry))
                       (attrs (cdr entry))
                       (is-dir (eq t (file-attribute-type attrs))))
                  (when (and (not is-dir)
                             (not (string-prefix-p "." name)))
                    (push (bloom-server--file-entry
                           (expand-file-name name dir)
                           (concat prefix name))
                          entries)))))
            (bloom-server--send-json proc 200 `((files . ,(vconcat (nreverse entries)))))))

         ;; Stat single file
         ((and (string= method "GET") (string= route "/v1/file/stat"))
          (let ((abs (bloom-server--resolve-path q-path)))
            (if (file-regular-p abs)
                (bloom-server--send-json
                 proc 200 (bloom-server--file-entry abs (bloom-server--relative-path abs)))
              (bloom-server--send-error proc 404 "File not found"))))

         ;; Read text file (buffer-aware)
         ((and (string= method "GET") (string= route "/v1/file"))
          (let ((abs (bloom-server--resolve-path q-path)))
            (if (file-regular-p abs)
                (let ((entry (bloom-server--file-entry abs (bloom-server--relative-path abs)))
                      (content (bloom-server--read-file-text abs)))
                  (bloom-server--send-json
                   proc 200 (append entry `((content . ,content)))))
              (bloom-server--send-error proc 404 "File not found"))))

         ;; Write text file (buffer-aware)
         ((and (string= method "PUT") (string= route "/v1/file"))
          (let* ((abs (bloom-server--resolve-path q-path))
                 (text (decode-coding-string body 'utf-8)))
            (bloom-server--write-file-text abs text)
            (bloom-server--send-json
             proc 200 (bloom-server--file-entry abs (bloom-server--relative-path abs)))))

         ;; Create empty file
         ((and (string= method "POST") (string= route "/v1/file/create"))
          (let ((abs (bloom-server--resolve-path q-path)))
            (if (file-exists-p abs)
                (bloom-server--send-error proc 409 "File already exists")
              (make-directory (file-name-directory abs) t)
              (write-region "" nil abs nil 'silent)
              (bloom-server--send-json
               proc 201 (bloom-server--file-entry abs (bloom-server--relative-path abs))))))

         ;; Rename / move file
         ((and (string= method "POST") (string= route "/v1/file/rename"))
          (let* ((payload (json-read-from-string (decode-coding-string body 'utf-8)))
                 (old-abs (bloom-server--resolve-path (cdr (assq 'oldPath payload))))
                 (new-abs (bloom-server--resolve-path (cdr (assq 'newPath payload)))))
            (cond
             ((not (file-exists-p old-abs))
              (bloom-server--send-error proc 404 "Source file not found"))
             ((file-exists-p new-abs)
              (bloom-server--send-error proc 409 "Target file already exists"))
             (t
              (make-directory (file-name-directory new-abs) t)
              (rename-file old-abs new-abs)
              (let ((buf (find-buffer-visiting old-abs)))
                (when (and buf (buffer-live-p buf))
                  (with-current-buffer buf
                    (set-visited-file-name new-abs t t))))
              (bloom-server--prune-empty-dirs (file-name-directory old-abs))
              (bloom-server--send-json proc 200 `((ok . t)))))))

         ;; Delete file
         ((and (string= method "DELETE") (string= route "/v1/file"))
          (let ((abs (bloom-server--resolve-path q-path)))
            (if (not (file-regular-p abs))
                (bloom-server--send-error proc 404 "File not found")
              (let ((buf (find-buffer-visiting abs)))
                (when (and buf (buffer-live-p buf))
                  (with-current-buffer buf
                    (set-buffer-modified-p nil)
                    (kill-buffer buf))))
              (delete-file abs)
              (bloom-server--prune-empty-dirs (file-name-directory abs))
              (bloom-server--send-json proc 200 `((ok . t))))))

         ;; --- Raw Attachment Endpoints ---

         ((and (string= method "GET") (string= route "/v1/raw/stat"))
          (let ((abs (bloom-server--resolve-path q-path)))
            (if (file-regular-p abs)
                (bloom-server--send-json
                 proc 200 (bloom-server--file-entry abs (bloom-server--relative-path abs)))
              (bloom-server--send-error proc 404 "Not found"))))

         ((and (string= method "GET") (string= route "/v1/raw/dir-exists"))
          (let ((abs (bloom-server--resolve-path q-dir)))
            (bloom-server--send-json proc 200 `((exists . ,(if (file-directory-p abs) t :json-false))))))

         ((and (string= method "GET") (string= route "/v1/raw/list"))
          (let* ((abs (bloom-server--resolve-path (or q-dir "")))
                 (prefix (let ((trimmed (string-trim (or q-dir "") "/+" "/+")))
                           (if (string-empty-p trimmed) "" (concat trimmed "/"))))
                 (files '())
                 (subdirs '()))
            (when (file-directory-p abs)
              (dolist (entry (directory-files-and-attributes abs nil nil t))
                (let* ((name (car entry))
                       (attrs (cdr entry))
                       (is-dir (eq t (file-attribute-type attrs))))
                  (unless (member name '("." ".."))
                    (if is-dir
                        (push name subdirs)
                      (push (bloom-server--file-entry
                             (expand-file-name name abs)
                             (concat prefix name))
                            files))))))
            (bloom-server--send-json
             proc 200 `((files . ,(vconcat (nreverse files)))
                        (subdirs . ,(vconcat (nreverse subdirs)))))))

         ((and (string= method "GET") (string= route "/v1/raw/file"))
          (let ((abs (bloom-server--resolve-path q-path)))
            (if (not (file-regular-p abs))
                (bloom-server--send-error proc 404 "Not found")
              (let ((bytes (with-temp-buffer
                             (set-buffer-multibyte nil)
                             (insert-file-contents-literally abs)
                             (buffer-string))))
                (bloom-server--send-response
                 proc 200 (bloom-server--mime-for-path abs) bytes)))))

         ((and (string= method "PUT") (string= route "/v1/raw/file"))
          (let ((abs (bloom-server--resolve-path q-path)))
            (make-directory (file-name-directory abs) t)
            (let ((coding-system-for-write 'no-conversion))
              (with-temp-buffer
                (set-buffer-multibyte nil)
                (insert body)
                (write-region (point-min) (point-max) abs nil 'silent)))
            (bloom-server--send-json
             proc 200 (bloom-server--file-entry abs (bloom-server--relative-path abs)))))

         ((and (string= method "DELETE") (string= route "/v1/raw/file"))
          (let ((abs (bloom-server--resolve-path q-path)))
            (if (file-regular-p abs)
                (progn
                  (delete-file abs)
                  (bloom-server--send-json proc 200 `((ok . t))))
              (bloom-server--send-error proc 404 "Not found"))))

         ((and (string= method "DELETE") (string= route "/v1/raw/dir"))
          (let ((abs (bloom-server--resolve-path q-dir)))
            (if (file-directory-p abs)
                (progn
                  (delete-directory abs t)
                  (bloom-server--send-json proc 200 `((ok . t))))
              (bloom-server--send-error proc 404 "Directory not found"))))

         ((and (string= method "POST") (string= route "/v1/raw/rename"))
          (let* ((payload (json-read-from-string (decode-coding-string body 'utf-8)))
                 (old-abs (bloom-server--resolve-path (cdr (assq 'path payload))))
                 (new-leaf (cdr (assq 'newLeaf payload)))
                 (new-abs (expand-file-name new-leaf (file-name-directory old-abs))))
            (cond
             ((not (file-regular-p old-abs))
              (bloom-server--send-error proc 404 "Not found"))
             ((file-exists-p new-abs)
              (bloom-server--send-error proc 409 "Target already exists"))
             (t
              (rename-file old-abs new-abs)
              (bloom-server--send-json proc 200 `((ok . t)))))))

         ((and (string= method "POST") (string= route "/v1/raw/move-dir"))
          (let* ((payload (json-read-from-string (decode-coding-string body 'utf-8)))
                 (src-abs (directory-file-name (bloom-server--resolve-path (cdr (assq 'src payload)))))
                 (dst-parent-abs (bloom-server--resolve-path (cdr (assq 'dstParent payload))))
                 (dst-abs (expand-file-name (file-name-nondirectory src-abs) dst-parent-abs)))
            (cond
             ((not (file-directory-p src-abs))
              (bloom-server--send-error proc 404 "Source directory not found"))
             ((file-exists-p dst-abs)
              (bloom-server--send-error proc 409 "Destination already exists"))
             (t
              (make-directory dst-parent-abs t)
              (rename-file src-abs dst-abs)
              (bloom-server--send-json proc 200 `((ok . t)))))))

         (t
          (bloom-server--send-error proc 404 (format "Unknown route: %s %s" method route)))))
    (error
     (bloom-server--send-error proc 500 (error-message-string err)))))

;; ---------------------------------------------------------------------------
;; Network Server Loop
;; ---------------------------------------------------------------------------

(defun bloom-server--filter (proc chunk)
  "Accumulate incoming bytes CHUNK for client PROC and dispatch when complete."
  (let ((buf (process-buffer proc)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (goto-char (point-max))
        (insert chunk)
        (goto-char (point-min))
        (when (re-search-forward "\r?\n\r?\n" nil t)
          (let* ((header-end (point))
                 (header-str (buffer-substring-no-properties (point-min) header-end))
                 (lines (split-string header-str "\r?\n" t))
                 (req-line (split-string (car lines) " "))
                 (method (nth 0 req-line))
                 (target (nth 1 req-line))
                 (content-length
                  (or (cl-loop for line in (cdr lines)
                               when (string-match "\\`Content-Length:[ \t]*\\([0-9]+\\)" line)
                               return (string-to-number (match-string 1 line)))
                      0))
                 (body-bytes-have (- (position-bytes (point-max))
                                     (position-bytes header-end))))
            (when (>= body-bytes-have content-length)
              (let ((body (buffer-substring-no-properties
                           header-end
                           (byte-to-position (+ (position-bytes header-end) content-length)))))
                (erase-buffer)
                (bloom-server--handle-request proc method target (cdr lines) body)))))))))

(defun bloom-server--sentinel (proc _event)
  "Clean up client buffer when PROC closes."
  (when (memq (process-status proc) '(closed exit signal failed))
    (let ((buf (process-buffer proc)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(defun bloom-server--accept-sentinel (_proc _event)
  "Unused accept sentinel.")

(defun bloom-server--peer-ip (client)
  "Return the remote IP string (without port) for CLIENT process."
  (let ((remote (process-contact client :remote)))
    (when remote
      (format-network-address remote t))))

(defun bloom-server--resolve-allowed-ips ()
  "Expand `bloom-server-allowed-hosts' into a list of IP strings."
  (let ((ips '()))
    (dolist (entry bloom-server-allowed-hosts)
      (when (stringp entry)
        (let ((trimmed (string-trim entry)))
          (unless (string-empty-p trimmed)
            (push trimmed ips)
            (when (fboundp 'network-lookup-address-info)
              (dolist (addr (ignore-errors (network-lookup-address-info trimmed)))
                (let ((ip (ignore-errors (format-network-address addr t))))
                  (when ip (push ip ips)))))))))
    (delete-dups ips)))

(defun bloom-server--host-allowed-p (peer-ip)
  "Return non-nil if PEER-IP is whitelisted in `bloom-server-allowed-hosts'."
  (and (stringp peer-ip)
       (member peer-ip (bloom-server--resolve-allowed-ips))))

;;;###autoload
(defun bloom-server-start (&optional port host)
  "Start the Bloom HTTP server on HOST (default `bloom-server-host') and PORT."
  (interactive)
  (when (and bloom-server--process (process-live-p bloom-server--process))
    (bloom-server-stop))
  (let ((bind-host (or host bloom-server-host))
        (bind-port (or port bloom-server-port)))
    (setq bloom-server--process
          (make-network-process
           :name "bloom-server"
           :server t
           :host bind-host
           :service bind-port
           :family 'ipv4
           :coding 'binary
           :noquery t
           :reuseaddr t
           :filter #'bloom-server--filter
           :sentinel #'bloom-server--sentinel
           :log (lambda (_server client _msg)
                  (let ((peer-ip (bloom-server--peer-ip client)))
                    (if (not (bloom-server--host-allowed-p peer-ip))
                        (progn
                          (message "Bloom server rejected connection from non-whitelisted host: %s" peer-ip)
                          (delete-process client))
                      (let ((buf (generate-new-buffer " *bloom-client*")))
                        (with-current-buffer buf (set-buffer-multibyte nil))
                        (set-process-buffer client buf)
                        (set-process-coding-system client 'binary 'binary)
                        (set-process-filter client #'bloom-server--filter)
                        (set-process-sentinel client #'bloom-server--sentinel)))))))
    (message "Bloom server listening on http://%s:%d (root: %s)"
             bind-host bind-port (bloom-server--root-dir))
    bloom-server--process))

;;;###autoload
(defun bloom-server-stop ()
  "Stop the Bloom HTTP server if running."
  (interactive)
  (when (and bloom-server--process (process-live-p bloom-server--process))
    (delete-process bloom-server--process)
    (message "Bloom server stopped"))
  (setq bloom-server--process nil))

;;;###autoload
(defun bloom-server-status ()
  "Report whether the Bloom HTTP server is running."
  (interactive)
  (if (and bloom-server--process (process-live-p bloom-server--process))
      (message "Bloom server running on http://%s:%d (root: %s)"
               bloom-server-host bloom-server-port (bloom-server--root-dir))
    (message "Bloom server is stopped")))

(provide 'bloom-server)
;;; bloom-server.el ends here
