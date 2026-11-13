;;; bloom-server-test.el --- ERT tests for bloom-server.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'org-capture)
(require 'bloom-server)

(ert-deftest bloom-server-test-serialize-todo-keywords ()
  (let ((org-todo-keywords
         '((sequence "TODO(t)" "NEXT(n!)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)"))))
    (should (equal (bloom-server--serialize-todo-keywords)
                   "TODO NEXT WAITING | DONE CANCELLED"))))

(ert-deftest bloom-server-test-serialize-capture-templates ()
  (let* ((tmp (make-temp-file "bloom-org-" t))
         (bloom-server-directory tmp)
         (org-directory tmp)
         (org-capture-templates
          `(("t" "Todo" entry (file+headline ,(expand-file-name "tasks.org" tmp) "Inbox")
             "* TODO %?\n%U")
            ("j" "Journal" entry (file+datetree "journal.org")
             "* %?\nEntered on %U" :tree-type week)
            ("g" "Group Header")
            ("n" "Note" entry (file "notes.org") "* %?" :prepend t))))
    (unwind-protect
        (let ((res (bloom-server--serialize-capture-templates)))
          (should (= (length res) 3))
          (should (equal (alist-get 'key (aref res 0)) "t"))
          (should (equal (alist-get 'name (aref res 0)) "Todo"))
          (should (equal (alist-get 'type (alist-get 'target (aref res 0))) "file+headline"))
          (should (equal (alist-get 'file (alist-get 'target (aref res 0))) "tasks.org"))
          (should (equal (alist-get 'key (aref res 1)) "j"))
          (should (equal (alist-get 'treeType (alist-get 'target (aref res 1))) "week"))
          (should (equal (alist-get 'key (aref res 2)) "n"))
          (should (eq (alist-get 'prepend (aref res 2)) t)))
      (delete-directory tmp t))))

(ert-deftest bloom-server-test-path-traversal-rejected ()
  (let* ((tmp (make-temp-file "bloom-org-" t))
         (bloom-server-directory tmp))
    (unwind-protect
        (should-error (bloom-server--resolve-path "../../etc/passwd"))
      (delete-directory tmp t))))

(ert-deftest bloom-server-test-walk-skips-dotfiles-and-shards ()
  (let* ((tmp (make-temp-file "bloom-org-" t))
         (bloom-server-directory tmp))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" tmp) t)
          (write-region "ignored" nil (expand-file-name ".git/config" tmp) nil 'silent)
          (make-directory (expand-file-name "data/ab/123456" tmp) t)
          (write-region "img" nil (expand-file-name "data/ab/123456/pic.svgz" tmp) nil 'silent)
          (write-region "* Hello" nil (expand-file-name "inbox.org" tmp) nil 'silent)
          (let ((files (bloom-server--walk-vault tmp nil nil)))
            (should (= (length files) 1))
            (should (equal (alist-get 'name (car files)) "inbox.org"))))
      (delete-directory tmp t))))

(ert-deftest bloom-server-test-archive-location ()
  (let* ((tmp (make-temp-file "bloom-org-" t))
         (bloom-server-directory tmp)
         (org-directory tmp)
         (org-archive-location "archive/2026.org::* Done"))
    (unwind-protect
        (let ((loc (bloom-server--serialize-archive-location)))
          (should (equal (alist-get 'file loc) "archive/2026.org"))
          (should (equal (alist-get 'headingPath loc) "Done")))
      (delete-directory tmp t))))

(ert-deftest bloom-server-test-serialize-saved-tags ()
  (let ((org-tag-alist '((:startgroup) ("work" . ?w) ("home" . ?h) (:endgroup) ("urgent")))
        (org-tag-persistent-alist '(("work" . ?w) ("reading" . ?r))))
    (should (equal (bloom-server--serialize-saved-tags)
                   ["work" "home" "urgent" "reading"]))))

(ert-deftest bloom-server-test-whitelisted-hosts ()
  (let ((bloom-server-allowed-hosts '("127.0.0.1" "10.0.0.2")))
    (should (bloom-server--host-allowed-p "127.0.0.1"))
    (should (bloom-server--host-allowed-p "10.0.0.2"))
    (should-not (bloom-server--host-allowed-p "10.0.0.3"))
    (should-not (bloom-server--host-allowed-p nil))))

(provide 'bloom-server-test)
;;; bloom-server-test.el ends here
