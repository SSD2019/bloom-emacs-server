# bloom-emacs-server

A lightweight, zero-dependency HTTP/JSON server inside Emacs for the [Bloom](https://github.com/SSD2019/bloom) Android Org-mode app over WireGuard.

## Features

- **Buffer-aware file access**: Reads from live Emacs buffers when open and reverts visiting buffers cleanly on writes.
- **Live Org-mode config sync**: Exposes `org-todo-keywords`, `org-capture-templates` (with shortcut keys), `org-tag-alist` / `org-tag-persistent-alist`, `org-default-priority`, `org-archive-location`, `org-deadline-warning-days`, `org-attach-id-dir`, and `org-roam-dailies-directory`.
- **Binary attachment streaming**: Serves and accepts `org-attach` / `data/` files including `.svgz` and images.
- **Host whitelist**: Only accepts connections from IPs/hostnames listed in `bloom-server-allowed-hosts`.

## Installation

```elisp
(add-to-list 'load-path "~/.vim/emacs_plugin/bloom-emacs-server")
(require 'bloom-server)

;; Bind to your WireGuard interface IP or 0.0.0.0
(setq bloom-server-host "0.0.0.0")
(setq bloom-server-port 8765)

;; Only allow the server to start on whitelisted machine hostnames
(setq bloom-server-allowed-hosts '("AMDdesktop"))

(bloom-server-start)
```

## Testing

```bash
emacs --batch -Q -L . -l ert -l bloom-server -l bloom-server-test -f ert-run-tests-batch-and-exit
```

