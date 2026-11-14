# bloom-emacs-server

A lightweight, zero-dependency HTTP/JSON server inside Emacs for the [Bloom](https://github.com/SSD2019/bloom) Android Org-mode app (designed for use over WireGuard or a private network).

## Features

- **Buffer-aware file access**: Reads from live Emacs buffers when open and reverts visiting buffers cleanly on writes.
- **Live Org-mode config sync**: Exposes `org-todo-keywords`, `org-capture-templates` (with shortcut keys), `org-tag-alist` / `org-tag-persistent-alist`, `org-default-priority`, `org-archive-location`, `org-deadline-warning-days`, `org-attach-id-dir`, and `org-roam-dailies-directory`.
- **Binary attachment streaming**: Serves and accepts `org-attach` / `data/` files including `.svg`, `.svgz`, and raster images.
- **Host whitelist**: Only starts on machines whose hostname (`(system-name)`) is listed in `bloom-server-allowed-hosts`.

## Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/SSD2019/bloom-emacs-server.git /path/to/bloom-emacs-server
   ```
2. Add to your Emacs configuration:
   ```elisp
   (add-to-list 'load-path "/path/to/bloom-emacs-server")
   (require 'bloom-server)

   ;; Bind to your VPN/WireGuard interface IP or 0.0.0.0
   (setq bloom-server-host "0.0.0.0")
   (setq bloom-server-port 8765)

   ;; Optional: override org-directory as the vault root
   ;; (setq bloom-server-directory "~/org")

   ;; Only allow the server to start on whitelisted machine hostnames
   (setq bloom-server-allowed-hosts '("my-desktop"))

   (bloom-server-start)
   ```

## Testing

```bash
emacs --batch -Q -L . -l ert -l bloom-server -l bloom-server-test -f ert-run-tests-batch-and-exit
```
