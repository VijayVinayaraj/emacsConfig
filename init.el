;; (load "~/.emacs.d/loadDirectory.el")
;;(load-directory "~/.emacs.d/customFunctions")

(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

(require 'emacs-config)
(require 'terminal-config)
(require 'elisp-config)
(require 'keybindings-config)
(require 'miscellaneous-config)
(require 'meow-config)
(require 'lsp-config)
(require 'org-config)
(require 'lint-format-config)
(require 'custom-function-config)
(require 'programming-config)

(use-package howm
  :ensure t
  :init
  ;; Where to store the files?
  (setq howm-directory "~/orgRoam/pages/")
  (setq howm-home-directory howm-directory)
  ;; What format to use for the files?
  (setq howm-file-name-format "%Y-%m-%d-%H%M%S.org")
  (setq howm-view-title-header "*")
  (setq howm-dtime-format "<%Y-%m-%d %a %H:%M>")
  ;; Avoid conflicts with Org-mode by changing Howm's prefix from "C-c ,".
  (setq howm-prefix (kbd "C-c ;"))
  :bind*
  ;; Conveniently open the Howm menu with "C-c ; ;".
  ("C-c ; ;" . howm-menu))


(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))


(use-package magit
  :bind (("M-g g" . magit ))
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


