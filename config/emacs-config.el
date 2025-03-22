(provide 'emacs-config)

(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent) ; Emacs 28 with native compilation
  (setq native-compile-prune-cache t))

(setq pixel-scroll-precision-large-scroll-height 80.0)
(set-fringe-mode 10)
(setq visible-bell t)
(column-number-mode)
(global-display-line-numbers-mode t)
(setq make-backup-files nil)
(setq create-lockfiles nil)
(set-face-attribute 'default nil :font "FiraCode Nerd Font" :height 110)
(add-to-list 'default-frame-alist '(alpha-background . 90))
;; auto-save-mode doesn't create the path automatically!
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))
(global-set-key [?\C-\=] 'text-scale-increase)
(global-set-key [?\C-\-] 'text-scale-decrease)

;; (fset 'yes-or-no-p 'y-or-n-p)


(when (string= (getenv "XDG_SESSION_TYPE") "wayland")
  (setq wl-copy-process nil)

  (defun wl-copy (text)
    (setq wl-copy-process (make-process :name "wl-copy"
                                        :buffer nil
                                        :command '("wl-copy" "-f" "-n")
                                        :connection-type 'pipe))
    (process-send-string wl-copy-process text)
    (process-send-eof wl-copy-process))

  (defun wl-paste ()
    (if (and wl-copy-process (process-live-p wl-copy-process))
        nil ; should return nil if we're the current paste owner
      (shell-command-to-string "wl-paste -n | tr -d \r")))

  (setq interprogram-cut-function 'wl-copy)
  (setq interprogram-paste-function 'wl-paste))
