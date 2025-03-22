;; (load "~/.emacs.d/loadDirectory.el")
;;(load-directory "~/.emacs.d/customFunctions")

(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent) ; Emacs 28 with native compilation
  (setq native-compile-prune-cache t))

(setq inhibit-startup-message t)
  (setq pixel-scroll-precision-large-scroll-height 80.0)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (set-fringe-mode 10)
  (menu-bar-mode -1)
 ;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  (setq visible-bell t)
  (column-number-mode)
  (global-display-line-numbers-mode t)
  (setq gc-cons-threshold (* 2 1000 1000))
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

(fset 'yes-or-no-p 'y-or-n-p)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(defun prot-simple-keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   (( > (minibuffer-depth) 0)
    (abort-recursive-edit))
   ((and (bound-and-true-p meow-mode) (meow-insert-mode-p))
    (meow-insert-exit))
   (t
    (keyboard-quit))))

(use-package no-littering)

(use-package orderless
  :demand t
  :config

  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        ;;; Enable partial-completion for files.
        ;;; Either give orderless precedence or partial-completion.
        ;;; Note that completion-category-overrides is not really an override,
        ;;; but rather prepended to the default completion-styles.
        ;; completion-category-overrides '((file (styles orderless partial-completion))) ;; orderless is tried first
        completion-category-overrides nil)) ;; partial-completion is tried first

(use-package xterm-color
  :commands (xterm-color-filter))
(use-package eshell
  :after xterm-color
  :config
  (setq eshell-scroll-to-bottom-on-input t)
  (define-key eshell-mode-map (kbd "<tab>") #'company-complete)
  (define-key eshell-hist-mode-map (kbd "M-r") #'consult-history)
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setenv "TERM" "xterm-256color")))
  (add-hook 'eshell-before-prompt-hook (setq xterm-color-preserve-properties t))
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions
        (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))

(use-package multiple-cursors
  :bind
  (("C-c m t" . mc/mark-all-like-this)
   ("C-c m m" . mc/mark-all-like-this-dwim)
   ("C-c m l" . mc/edit-lines)
   ("C-c m e" . mc/edit-ends-of-lines)
   ("C-c m a" . mc/edit-beginnings-of-lines)
   ("C-c m n" . mc/mark-next-like-this)
   ("C-c m p" . mc/mark-previous-like-this)
   ("C-c m s" . mc/mark-sgml-tag-pair)
   ("C-c m d" . mc/mark-all-like-this-in-defun)))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
   (setq yas-installed-snippets-dir "~/emacs/yasnippet-snippets")
   (yas-global-mode 1)
)

(with-eval-after-load 'elisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c C-d C-d") 'describe-function)
  (define-key emacs-lisp-mode-map (kbd "C-c C-d d") 'describe-function)
  (define-key emacs-lisp-mode-map (kbd "C-c C-k") 'eval-buffer))

(use-package highlight-quoted
  :ensure t
  :hook
  (emacs-lisp-mode . highlight-quoted-mode))

(use-package eros
  :ensure t
  :hook
  (emacs-lisp-mode . eros-mode))

(use-package suggest
  :ensure t
  :defer t)

(use-package ipretty
  :defer t
  :ensure t
  :config
  (ipretty-mode 1))

;; Hide package namespaces
(use-package nameless
  :ensure t
  :hook
  (emacs-lisp-mode .  nameless-mode)
  :custom
  (nameless-global-aliases '())
  (nameless-private-prefix t))

(use-package erefactor
  :ensure t
  :defer t)

(use-package flycheck-package
  :ensure t
  :after flycheck
  :hook
  (emacs-lisp-mode . flycheck-package-setup))

;; Emacs Lisp Static Analyzer
(use-package elsa
  :defer t
  :ensure t)

(use-package flycheck-elsa
  :ensure t
  :after flycheck
  :hook
  (emacs-lisp-mode . flycheck-elsa-setup))

(use-package lispy :hook (emacs-lisp-mode . lispy-mode))

;; (use-package iedit
;;   :bind
;;   (("<return>" . nil)("C-m"  . iedit-mode) ; also note: C-' toggles focus of matches
;;    :map iedit-mode-keymap
;;    ("C-g" . iedit-mode)) )

(use-package org
  :custom
  (org-image-actual-width '(400)))

(setq org-clock-sound "~/Music/rain_alarm.mp3")
(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("cc" . "src c"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom((doom-modeline-height 15)))

(use-package doom-themes
  :config
  (doom-themes-visual-bell-config)
  :init (load-theme 'doom-moonlight t ))

(use-package nerd-icons)

(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5))

(bind-keys*
 ("ESC ESC" . prot-simple-keyboard-quit-dwim)
  ("C-g" . prot-simple-keyboard-quit-dwim)
 ("C-t . f" . find-file)
 ("C-t . c" . comment-region)
 ("C-t . u" . uncomment-region)
 ("M-j" . avy-goto-char-timer)
 ("C-t j" . avy-goto-char)
 ("C-t w" . avy-goto-word-0)
 ("C-t l" . avy-goto-line)

 ("C-t b c" . clone-indirect-buffer)
 ("C-t b C" . clone-indirect-buffer-other-window)
 ("C-t b k" . kill-this-buffer)
 ("C-t b K" . kill-some-buffers)
 ("C-t b n" . next-buffer)
 ("C-t b p" . previous-buffer)
 ("C-t b r" . revert-buffer)
 ("C-t b R" . rename-buffer)
 ("C-t b s" . basic-save-buffer)
 ("C-t b S" . save-some-buffers)

 ("C-t r" . consult-recent-files)
 ("C-c C-b" . consult-buffer)
 ("C-t B b" . consult-bookmark)
 ("C-t h t" . consult-theme)
 ("C-t s r" . consult-ripgrep)
 ("C-t s g" . consult-grep)
 ("C-t s G" . consult-git-grep)
 ("C-t s f" . consult-find)
 ("C-t s F" . consult-locate)
 ("C-t s y" . consult-yank-from-kill-ring)
 ("C-t i" . consult-imenu)
 )

(use-package paredit)
;; prevent paredit from adding a space before opening paren in certain modes
(defun cs/mode-space-delimiter-p (endp delimiter)

  "Don't insert a space before delimiters in certain modes"
  (or
   (bound-and-true-p tsx-ts-mode)
   (bound-and-true-p typescript-ts-mode)
   (bound-and-true-p web-mode)
   (bound-and-true-p js-ts-mode)
   (bound-and-true-p js-mode)
   (bound-and-true-p javascript-mode)))
(add-to-list 'paredit-space-for-delimiter-predicates #'cs/mode-space-delimiter-p)

;; (use-package projectile
;;   :diminish projectile-mode
;;   :config (projectile-mode)
;;   :custom ((projectile-completion-system 'ivy))
;;   :bind-keymap
;;   ("C-c p" . projectile-command-map)
;;   :init
;;   ;; NOTE: Set this to the folder where you keep your Git repos!
;;   (when (file-directory-p "~/workspaces/")
;;     (setq projectile-project-search-path '("~/workspaces/")))
;;   (setq projectile-switch-project-action #'projectile-dired))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

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

(use-package vertico
  :init (vertico-mode)
  (setq vertico-cycle t) ;; enable cycling for 'vertico-next' and 'vertico-prev'
  :bind (:map vertico-map
	      ("C-j" . vertico-next)
	      ("C-k" . vertico-previous)
	      ("C-f" . vertico-exit)
	      :map minibuffer-local-map
	      ("M-h" . backward-kill-word)))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package consult
  :after orderless
  :bind
  ("C-s" . consult-line ))

(use-package corfu
    :hook ((lsp-completion-mode . kb/corfu-setup-lsp) (prog-mode . corfu-mode)) ; Use corfu for lsp completion
    :bind
    (:map corfu-map
  	("C-j" . corfu-next)
  	("C-k" . corfu-previous)
  	("<escape>" . corfu-quit)
  	("<return>" . corfu-insert)	
  	("C-g" . corfu-quit))
    :custom
    ;; Works with `indent-for-tab-command'. Make sure tab doesn't indent when you
    ;; want to perform completion
    ;; (tab-always-indent 'complete)
    (completion-cycle-threshold nil)      ; Always show candidates in menu

    (corfu-auto t)
    (corfu-auto-prefix 2)
    (corfu-auto-delay 0.25)

    (corfu-min-width 80)
    (corfu-max-width corfu-min-width)     ; Always have the same width
    (corfu-count 14)
    (corfu-scroll-margin 4)
    (corfu-cycle nil)
    (corfu-quit-at-boundary t)   ; Quit popup when typing separator (space)
(corfu-separator ?\s)                ; Use space as separator
(corfu-quit-no-match t)     ; Quit if no match after separator
(corfu-preview-current nil)          ; Disable preview for better VS Code-like experience
(corfu-preselect 'prompt) 
    ;; `nil' means to ignore `corfu-separator' behavior, that is, use the older
    ;; `corfu-quit-at-boundary' = nil behavior. Set this to separator if using
    ;; `corfu-auto' = `t' workflow (in that case, make sure you also set up
    ;; `corfu-separator' and a keybind for `corfu-insert-separator', which my
    ;; configuration already has pre-prepared). Necessary for manual corfu usage with
    ;; orderless, otherwise first component is ignored, unless `corfu-separator'
    ;; is inserted.
    ;; (corfu-quit-at-boundary nil)
    ;; (corfu-separator ?\s)            ; Use space
    ;; (corfu-quit-no-match 'separator) ; Don't quit if there is `corfu-separator' inserted
    ;; (corfu-preview-current 'insert)  ; Preview first candidate. Insert on input if only one
    ;; (corfu-preselect-first t)        ; Preselect first candidate?

    ;; Other
    (lsp-completion-provider :none)       ; Use corfu instead for lsp completions
    :init
    (global-corfu-mode)
    :config
        ;; Setup lsp to use corfu for lsp completion
    (defun kb/corfu-setup-lsp ()
      "Use orderless completion style with lsp-capf instead of the
  default lsp-passthrough."
      (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
            '(orderless))))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil)  ; Use midpoint color between foreground and background colors ("blended")?
  (kind-icon-blend-frac 0.08)

  ;; NOTE 2022-02-05: `kind-icon' depends `svg-lib' which creates a cache
  ;; directory that defaults to the `user-emacs-directory'. Here, I change that
  ;; directory to a location appropriate to `no-littering' conventions, a
  ;; package which moves directories of other packages to sane locations.
  (svg-lib-icons-dir (no-littering-expand-var-file-name "svg-lib/cache/")) ; Change cache dir
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter) ; Enable `kind-icon'

  ;; Add hook to reset cache so the icon colors match my theme
  ;; NOTE 2022-02-05: This is a hook which resets the cache whenever I switch
  ;; the theme using my custom defined command for switching themes. If I don't
  ;; do this, then the backgound color will remain the same, meaning it will not
  ;; match the background color corresponding to the current theme. Important
  ;; since I have a light theme and dark theme I switch between. This has no
  ;; function unless you use something similar
  (add-hook 'kb/themes-hooks #'(lambda () (interactive) (kind-icon-reset-cache))))

(use-package cape
  ;; bindings for dedicated completion commands
  :bind
  (("M-p p" . completion-at-point) ;; capf
   ("M-p t" . complete-tag) ;; etags
   ("M-p d" . cape-dabbrev) ;; dabbrev
   ("M-p h" . cape-history)
   ("M-p f" . cape-file)
   ("M-p k" . cape-keyword)
   ("M-p s" . cape-symbol)
   ("M-p a" . cape-abbrev)
   ("M-p i" . cape-ispell)
   ("M-p l" . cape-line)
   ("M-p w" . cape-dict)
   ("M-p \\" . cape-tex)
   ("M-p &" . cape-sgml)
   ("M-p r" . cape-rfc1345))
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

;; A few more useful configurations...
(use-package emacs
     :init
 
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

(use-package embark
  :bind
  (("C-." . embark-act) ;; easily accessible 'embark-act' binding.
   ("C-;" . embark-dwim)
   :map vertico-map
   ("C-." . embark-act)
   :map embark-heading-map
   ("l"  . org-id-store-link))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package avy
          :ensure t
          :config
          (avy-setup-default))

        (defun avy-action-kill-whole-line (pt)
        (save-excursion
          (goto-char pt)
          (kill-whole-line))
        (select-window
         (cdr
          (ring-ref avy-ring 0)))
        t)

      (setf (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
            (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line)

      (defun avy-action-copy-whole-line (pt)
      (save-excursion
        (goto-char pt)
        (cl-destructuring-bind (start . end)
            (bounds-of-thing-at-point 'line)
          (copy-region-as-kill start end)))
      (select-window
       (cdr
        (ring-ref avy-ring 0)))
      t)

    (defun avy-action-yank-whole-line (pt)
      (avy-action-copy-whole-line pt)
      (save-excursion (yank))
      t)

    (setf (alist-get ?y avy-dispatch-alist) 'avy-action-yank
          (alist-get ?w avy-dispatch-alist) 'avy-action-copy
          (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line
          (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line)

    (defun avy-action-teleport-whole-line (pt)
      (avy-action-kill-whole-line pt)
      (save-excursion (yank)) t)

  (setf (alist-get ?t avy-dispatch-alist) 'avy-action-teleport
        (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line)

  (defun avy-action-embark (pt)
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

(setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark)

(use-package beacon
  :ensure t
  :custom (beacon-color "white")
  :config (beacon-mode t))

;;  (use-package vterm
;;   :config
;;   (setq shell-file-name "/bin/fish"
;; 	vterm-max-scrollback 5000))
;; (use-package eterm-256color
;;   :hook (term-mode . eterm-256color-mode))

;;  (use-package vterm-toggle
;;       :after vterm
;;       :bind
;;       ("M-t" . vterm-toggle)
;;       :config
;;       (setq vterm-toggle-fullscreen-p nil
;; 	    shell-file-name "/bin/fish")
;; (add-to-list 'display-buffer-alist
;;              '((lambda (buffer-or-name _)
;;                    (let ((buffer (get-buffer buffer-or-name)))
;;                      (with-current-buffer buffer
;;                        (or (equal major-mode 'vterm-mode)
;;                            (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
;;                 (display-buffer-reuse-window display-buffer-at-bottom)
;;                 ;;(display-buffer-reuse-window display-buffer-in-direction)
;;                 ;;display-buffer-in-direction/direction/dedicated is added in emacs27
;;                 ;;(direction . bottom)
;;                 ;;(dedicated . t) ;dedicated is supported in emacs27
;;                 (reusable-frames . visible)
;;                 (window-height . 0.3)))
;;     )

;; (setq treesit-language-source-alist
;;   	'((c "https://github.com/tree-sitter/tree-sitter-c")
;;   	(cpp "https://github.com/tree-sitter/tree-sitter-cpp")
;;   	  (bash "https://github.com/tree-sitter/tree-sitter-bash")
;;   	  (cmake "https://github.com/uyha/tree-sitter-cmake")
;;   	  (css "https://github.com/tree-sitter/tree-sitter-css")
;;   	  (elisp "https://github.com/Wilfred/tree-sitter-elisp")
;;   	  (go "https://github.com/tree-sitter/tree-sitter-go")
;;   	  (html "https://github.com/tree-sitter/tree-sitter-html")
;;   	  (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
;;   	  (json "https://github.com/tree-sitter/tree-sitter-json")
;;   	  (make "https://github.com/alemuller/tree-sitter-make")
;;   	  (markdown "https://github.com/ikatyang/tree-sitter-markdown")
;;   	  (python "https://github.com/tree-sitter/tree-sitter-python")
;;   	  (toml "https://github.com/tree-sitter/tree-sitter-toml")
;;   	  (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
;;   	  (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
;;   	  (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
;;   	  (html "https://github.com/tree-sitter/tree-sitter-html.git")
;;   	  (yaml "https://github.com/ikatyang/tree-sitter-yaml")))


;;   (setq major-mode-remap-alist
;;   	'((yaml-mode . yaml-ts-mode)
;;   	  (bash-mode . bash-ts-mode)
;;   	  (js2-mode . js-ts-mode)
;;   	  (typescript-mode . typescript-ts-mode)
;;   	  (json-mode . json-ts-mode)
;;   	  (css-mode . css-ts-mode)
;;             (c++-mode . c++-ts-mode)
;;   	  (c-mode . c-ts-mode)
;;   	  (python-mode . python-ts-mode)))
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; (use-package ivy)

(use-package apheleia
  :ensure t
  :config
  (setf (alist-get 'prettier apheleia-formatters)
        '(npx "prettier"
              "--trailing-comma"  "es5"
              "--bracket-spacing" "true"
              "--single-quote"    "true"
	      "--tabWidth" "2"
              "--semi"            "false"
              "--print-width"     "100"
              file))
  (add-to-list 'apheleia-mode-alist '(typescript-ts-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(json-ts-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(web-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(tsx-ts-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(js-ts-mode . prettier))
  (apheleia-global-mode +1))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package restclient)

(use-package kotlin-mode
  :after (lsp-mode dap-mode)
  ;; :config
  ;; (require 'dap-kotlin)
  ;; ;; should probably have been in dap-kotlin instead of lsp-kotlin
  ;; (setq lsp-kotlin-debug-adapter-path (or (executable-find "kotlin-debug-adapter") ""))
  :hook
  (kotlin-mode . lsp))

(use-package typescript-ts-mode
  :ensure nil
  :hook lsp-deferred
  :mode ("\\.ts\\'" "\\.mts\\'" "\\.cts\\'"))

(defun my/webmode-hook ()
  "Webmode hooks."
  (setq web-mode-enable-comment-annotation t)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-attr-indent-offset 0)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-auto-indentation t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  )
(use-package web-mode
  :ensure t
  :mode ( ("\\.html\\'" . web-mode))
  :config
      (add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
      :commands web-mode
  :hook (web-mode . my/webmode-hook)
  )

(use-package jtsx
  :ensure t
  :mode (("\\.jsx?\\'" . jtsx-jsx-mode)
         ("\\.tsx\\'" . jtsx-tsx-mode)
         ("\\.ts\\'" . jtsx-typescript-mode))
  :commands jtsx-install-treesit-language
  :hook ((jtsx-jsx-mode . hs-minor-mode)
         (jtsx-tsx-mode . hs-minor-mode)
         (jtsx-typescript-mode . hs-minor-mode))
  :custom
  ;; Optional customizations
  ;; (js-indent-level 2)
  ;; (typescript-ts-mode-indent-offset 2)
  ;; (jtsx-switch-indent-offset 0)
  ;; (jtsx-indent-statement-block-regarding-standalone-parent nil)
  ;; (jtsx-jsx-element-move-allow-step-out t)
   (jtsx-enable-jsx-electric-closing-element t)
  ;; (jtsx-enable-electric-open-newline-between-jsx-element-tags t)
  ;; (jtsx-enable-jsx-element-tags-auto-sync nil)
  ;; (jtsx-enable-all-syntax-highlighting-features t)
  :config
  (defun jtsx-bind-keys-to-mode-map (mode-map)
    "Bind keys to MODE-MAP."
    (define-key mode-map (kbd "C-c C-j") 'jtsx-jump-jsx-element-tag-dwim)
    (define-key mode-map (kbd "C-c j o") 'jtsx-jump-jsx-opening-tag)
    (define-key mode-map (kbd "C-c j c") 'jtsx-jump-jsx-closing-tag)
    (define-key mode-map (kbd "C-c j r") 'jtsx-rename-jsx-element)
    (define-key mode-map (kbd "C-c <down>") 'jtsx-move-jsx-element-tag-forward)
    (define-key mode-map (kbd "C-c <up>") 'jtsx-move-jsx-element-tag-backward)
    (define-key mode-map (kbd "C-c C-<down>") 'jtsx-move-jsx-element-forward)
    (define-key mode-map (kbd "C-c C-<up>") 'jtsx-move-jsx-element-backward)
    (define-key mode-map (kbd "C-c C-S-<down>") 'jtsx-move-jsx-element-step-in-forward)
    (define-key mode-map (kbd "C-c C-S-<up>") 'jtsx-move-jsx-element-step-in-backward)
    (define-key mode-map (kbd "C-c j w") 'jtsx-wrap-in-jsx-element)
    (define-key mode-map (kbd "C-c j u") 'jtsx-unwrap-jsx)
    (define-key mode-map (kbd "C-c j d") 'jtsx-delete-jsx-node)
    (define-key mode-map (kbd "C-c j t") 'jtsx-toggle-jsx-attributes-orientation)
    (define-key mode-map (kbd "C-c j h") 'jtsx-rearrange-jsx-attributes-horizontally)
    (define-key mode-map (kbd "C-c j v") 'jtsx-rearrange-jsx-attributes-vertically))
    
  (defun jtsx-bind-keys-to-jtsx-jsx-mode-map ()
      (jtsx-bind-keys-to-mode-map jtsx-jsx-mode-map))

  (defun jtsx-bind-keys-to-jtsx-tsx-mode-map ()
      (jtsx-bind-keys-to-mode-map jtsx-tsx-mode-map))

  (add-hook 'jtsx-jsx-mode-hook 'jtsx-bind-keys-to-jtsx-jsx-mode-map)
  (add-hook 'jtsx-tsx-mode-hook 'jtsx-bind-keys-to-jtsx-tsx-mode-map))

(use-package prettier-js
  :ensure t)
(add-hook 'web-mode-hook #'(lambda ()
                             (enable-minor-mode
                              '("\\.jsx?\\'" . prettier-js-mode))
			     (enable-minor-mode
                              '("\\.tsx?\\'" . prettier-js-mode))))

(use-package flycheck
  :ensure t
  :config
  (flycheck-add-mode 'javascript-eslint 'typescript-mode)
  (flycheck-add-mode 'javascript-eslint 'tsx-ts-mode)
  (flycheck-add-mode 'javascript-eslint 'typescript-ts-mode)
  (flycheck-add-mode 'javascript-eslint 'typescript-tsx-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'jtsx-tsx-mode)
  :init (global-flycheck-mode))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-l")
  (defun my/orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))

  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))
    ;; Optionally configure the first word as flex filtered.
    (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)
    ;; Optionally configure the cape-capf-buster.
    (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point))))
  :hook (((lsp-mode . electric-pair-mode)(lsp-completion-mode . my/lsp-mode-setup-completion)(c-mode          ; clangd
							       c++-mode        ; clangd
							       c-or-c++-mode   ; clangd
							       js-mode         ; ts-ls (tsserver wrapper)
							       js-jsx-mode     ; ts-ls (tsserver wrapper)
							       typescript-mode ; ts-ls (tsserver wrapper)
							       python-mode     ; pyright
							       web-mode        ; ts-ls/HTML/CSS
							       tsx-ts-mode) . lsp-deferred))
  :commands lsp
  :config
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
  (setq lsp-enable-which-key-integration t)
  
  (setq lsp-auto-guess-root t)
  (setq lsp-log-io nil)
  (setq lsp-restart 'auto-restart)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-eldoc-hook nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-semantic-tokens-enable nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-imenu nil)
  (setq lsp-enable-snippet nil)
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  (setq lsp-idle-delay 0.5))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

(use-package dap-mode
  :custom
  (lsp-enable-dap-auto-configure nil)
  :config
  (dap-ui-mode 1)
  )

(use-package c-ts-mode
  :ensure nil
  :mode ("\\.c\\'" "\\.C\\'" "\\.h\\'" "\\.H\\'")
  :config
  (require 'dap-gdb-lldb)
  (dap-gdb-lldb-setup))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

(use-package simple-httpd
  :ensure t)

(use-package magit
  :bind (("M-g g" . magit ))
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package flycheck-clangcheck
  :ensure t)


(defun foo ()
  (flycheck-set-checker-executable 'c/c++-clangcheck "/usr/bin/clang-check")
  (flycheck-select-checker 'c/c++-clangcheck))

(add-hook 'c-mode-hook #'foo)
(add-hook 'c++-mode-hook #'foo)
;; enable static analysis
(setq flycheck-clangcheck-analyze t)

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

(use-package ace-window
  :ensure t
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind("C-r" . ace-window))

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape" . ignore))
(meow-define-keys
   'insert
  '("C-g" . meow-insert-exit))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))

  	    (use-package meow
          :ensure t
          :custom
          (meow-use-cursor-position-hack t)
          (meow-use-clipboard t)
          (meow-goto-line-function 'consult-goto-line)
          :config
         (meow-global-mode 1)
         (meow-setup))

(use-package meow-tree-sitter
  :custom
  (meow-tree-sitter-register-defaults))
