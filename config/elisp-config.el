(provide 'elisp-config)


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

