;;; completion/corfu/config.el -*- lexical-binding: t; -*-

(defvar +corfu-global-capes
  '(:completion)
  "A list of global capes to be available at all times.
The key :completion is used to specify where completion candidates should be
placed, otherwise they come first.")

(defvar +corfu-capf-hosts
  '(lsp-completion-at-point
    eglot-completion-at-point
    elisp-completion-at-point
    tags-completion-at-point-function)
  "A prioritised list of host capfs to create a super cape onto from
  `+corfu-global-capes'.")

(defvar +corfu-auto-delay 0.1
  "How long after point stands still will completion be called automatically,
in seconds.

Setting `corfu-auto-delay' directly may not work, as it needs to be set *before*
enabling `corfu-mode'.")

(defvar +corfu-ispell-completion-modes '(org-mode markdown-mode text-mode)
  "Modes to enable ispell completion in.

For completion in comments, see `+corfu-ispell-in-comments-and-strings'.")

(defvar +corfu-ispell-in-comments-and-strings t
  "Enable completion with ispell inside comments when in a `prog-mode'
derivative.")

(defvar +nerd-icons-font "VictorMono Nerd Font"
  "Font used for nerd-icon-corfu

  Default font used `VictorMon Nerd Font'")

(use-package! corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay +corfu-auto-delay)
  (corfu-on-exact-match nil)
  (corfu-cycle t)
  (corfu-auto-prefix 2)
  (completion-cycle-threshold 1)
  (tab-always-indent 'complete)
  (corfu-min-width 50)
  :hook
  (doom-first-buffer . global-corfu-mode)
  :config
  (when (modulep! +minibuffer)
    (add-hook! 'minibuffer-setup-hook
      (defun corfu-move-to-minibuffer ()
        "Move current completions to the minibuffer"
        (interactive)
        (let ((completion-extra-properties corfu--extra)
              completion-cycle-threshold completion-cycling)
          (apply #'consult-completion-in-region completion-in-region--data)))))

  ;; Dirty hack to get c completion running
  ;; Discussion in https://github.com/minad/corfu/issues/34
  (when (and (modulep! :lang cc)
             (equal tab-always-indent 'complete))
    (map! :map c-mode-base-map
          :i [remap c-indent-line-or-region] #'completion-at-point))

  ;; Reset lsp-completion provider
  (after! lsp-mode
    (setq lsp-completion-provider :none))

  (add-hook! '(lsp-mode-hook eglot-mode-hook after-change-major-mode-hook)
    (defun +corfu--load-capes ()
      "Load all capes specified in `+corfu-global-capes'."
      (interactive)
      (when-let ((host (cl-intersection +corfu-capf-hosts completion-at-point-functions)))
        (setq-local
         completion-at-point-functions
         (cl-substitute
          (apply #'cape-super-capf (cl-substitute (car host) :completion (cl-pushnew :completion +corfu-global-capes)))
          (car host)
          completion-at-point-functions)))))

  (map! :map corfu-map
        "C-SPC"    #'corfu-insert-separator
        "C-n"      #'corfu-next
        "C-p"      #'corfu-previous
        "M-m"      #'corfu-move-to-minibuffer
        (:prefix "C-x"
                 "C-k"     #'cape-dict
                 "C-f"     #'cape-file))

  (after! evil
    (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
    (advice-add 'corfu--teardown :after 'evil-normalize-keymaps)
    (evil-make-overriding-map corfu-map))

  (defadvice! +corfu--org-return (orig) :around '+org/return
    (if (and (modulep! :completion corfu)
             corfu-mode
             (>= corfu--index 0))
        (corfu-insert)
      (funcall orig)))

  (unless (display-graphic-p)
    (corfu-doc-terminal-mode)
    (corfu-terminal-mode)))


(use-package! orderless
  :when (modulep! +orderless)
  :init
  (setq completion-styles '(orderless partial-completion)
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package! nerd-icons-corfu
  :after corfu
  :init
  (after! nerd-icons
    (setq nerd-icons-font-family +nerd-icons-font))
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package! cape
  :defer t
  :init
  (map!
   [remap dabbrev-expand] 'cape-dabbrev)

  (add-hook! 'latex-mode-hook
    (defun +corfu--latex-set-capfs ()
      (make-local-variable '+corfu-global-capes)
      (add-to-list '+corfu-global-capes #'cape-tex)))
  ;; Enable completion in Comments And Strings
  (when +corfu-ispell-in-comments-and-strings
    (defalias 'corfu--ispell-in-comments-and-strings
      (cape-super-capf (cape-capf-inside-comment #'cape-dict)
                       (cape-capf-inside-string #'cape-dict)))
    (add-hook 'prog-mode-hook
              (lambda ()
                (add-to-list 'completion-at-point-functions #'corfu--ispell-in-comments-and-strings))))
  ;; Add Cape-dict into select modes
  (dolist (sym +corfu-ispell-completion-modes)
    (add-hook (intern (concat (symbol-name sym) "-hook"))
              (lambda ()
                (add-to-list 'completion-at-point-functions #'cape-dict))))
  (add-hook! '(TeX-mode-hook LaTeX-mode-hook org-mode-hook)
    (lambda ()
      (add-to-list 'completion-at-point-functions #'cape-tex t))
    :depth 2)

  (add-to-list '+corfu-global-capes #'cape-file)
  (add-to-list '+corfu-global-capes #'cape-keyword t)
  (add-to-list '+corfu-global-capes #'cape-dabbrev t))
  ;; :config
  ;; ;; Enhances speed on large projects, for which many buffers may be open.
  ;; (setq cape-dabbrev-check-other-buffers nil))

(use-package! corfu-history
  :after corfu
  :init
  (after! savehist
    (add-to-list 'savehist-additional-variables 'corfu-history))
  :hook (corfu-mode . corfu-history-mode))


(use-package! corfu-quick
  :after corfu
  :bind (:map corfu-map
              ("C-q" . corfu-quick-insert)))


(use-package! corfu-echo
  :after corfu
  :hook (corfu-mode . corfu-echo-mode))


(use-package! corfu-info
  :after corfu)


(use-package! corfu-popupinfo
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :config
  (setq corfu-popupinfo-delay '(0.5 . 1.0))
  (map! (:map 'corfu-map
         :desc "scroll info up" "C-<up>" #'corfu-popupinfo-scroll-down
         :desc "scroll info down" "C-<down>" #'corfu-popupinfo-scroll-up
         :desc "scroll info up" "C-S-p" #'corfu-popupinfo-scroll-down
         :desc "scroll info down" "C-S-n" #'corfu-popupinfo-scroll-up
         :desc "toggle info" "C-h" #'corfu-popupinfo-toggle)
        (:map 'corfu-popupinfo-map
         :when (modulep! :editor evil)
         ;; Reversed because popupinfo assumes opposite of what feels intuitive
         ;; with evil.
         :desc "scroll info up" "C-S-k" #'corfu-popupinfo-scroll-down
         :desc "scroll info down" "C-S-j" #'corfu-popupinfo-scroll-up)))


(use-package! yasnippet-capf
  :after corfu
  :init
  (add-to-list '+corfu-global-capes #'yasnippet-capf))


(use-package! package-capf
  :after corfu
  :init
  (add-hook! 'emacs-lisp-mode-hook
    (defun +corfu--emacs-lisp-set-capfs ()
      (make-local-variable '+corfu-global-capes)
      (add-to-list '+corfu-global-capes #'package-capf)
      (+corfu--load-capes))))


(use-package! evil-collection-corfu
  :when (modulep! :editor evil +everywhere)
  :defer t
  :init (setq evil-collection-corfu-key-themes '(default magic-return))
  :config
  (evil-collection-corfu-setup))
