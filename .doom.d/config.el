;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Load Personal File
(let* ((personal-file (concat doom-private-dir "personal.el.gpg"))
       (personal-file-bc (concat personal-file ".elc")))
  (unless (file-exists-p personal-file-bc)
    (epa-file-enable)
    (byte-compile-file personal-file)))
(load-library "personal.el.gpg")

(setq doom-font (font-spec :family "Caskaydia Cove" :size 15)
      doom-variable-pitch-font (font-spec :family "Caskaydia Cove" :size 15)
      doom-big-font (font-spec :family "Caskaydia Cove" :size 24))

(use-package! doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-dracula t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))
;;(setq doom-theme 'doom-ephemeral)
;;
;; Solaire-Mode To Add Dark Background unreal buffers
(solaire-global-mode +1)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Sets transparency for focuses and unfocused frames.
(set-frame-parameter (selected-frame) 'alpha '(95 . 80))
(add-to-list 'default-frame-alist '(alpha . (95 . 80)))

(map! :leader
      (:prefix ("c". "code")
       :desc "Comment Line(s)" "[" #'comment-region
       :desc "Uncomment Line(s)" "]" #'uncomment-region))

(map! :leader
      (:prefix ("b". "buffer")
       :desc "List bookmarks" "L" #'list-bookmarks
       :desc "Save current bookmarks to bookmark file" "w" #'bookmark-save))


;; Set Image to be the banner
(setq fancy-splash-image "~/.doom.d/img/emacs.png")

;; Call splashcii to get the banne and output it .

;; (defvar +fl/splashcii-query ""
;;   "The query to search on asciiur.com")

;; (defun +fl/splashcii-banner ()
;;   (mapc (lambda (line)
;;           (insert (propertize (+doom-dashboard--center +doom-dashboard--width line)
;;                               'face 'doom-dashboard-banner) " ")
;;           (insert "\n"))
;;         (split-string (with-output-to-string
;;                         (call-process "splashcii" nil standard-output nil +fl/splashcii-query))
;;                       "\n" t)))

;; (setq +doom-dashboard-ascii-banner-fn #'+fl/splashcii-banner)

;; (setq +fl/splashcii-query "space")

;; Load Org-Roam Config
(load-file "~/.doom.d/roam.el")

;; Enable Aggressive Indent
(use-package! aggressive-indent
  :init
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

;; Setting the indent guides to show a pipe character.
(use-package! highlight-indent-guides
  :init
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-delay 0
        highlight-indent-guides-responsive 'stack
        highlight-indent-guides-auto-enabled nil))

; Neotree
(use-package! "neotree"
  :bind(("C-c C-f" . neotree-toggle))
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;; Code Highlight
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-latex-listings 'minted)
  (setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (add-to-list 'org-latex-classes
               '("org-plain-latex"
                 "\\documentclass{article}
                 [NO-DEFAULT-PACKAGES]
                 [PACKAGES]
                 [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;; Set Org-Superstar
(require 'org-superstar)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

;; Email Setup
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(with-eval-after-load 'mu4e
  (setq mu4e-context-policy 'ask-if-none
        mu4e-compose-context-policy 'pick-first)
  (setq mu4e-change-filenames-when-moving t)
  ;; refresh mail using isync every 10 minutes
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-mu-binary "~/Downloads/mu-1.6.6/mu/mu")
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-root-maildir "~/Mail")
  (bind-key "C-c C-m" 'mu4e)
  (setq mu4e-html2text-command "w3m -dump -T text/html -o display_link_number=true")
  ;; (setq mu4e-html2text-command "html2markdown | grep -v '&nbsp_place_holder;'")

  (set-email-account! "Personal"
                      '((mu4e-sent-folder          . "/Gmail/[Gmail]/Sent Mail")
                        (mu4e-drafts-folder        . "/Gmail/[Gmail]/Drafts")
                        (mu4e-refile-folder        . "/Gmail/[Gmail]/All Mail")
                        (mu4e-trash-folder         . "/Gmail/[Gmail]/Trash")
                        (user-mail-address         . my_email)
                        (user-full-name            . my_name)
                        (mu4e-compose-signature    . my_signature)
                        (smtpmail-smtp-user        . my_email)
                        (smtpmail-smtp-server      . "smtp.gmail.com")
                        (smtpmail-stream-type      . ssl)
                        (smtpmail-smtp-service     . 465))
                      t)

  (setq mu4e-maildir-shortcuts
        '(("/Gmail/inbox"             . ?i)
          ("/Gmail/[Gmail]/Sent Mail" . ?s)
          ("/Gmail/[Gmail]/Trash"     . ?t)
          ("/Gmail/[Gmail]/Drafts"    . ?d)
          ("/Gmail/[Gmail]/All Mail"  . ?a))))


(use-package! mu4e-alert
  :after mu4e
  :config
  (setq doom-modeline-mu4e t)

  (mu4e-alert-enable-mode-line-display)
  (mu4e-alert-enable-notifications)

  (mu4e-alert-set-default-style 'libnotify)
  (map! :leader
        (:prefix-map ("d". "Doom")
         :desc "Disable Mu4e Modeline Alert" "d" #'mu4e-alert-disable-mode-line-display
         :desc "Enable Mu4e Modeline Alert" "i" #'mu4e-alert-enable-mode-line-display))

  (defvar +mu4e-alert-bell-cmd '("paplay" . "/usr/share/sounds/freedesktop/stereo/message.oga")
    "Cons list with command to play a sound, and the sound file to play.
                 Disabled when set to nil.")

  (setq mu4e-alert-email-notification-types '(subjects))
  (defun +mu4e-alert-grouped-mail-notification-formatter-with-bell (mail-group _all-mails)
    "Default function to format MAIL-GROUP for notification.
                 ALL-MAILS are the all the unread emails"
    (when +mu4e-alert-bell-cmd
      (start-process (car +mu4e-alert-bell-cmd) (cdr +mu4e-alert-bell-cmd)))
    (if (> (length mail-group) 1)
        (let* ((mail-count (length mail-group))
               (first-mail (car mail-group))
               (title-prefix (format "You have %d unread emails"
                                     mail-count))
               (field-value (mu4e-alert--get-group first-mail))
               (title-suffix (format (pcase mu4e-alert-group-by
                                       (`:from "from %s:")
                                       (`:to "to %s:")
                                       (`:maildir "in %s:")
                                       (`:priority "with %s priority:")
                                       (`:flags "with %s flags:"))
                                     field-value))
               (title (format "%s %s" title-prefix title-suffix)))
          (list :title title
                :body (s-join "\n"
                              (mapcar (lambda (mail)
                                        (format "%s<b>%s</b> • %s"
                                                (cond
                                                 ((plist-get mail :in-reply-to) "⮩ ")
                                                 ((string-match-p "\\`Fwd:"
                                                                  (plist-get mail :subject)) " ⮯ ")
                                                 (t "  "))
                                                (truncate-string-to-width (caar (plist-get mail :from))
                                                                          20 nil nil t)
                                                (truncate-string-to-width
                                                 (replace-regexp-in-string "\\`Re: \\|\\`Fwd: " ""
                                                                           (plist-get mail :subject))
                                                 40 nil nil t)))
                                      mail-group))))
      (let* ((new-mail (car mail-group))
             (subject (plist-get new-mail :subject))
             (sender (caar (plist-get new-mail :from))))
        (list :title sender :body subject))))
  (setq mu4e-alert-grouped-mail-notification-formatter #'+mu4e-alert-grouped-mail-notification-formatter-with-bell))

;;   Setup Org-Mime For mu4e
(use-package! org-mime
  :ensure t
  :config
  (setq org-mime-export-options '(:section-numbers nil
                                  :with-author nil
                                  :with-toc nil))
  (add-hook 'org-mime-html-hook
            (lambda ()
              (org-mime-change-element-style
               "pre" (format "color: %s; background-color: %s; padding: 0.5em;"
                             "#E6E1DC" "#232323"))))
  (add-hook 'message-send-hook 'org-mime-htmlize))

;; Discord Rich
(require 'elcord)
(elcord-mode)

;; Hide Doom-modeline
(map! :leader
      (:prefix-map ("d" . "Doom")
       :desc "Hide Doom Modeline" "b" #'hide-mode-line-mode))


;; Add Lua,Nya,And Add Hook Rainbow-Mode,0x0
(require 'lua-mode)

(require 'nyan-mode)

(require 'mpdmacs)
(mpdmacs-mode)

(require '0x0)
(map! :leader
      (:prefix-map ("d" . "Doom")
       (:prefix ("x" . "0x0")
        :desc "Yank Buffer/Region To 0x0" "1" #'0x0-upload-text
        :desc "Upload File To 0x0" "0" #'0x0-upload-file)))

(use-package! rainbow-mode
  :init
  (add-hook 'prog-mode-hook 'rainbow-mode))

;; Remove History Duplicates
(setq history-delete-duplicates t)

;; Add Flyspell Checker
(require 'flyspell)
(map! :leader
      (:prefix-map ("d" . "Doom")
       (:prefix ("s". "spell")
        :desc "Run Spell Check On Region" "r" #'flyspell-region
        :desc "Correct Next Word" "n" #'flyspell-correct-next
        :desc "Correct Previous Word" "p" #'flyspell-correct-previous)))

;; Set Screenshot
(load-file "~/.doom.d/lisp/screenshot/screenshot.el")
(use-package! screenshot
  :defer t
  :config (setq screenshot-upload-fn "~/.local/bin/0x0 %s 2>/dev/null"))

;; Set Screencast
;; (use-package! gif-screencast
;;   :commands gif-screencast-mode
;;   :config
;;   (map! :map gif-screencast-mode-map
;;         :g "<f8>" #'gif-screencast-toggle-pause
;;         :g "<f9>" #'gif-screencast-stop)
;;   (setq gif-screencast-program "maim"
;;         gif-screencast-args `("--quality" "3" "-i" ,(string-trim-right
;;                                                      (shell-command-to-string
;;                                                       "xdotool getactivewindow")))
;;         gif-screencast-optimize-args '("--batch" "--optimize=3" "--usecolormap=/tmp/doom-color-theme"))
;;   (defun gif-screencast-write-colormap ()
;;     (f-write-text
;;      (replace-regexp-in-string
;;       "\n+" "\n"
;;       (mapconcat (lambda (c) (if (listp (cdr c))
;;                                  (cadr c))) doom-themes--colors "\n"))
;;      'utf-8
;;      "/tmp/doom-color-theme" ))
;;   (gif-screencast-write-colormap)
;;   (add-hook 'doom-load-theme-hook #'gif-screencast-write-colormap))

;; Enable Org Pretty Table
(progn
  (add-to-list 'load-path "~/.doom.d/lisp/org-pretty-table")
  (require 'org-pretty-table)
  (add-hook 'org-mode-hook (lambda () (org-pretty-table-mode))))

;; Enable Org Appear
(progn
  (add-to-list 'load-path "~/.doom.d/lisp/org-appear")
  (require 'org-appear)
  (add-hook 'org-mode-hook (lambda () (org-appear-mode))))

;; Enable emojy
(require 'emojify)
;; Enable sublimity For smooth scrolling and minimap
(add-to-list 'load-path "~/.emacs.d/.local/elpa/sublimity-20200905.1730/")
(require 'sublimity)
;; ;; Smooth  Scroll (Not Really Prefer The Builtin Feel feel to try it tho)
;; (use-package sublimity-scroll
;;   :config
;;   (setq sublimity-scroll-weight 5
;;         sublimity-scroll-vertical-frame-delay 0.01
;;         sublimity-scroll-drift-length 15))

;; Minimap
(use-package! sublimity-map
  :config
  (sublimity-map-set-delay 3)
  (setq sublimity-map-size 20)
  (setq sublimity-map-fraction 0.9)
  (setq sublimity-map-text-scale -9))

;; Set Keybinding for vterm
(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "<C-left>") 'vterm-send-M-b)
  (define-key vterm-mode-map (kbd "<C-right>") 'vterm-send-M-e)
  (define-key vterm-mode-map (kbd "<C-backspace>")
    '(lambda () (interactive) (vterm-send-key (kbd "C-w")))))


;; Map Window
(global-set-key (kbd "S-<left>") 'evil-window-left)
(global-set-key (kbd "S-<right>") 'evil-window-right)
(global-set-key (kbd "S-<up>") 'evil-window-up)
(global-set-key (kbd "S-<down>") 'evil-window-down)

;; Consult-dir
(use-package! consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))


;; Org-Super-agenda
(use-package! org-super-agenda
  :after org-agenda
  :init
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-custom-commands
        '(("t" "Today view"
           ((agenda "" ((org-agenda-overriding-header "")
                        (org-agenda-span 'day)
                        (org-agenda-start-day nil)
                        ;; always show timelines!
                        (org-agenda-time-grid '((daily today) (800 1000 1200 1400 1600 1800 2000) "" "----------------"))
                        (org-agenda-prefix-format '((agenda . " %i %?-12t%-6e% s")))
                        (org-super-agenda-groups
                         '((:name "Scheduled Today"
                            :time-grid t
                            :date today
                            :order 1)
                           (:name "Habits"
                            :habit t
                            :date today
                            :order 2)
                           (:name "Overdue"
                            :deadline past
                            :order 3)
                           (:name "Ongoing"
                            :scheduled past
                            :order 4
                            )
                           (:discard (:anything t)))
                         )
                        )
                    )
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-agenda-prefix-format '((agenda . " %i %?-12t%-6e% s")
                                                     (todo . " %i %-6e")
                                                     (tags . " %i %-12:c")
                                                     (search . " %i")))
                         (org-super-agenda-groups
                          '((:discard (:scheduled today))
                            (:name "Low Effort (<= 15 min)"
                             :and (:effort< "0:16")
                             :order 1)
                            (:name "Next Tasks"
                             :todo "NEXT"
                             :order 2)
                            (:discard (:anything t))))))))
          ("w" "Week view"
           ((agenda "" ((org-agenda-overriding-header "Week view")
                        (org-agenda-span 'week)
                        (org-agenda-start-on-weekday 1)
                        (org-agenda-time-grid '(nil (800 1000 1200 1400 1600 1800 2000) "" "----------------"))
                        (org-agenda-prefix-format '((agenda . " %i %?-12t%-6e% s")))
                        )
                    )
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '((:name "Overdue (past scheduled/deadline)"
                             :deadline past
                             :scheduled past
                             :order 1
                             )
                            (:name "Individual Tasks"
                             :file-path "task"
                             :order 2
                             )
                            (:name "Next tasks"
                             :todo "NEXT"
                             :order 3)
                            (:discard (:anything t))
                            )
                          )
                         )
                     )
            )
           )
          ("p" . "Planning")
          ("pm" "Month view"
           (
            (tags-todo "+Goal" ((org-agenda-overriding-header "Goals")
                                )
                       )
            (agenda "" ((org-agenda-span 'month)
                        (org-agenda-start-day "01")
                        (org-super-agenda-groups
                         '((:discard (:todo "GOAL"))
                           (:discard (:todo "RECUR"))
                           (:scheduled t))
                         )
                        )
                    )
            (todo "" ((org-agenda-overriding-header "Things to schedule")
                      (org-super-agenda-groups
                       '((:name "Individual tasks"
                          :file-path "task"
                          )
                         (:name "Next tasks"
                          :todo "NEXT"
                          )
                         (:discard (:anything t)))
                       )
                      )
                  )
            ))
          ))
  :config
  (org-super-agenda-mode))


;; Highlighting
;; (package! ov-highlight :recipe (:local-repo "~/.doom.d/lisp/ov-highlight/ov-highlight.el"))
;; (load-file "~/.doom.d/lisp/ov-highlight/ov-highlight.el")
;; (require 'ov-highlight)
;; (map! :leader
;;       (:prefix-map ("d" . "Doom")
;;        (:prefix ("h" . "Highlight")
;;         :desc "Highlight Foreground Red" "r" #'ov-highlight-red-fg
;;         :desc "Highlight With Pink" "g" #'ov-highlight-green
;;         :desc "Highlight With Pink" "p" #'ov-highlight-pink
;;         :desc "Highlight Clear" "c" #'ov-highlight-clear)))

;; Load scimax-bookmark
(org-babel-load-file "~/.doom.d/lisp/scimax-editmarks.org")

