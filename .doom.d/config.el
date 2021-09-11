;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "ybenel"
      user-mail-address "ybenel@pm.me")

(setq doom-font (font-spec :family "Fira Code" :size 15)
      doom-variable-pitch-font (font-spec :family "Ubuntu" :size 15)
      doom-big-font (font-spec :family "Fira Code" :size 24))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-moonlight t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))
;;(setq doom-theme 'doom-ephemeral)

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
;; (setq fancy-splash-image "~/.doom.d/blackhole.png")

;; Call splashcii to get the banne and output it .

(defvar +fl/splashcii-query ""
  "The query to search on asciiur.com")

(defun +fl/splashcii-banner ()
  (mapc (lambda (line)
          (insert (propertize (+doom-dashboard--center +doom-dashboard--width line)
                              'face 'doom-dashboard-banner) " ")
          (insert "\n"))
        (split-string (with-output-to-string
                        (call-process "splashcii" nil standard-output nil +fl/splashcii-query))
                      "\n" t)))

(setq +doom-dashboard-ascii-banner-fn #'+fl/splashcii-banner)

(setq +fl/splashcii-query "space")

;; Setup Org-Roam template
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/org/Boxes")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
   ("b" "Box Notes" plain
      (file "~/org/Templates/Boxes.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point))
  :config
  (org-roam-setup))

;; Setting the indent guides to show a pipe character.
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character
       highlight-indent-guides-delay 0
       highlight-indent-guides-responsive 'stack
       highlight-indent-guides-auto-enabled nil)

; Autoload Lua-mode;;Rainbow-Mode
(load-file "~/.emacs.d/.local/elpa/lua-mode-20210809.1320/lua-mode.el")
(load-file "~/.emacs.d/.local/elpa/rainbow-mode-1.0.5/rainbow-mode.el")

; Neotree
(use-package "neotree"
  :bind(("C-c C-f" . neotree-toggle))
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;; Email Setup

;;(require 'mu4e)

;; use mu4e for e-mail in emacs
;;(setq mail-user-agent 'mu4e-user-agent)
;;
;;(setq mu4e-drafts-folder "/Draft")
;;(setq mu4e-trash-folder  "/Trash")
;;
;;;; don't save message to Sent Messages, Gmail/IMAP takes care of this
;;(setq mu4e-sent-messages-behavior 'delete)
;;;; setup some handy shortcuts
;;;; you can quickly switch to your Inbox -- press ``ji''
;;;; then, when you want archive some messages, move them to
;;;; the 'All Mail' folder by pressing ``ma''.
;;
;;(setq mu4e-maildir-shortcuts
;;    '( (:maildir "/INBOX"       :key ?i)
;;       (:maildir "/Sent"        :key ?s)
;;       (:maildir "/Trash"       :key ?t)
;;       (:maildir "/Draft"       :key ?d)))
;;
;;(setq mu4e-get-mail-command "offlineimap"
;;      mu4e-update-interval  300)
;;(setq
;;   user-mail-address "USERMAIL@gmail.com"
;;   user-full-name  "Younes Ben El"
;;   mu4e-compose-signature)
;;(require 'smtpmail)
;;(setq message-send-mail-function 'smtpmail-send-it
;;   starttls-use-gnutls t
;;   smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;;   smtpmail-auth-credentials
;;     '(("smtp.gmail.com" 587 "USERMAIL@gmail.com" nil))
;;   smtpmail-default-smtp-server "smtp.gmail.com"
;;   smtpmail-smtp-server "smtp.gmail.com"
;;   smtpmail-smtp-service 587)
;;(setq message-kill-buffer-on-exit t)
