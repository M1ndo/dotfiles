;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "ybenel"
      user-mail-address "ybenel@pm.me")

(setq doom-font (font-spec :family "Mononoki Nerd Font" :size 15)
      doom-variable-pitch-font (font-spec :family "Ubuntu" :size 15)
      doom-big-font (font-spec :family "Mononoki Nerd Font" :size 24))
;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these setq:
;; (doom two-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-dark+ t)

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

;; Setting the indent guides to show a pipe character.
;;(def-package! highlight-indent-guides
;;  :commands highlight-indent-guides-mode
;;  :hook (prog-mode . highlight-indent-guides-mode)
;;  :confi
;; (setq highlight-indent-guides-method 'character
;;        highlight-indent-guides-character ?\|
;;       highlight-indent-guides-delay 0.01
;;        highlight-indent-guides-responsive 'top
;;        highlight-indent-guides-auto-enabled nil))

(require 'mu4e)

;; use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent)

(setq mu4e-drafts-folder "/Draft")
(setq mu4e-trash-folder  "/Trash")

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)
;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.

(setq mu4e-maildir-shortcuts
    '( (:maildir "/INBOX"       :key ?i)
       (:maildir "/Sent"        :key ?s)
       (:maildir "/Trash"       :key ?t)
       (:maildir "/Draft"       :key ?d)))

(setq mu4e-get-mail-command "offlineimap"
      mu4e-update-interval  300)
(setq
   user-mail-address "USERMAIL@gmail.com"
   user-full-name  "Younes Ben El"
   mu4e-compose-signature)
(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
   starttls-use-gnutls t
   smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
   smtpmail-auth-credentials
     '(("smtp.gmail.com" 587 "USERMAIL@gmail.com" nil))
   smtpmail-default-smtp-server "smtp.gmail.com"
   smtpmail-smtp-server "smtp.gmail.com"
   smtpmail-smtp-service 587)
(setq message-kill-buffer-on-exit t)
