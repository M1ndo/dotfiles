;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "ybenel"
      user-mail-address "ybenel@pm.me")

(setq doom-font (font-spec :family "Fira Code" :size 15)
      doom-variable-pitch-font (font-spec :family "Ubuntu" :size 15)
      doom-big-font (font-spec :family "Fira Code" :size 24))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-moonlight t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
  (doom-themes-org-config))
(setq org-directory "~/org/")
(setq display-line-numbers-type t)
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

(setq fancy-splash-image "~/.doom.d/blackholed.png")


;; (defvar +fl/splashcii-query ""
;;   "The query to search on asciiur.com")

;; (defun +fl/splashcii ()
;;   (split-string (with-output-to-string
;;                   (call-process "splashcii" nil standard-output nil +fl/splashcii-query))
;;                 "\n" t))

;; (defun +fl/doom-banner ()
;;   (let ((point (point)))
;;     (mapc (lambda (line)
;;             (insert (propertize (+doom-dashboard--center +doom-dashboard--width line)
;;                                 'face 'doom-dashboard-banner) " ")
;;             (insert "\n"))
;;           (+fl/splashcii))
;;     (insert (make-string (or (cdr +doom-dashboard-banner-padding) 0) ?\n))))

;; ;; override the first doom dashboard function
;; (setcar (nthcdr 0 +doom-dashboard-functions) #'+fl/doom-banner)

;; (setq +fl/splashcii-query "space")


;;(def-package! highlight-indent-guides
;;  :commands highlight-indent-guides-mode
;;  :hook (prog-mode . highlight-indent-guides-mode)
;;  :confi
;; (setq highlight-indent-guides-method 'character
;;        highlight-indent-guides-character ?\|
;;       highlight-indent-guides-delay 0.01
;;        highlight-indent-guides-responsive 'top
;;        highlight-indent-guides-auto-enabled nil))

;;(require 'mu4e)

;;(setq mail-user-agent 'mu4e-user-agent)
;;
;;(setq mu4e-drafts-folder "/Draft")
;;(setq mu4e-trash-folder  "/Trash")
;;
;;(setq mu4e-sent-messages-behavior 'delete)
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
