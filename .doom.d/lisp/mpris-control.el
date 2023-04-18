;;; mpris-control.el --- Control MPRIS compatible media players -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "25.3") (dash))

;;; Commentary:

;; Control MPRIS compatible media players.

;; TODO:
;; - Fetch metadata on activation, not only when song changes
;; - Select mediaplayer
;; - Control commands

;;; Code:

(require 'dbus)
(require 'dash)

(defvar mpris-control-metadata nil
  "Media player metadata.")

(defvar mpris-control-properties-changed-registration nil)

(defvar mpris-control-metadata-hook '(mpris-control-update-mode-line)
  "Hook that will be called when song metadata changes.")

(defun mpris-control-format-artist-and-track (metadata)
  (-when-let ((&alist "xesam:artist" (((artist)))
                      "xesam:title" ((title))
                      "xesam:album" ((album)))
              metadata)
    (concat
     (propertize "🦖 " 'face '(:foreground "red"))
     (propertize (format "%s: %s " artist title)
                 'help-echo
                 'face '(:Inherit mode-line-emphasis)
                 (format "artist: %s; album: %s; track: %s" artist album title )))))

(defvar mpris-control-mode-line-formatter
  #'mpris-control-format-artist-and-track
  "Function to use for formatting media player status for mode line use.")

(defvar mpris-control-mode-line-info "")

;; TODO: only store status and call hook
(defun mpris-control-properties-changed-callback (a received b)
  (-when-let ((&alist "Metadata" ((metadata))) received)
    (setq
     mpris-control-metadata metadata)
    (run-hooks 'mpris-control-metadata-hook)))

(defun mpris-control-update-mode-line ()
  (setq mpris-control-mode-line-info
        (funcall mpris-control-mode-line-formatter mpris-control-metadata))
  (force-mode-line-update t))

;;;###autoload
(define-minor-mode mpris-control-info-mode
  "mpris info mode"
  :global t
  (if mpris-control-info-mode
      (progn
        (setq mpris-control-properties-changed-registration
              (dbus-register-signal
               :session nil
               "/org/mpris/MediaPlayer2"
               "org.freedesktop.DBus.Properties"
               "PropertiesChanged"
               #'mpris-control-properties-changed-callback)))
    (dbus-unregister-object mpris-control-properties-changed-registration)))

;; This is separate so users can disable it and use the metadata change
;; notifications for other purposes.
(defun mpris-control-setup-mode-line ()
  (if mpris-control-info-mode
      (progn
        (unless global-mode-string
          (setq global-mode-string '("")))
        (unless (memq 'mpris-control-mode-line-info global-mode-string)
          (setq global-mode-string
                (append global-mode-string '(mpris-control-mode-line-info)))))
    (progn
      (setq mpris-control-mode-line-info "")
      (force-mode-line-update t))))

(add-hook 'mpris-control-info-mode-hook #'mpris-control-setup-mode-line)

(provide 'mpris-control)

;;; mpris-control.el ends here
