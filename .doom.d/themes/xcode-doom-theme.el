;;; xcode-doom-theme.el --- based off of Apple's Xcode Dark Theme -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: February 28, 2020 (#414)
;; Updated: December 8, 2023
;; Author: kadenbarlow <https://github.com/kadenbarlow>
;; Author: ybenel <https://github.com/m1ndo/dotfiles>
;; Maintainer: ybenel
;; Source: Xcode.app
;;
;;; Commentary:
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup xcode-doom-theme nil
  "Options for the `xcode-doom' theme."
  :group 'doom-themes)

(defcustom xcode-doom-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'xcode-doom-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme xcode-doom
  "A theme based off of the Xcode Dark Theme"

  ;; name        gui       256       16
  ((bg         '("#1B2B34" nil       nil            ))
   (bg-alt     '("#14232D" nil       nil            ))
   (base0      '("#1B2B34" "black"   "black"        ))
   (base1      '("#343D46" "#1e1e1e" "brightblack"  ))
   (base2      '("#4F5B66" "#2e2e2e" "brightblack"  ))
   (base3      '("#65737E" "#262626" "brightblack"  ))
   (base4      '("#A7ADBA" "#3f3f3f" "brightblack"  ))
   (base5      '("#C0C5CE" "#525252" "brightblack"  ))
   (base6      '("#CDD3DE" "#6b6b6b" "brightblack"  ))
   (base7      '("#D8DEE9" "#979797" "white"        ))
   (base8      base7)
   (fg-alt     base6)
   (fg         base8)

   (red        '("#FC6A5D" "#FC6A5D" "red"))
   (orange     '("#FD8F3F" "#FD8F3F" "orange"))
   (yellow     '("#D0BF68" "#D0BF68" "yellow"))
   (green      '("#67B7A4" "#67B7A4" "green"))
   (blue       '("#5DD8FF" "#5DD8FF" "brightblue"))
   (teal       '("#59B0CF" "#59B0CF" "brightblue"))
   (magenta    '("#D0A8FF" "#D0A8FF" "magenta"))
   (cyan       '("#8abeb7" "#8abeb7" "cyan"))
   (dark-cyan  (doom-darken cyan 0.4))


   (grey       '("#6C7986" "#6C7986" "brightblack"))
   (light-green'("#9EF1DD" "#9EF1DD" "lightgreen"))
   (violet     '("#A167E6" "#A167E6" "brightmagenta"))
   (dark-blue  '("#41A1C0" "#41A1C0" "darkblue"))
   (pink       '("#FC5FA3" "#FC5FA3" "pink"))

   ;; face categories
   (highlight      blue)
   (vertical-bar   `("#161616" ,@base0))
   (selection      `(,(car (doom-lighten bg 0.1)) ,@(cdr base4)))
   (builtin        light-green)
   (comments       grey)
   (doc-comments   (doom-lighten grey 0.14))
   (constants      violet)
   (functions      magenta)
   (keywords       pink)
   (methods        dark-blue)
   (operators      orange)
   (type           blue)
   (strings        red)
   (variables      dark-blue)
   (numbers        yellow)
   (region         selection)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    fg-alt)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (modeline-bg     `(,(doom-darken (car bg-alt) 0.3) ,@(cdr base3)))
   (modeline-bg-alt `(,(car bg) ,@(cdr base1)))
   (modeline-fg     base8)
   (modeline-fg-alt comments)
   (-modeline-pad
    (when xcode-doom-padded-modeline
      (if (integerp xcode-doom-padded-modeline)
          xcode-doom-padded-modeline
        4))))

  ;; --- faces ------------------------------
  (((font-lock-keyword-face &override) :weight 'bold)
   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground orange :weight 'bold)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-alt :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))

   ;;;; VCS/magit readability
   (diff-refine-removed :foreground vc-deleted :background bg :inverse-video t)
   (diff-refine-added :foreground vc-added :background bg :inverse-video t)

   (magit-diff-removed-highlight :foreground vc-deleted :background base1 :weight 'bold)

   (magit-diff-base :foreground vc-modified :background bg-alt)
   (magit-diff-removed :foreground vc-deleted :background base1)
   (magit-diff-added :foreground vc-added :background base1)

   ;;;; org <built-in>
   ((org-block &override) :background bg-alt)
   ;; ((org-block-begin-line &override) :background bg-alt)
   ;; ((org-block-end-line &override) :background bg-alt)
   ;; (org-hide :foreground hidden)
   ;;;; doom-modeline
   (doom-modeline-buffer-path       :foreground dark-blue :bold bold)
   (doom-modeline-buffer-major-mode :inherit 'doom-modeline-buffer-path)
   ;;;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground yellow)
   (rainbow-delimiters-depth-2-face :foreground blue)
   (rainbow-delimiters-depth-3-face :foreground orange)
   (rainbow-delimiters-depth-4-face :foreground green)
   (rainbow-delimiters-depth-5-face :foreground magenta)
   (rainbow-delimiters-depth-6-face :foreground violet)
   (rainbow-delimiters-depth-7-face :foreground teal))

  ;; --- variables --------------------------
  ;; ()
  )

;;; xcode-doom-theme.el ends here
