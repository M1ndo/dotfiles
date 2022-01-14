(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                            (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
(add-hook 'org-mode-hook #'+org-pretty-mode)

;; (custom-set-faces!
;;   '(outline-1 :weight regular :height 1.2)
;;   '(outline-2 :weight bold :height 1.1)
;;   '(outline-3 :weight bold :height 0.9)
;;   '(outline-4 :weight semibold :height 0.9)
;;   '(outline-5 :weight semibold :height 0.7)
;;   '(outline-6 :weight semibold)
;;   '(outline-8 :weight semibold)
;;   '(outline-9 :weight semibold))
;; (custom-set-faces!
;;   '(outline-1 :weight regular :height 1.2)
;;   '(outline-2 :weight regular :height 1.1)
;;   '(outline-3 :weight regular :height 0.9)
;;   '(outline-4 :weight regular :height 0.9)
;;   '(outline-5 :weight regular :height 0.7)
;;   '(outline-6 :weight regular)
;;   '(outline-8 :weight regular)
;;   '(outline-9 :weight regular))
;; Org Document Title Make It Bigger
(custom-set-faces!
  '(org-document-title :height 1.7))
;; Using Error Face In Deadlines
(setq org-agenda-deadline-faces
      '((1.001 . error)
        (1.0 . org-warning)
        (0.5 . org-upcoming-deadline)
        (0.0 . org-upcoming-distant-deadline)))
;; Custom Faces For Org Elements
(custom-theme-set-faces
   'user
   '(org-link ((t (:foreground "SpringGreen2" :underline t))))
   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598")))))
;; Adding Italic To Quote BLocks
(setq org-fontify-quote-and-verse-blocks t)
;; Editing Large Files Can Be Fucked up For that reason adding little delay in fontification.
(defun locally-defer-font-lock ()
  "Set jit-lock defer and stealth, when buffer is over a certain size."
  (when (> (buffer-size) 50000)
    (setq-local jit-lock-defer-time 0.05
                jit-lock-stealth-time 1)))

(add-hook 'org-mode-hook #'locally-defer-font-lock)
