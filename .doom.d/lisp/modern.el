;;; ../.dotfiles/.doom.d/lisp/modern.el -*- lexical-binding: t; -*-
(use-package! org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
        org-modern-table-vertical 1
        org-modern-table nil
        org-modern-table-horizontal 0.2
        org-modern-list '((43 . "➤")
                          (45 . "–")
                          (42 . "•"))
        org-modern-todo-faces
        '(("TODO" :inverse-video t :inherit org-todo)
          ("PROJ" :inverse-video t :inherit +org-todo-project :foreground "Aquamarine2")
          ("GYM" :inverse-video t :inherit +org-todo-project :foreground "Brown1")
          ("IDEA" :inverse-video t :inherit +org-todo-onhold :foreground "Orchid2")
          ("STRT" :inverse-video t :inherit +org-todo-active :foreground "OliveDrab1")
          ("DATE" :inverse-video t :inherit +org-todo-active :foreground "PaleGreen")
          ("LOOP"  :inverse-video t :inherit +org-todo-active :foreground "SlateBlue")
          ("NEXT" :inverse-video t :inherit +org-todo-active :foreground "SteelBlue4")
          ("YES"  :inverse-video t :inherit +org-todo-active :foreground "SeaGreen3")
          ("HOLD" :inverse-video t :inherit +org-todo-onhold :foreground "MediumPurple1")
          ("WAIT" :inverse-video t :inherit +org-todo-onhold :foreground "Gold1")
          ("[?]"  :inverse-video t :inherit +org-todo-onhold :foreground "SlateBlue3")
          ("[X]"  :inverse-video t :inherit +org-todo-onhold :foreground "goldenrod1")
          ("OKAY"  :inverse-video t :inherit +org-todo-onhold :foreground "LimeGreen")
          ("[ ]"  :inverse-video t :inherit +org-todo-onhold :foreground "HotPink3")
          ("[-]"  :inverse-video t :inherit +org-todo-cancel :foreground "LightCyan4")
          ("KILL" :inverse-video t :inherit +org-todo-cancel :foreground "Firebrick4")
          ("CANCELLED" :inverse-video t :inherit +org-todo-cancel :foreground "Firebrick2")
          ("NO"   :inverse-video t :inherit +org-todo-cancel :foreground "HotPink1"))
        org-modern-footnote
        (cons nil (cadr org-script-display))
        org-modern-block-fringe nil
        org-modern-block-name
        '((t . t)
          ("src" "»" "«")
          ("example" "»–" "–«")
          ("quote" "❝" "❞")
          ("export" "⏩" "⏪"))
        org-modern-progress nil
        org-modern-priority nil
        org-modern-horizontal-rule (make-string 36 ?─)
        org-modern-keyword
        '((t . t)
          ("title" . "𝙏")
          ("subtitle" . "𝙩")
          ("author" . "𝘼")
          ("email" . #("" 0 1 (display (raise -0.14))))
          ("date" . "𝘿")
          ("property" . "☸")
          ("options" . "⌥")
          ("startup" . "⏻")
          ("macro" . "𝓜")
          ("bind" . #("" 0 1 (display (raise -0.1))))
          ("bibliography" . "")
          ("print_bibliography" . #("" 0 1 (display (raise -0.1))))
          ("cite_export" . "⮭")
          ("print_glossary" . #("ᴬᶻ" 0 1 (display (raise -0.1))))
          ("glossary_sources" . #("" 0 1 (display (raise -0.14))))
          ("include" . "⇤")
          ("setupfile" . "⇚")
          ("html_head" . "🅷")
          ("html" . "🅗")
          ("latex_class" . "🄻")
          ("latex_class_options" . #("🄻" 1 2 (display (raise -0.14))))
          ("latex_header" . "🅻")
          ("latex_header_extra" . "🅻⁺")
          ("latex" . "🅛")
          ("beamer_theme" . "🄱")
          ("beamer_color_theme" . #("🄱" 1 2 (display (raise -0.12))))
          ("beamer_font_theme" . "🄱𝐀")
          ("beamer_header" . "🅱")
          ("beamer" . "🅑")
          ("attr_latex" . "🄛")
          ("attr_html" . "🄗")
          ("attr_org" . "⒪")
          ("call" . #("" 0 1 (display (raise -0.15))))
          ("name" . "⁍")
          ("header" . "›")
          ("caption" . "☰")
          ("results" . "🠶")))
  (custom-set-faces! '(org-modern-statistics :inherit org-checkbox-statistics-todo)))