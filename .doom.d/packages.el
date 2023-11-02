;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el
(when (package! screenshot :recipe (:local-repo "~/.doom.d/lisp/screenshot"))
  (package! posframe))
(package! org-appear :recipe (:local-repo "~/.doom.d/lisp/org-appear"))
(package! org-pretty-table :recipe (:local-repo "~/.doom.d/lisp/org-pretty-table"))
;; (package! clippy :recipe (:local-repo "~/.doom.d/lisp/clippy"))
(package! engrave-faces :recipe (:local-repo "~/.doom.d/lisp/engrave-faces"))
(package! softresize :recipe (:host github :repo "jdburgosr/softresize"))
(package! smudge :recipe (:local-repo "~/.doom.d/lisp/smudge"))
(package! gofmt-tag)
(package! org-super-agenda)
(package! hammy)
(package! sublimity)
(package! solaire-mode)
;; (package! selectric-mode)
(package! rainbow-mode)
;; (package! org-superstar)
(package! org-modern :pin "7d037569bc4a05f40262ea110c4cda05c69b5c52")
(package! org-roam)
(package! org-msg)
(package! org-mime)
(package! nyan-mode)
(package! neotree :disable t)
(package! mpdmacs)
(package! lua-mode)
(package! highlight-indent-guides)
(package! gif-screencast :disable t)
(package! elcord)
(package! dracula-theme :disable t)
(package! consult-dir)
(package! company-emojify :disable t)
(package! aggressive-indent)
(package! 0x0)
;; (package! centaur-tabs :pin "b4249c40581368faf7bb8e06f86b9eee199c3c6" :disable t)
(package! vlf :recipe (:host github :repo "m00natic/vlfi" :files ("*.el"))
  :pin "cc02f2533782d6b9b628cec7e2dcf25b2d05a27c" :disable t)
(package! dashboard :disable t)
;; (package! org-pretty-tags :pin "5c7521651b35ae9a7d3add4a66ae8cc176ae1c76")
(package! org-pretty-tags :disable t)
(package! org-tag-beautify :disable t)
(package! marginalia)
(package! theme-magic :disable t)
(package! poetry :pin "5b9ef569d629d79820e73b5380e54e443ba90616" :disable t)
(package! org-recent-headings)
(package! auto-complete :disable t)
(package! mw-thesaurus)
(package! beacon)
(package! org-sidebar)
(package! chronos)
(package! column-enforce-mode)
(package! centered-cursor-mode :disable t)
(package! transwin)
(package! bufler)
(package! rainbow-delimiters)
(package! olivetti)
(package! freeze-it)
(package! cyberpunk-theme :disable t)
(package! zoom-window)
(package! goggles :disable t)
(package! eldoc-box)
;;(package! vulpea)
(package! hl-anything)
(package! gptel :recipe (:host github :repo "karthink/gptel"))
(package! autothemer :disable t)
(package! meow :disable t)
(package! pkgbuild-mode)
(package! sudo-edit)
(package! org :pin "806abc5a2bbcb5f884467a0145547221ba09eb59")
