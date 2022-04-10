(TeX-add-style-hook
 "novel"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("memoir" "smalldemyvopaper" "11pt" "twoside" "onecolumn" "openright" "extrafontsizes")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8x") ("fontenc" "T1") ("Alegreya" "osf") ("AlegreyaSans" "osf") ("hyperref" "hidelinks")))
   (TeX-run-style-hooks
    "latex2e"
    "memoir"
    "memoir11"
    "inputenc"
    "fontenc"
    "Alegreya"
    "AlegreyaSans"
    "microtype"
    "setspace"
    "lettrine"
    "titlesec"
    "lipsum"
    "calc"
    "hologo"
    "hyperref")
   (TeX-add-symbols
    "ISBN"
    "press"
    "Title"
    "halftitlepage"
    "titleM")
   (LaTeX-add-lengths
    "drop"))
 :latex)

