(setq-default TeX-engine 'xetex
              TeX-PDF-mode t)

(use-package! engrave-faces-latex
  :after ox-latex)

(with-eval-after-load 'ox-latex
  ;; (add-to-list 'org-latex-packages-alist '("" "minted")) ;; I have it added into latex.setup
  (setq org-latex-listings 'engraved
        org-latex-compiler "XeLaTeX"
        org-latex-hyperref-template "\\hypersetup{\n pdfauthor={%a},\n pdftitle={%t},\n pdfkeywords={%k},\n pdfsubject={%d},\n pdfcreator={%c}, \n pdflang={%L}, \n colorlinks=true, \n linktoc=page, \n linkcolor=blue, \n urlcolor=bblue}\n")
  ;; Fix Encoding And Setting Default Packages
  (setq org-latex-default-packages-alist
        '(("AUTO" "inputenc" t)
          ("T1" "fontenc" t)
          ("" "graphicx" t)
          ("" "longtable" t)
          ("" "wrapfig" nil)
          ("" "rotating" nil)
          ("normalem" "ulem" t)
          ("" "amsmath" t)
          ("" "amssymb" t)
          ("" "amsthm" t)
          ("ngerman" "babel" t)
          ("" "capt-of" nil)
          ("" "hyperref" nil)))
  ;; Change Height between Subtitle And Title
  (setq org-latex-subtitle-format "\\\\\\smallskip\n\\large %s")

  ;; custom Example Block
  (defadvice! org-latex-example-block (example-block _contents info)
    "Transcode an EXAMPLE-BLOCK element from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual
information."
    (when (org-string-nw-p (org-element-property :value example-block))
      (let ((environment (or (org-export-read-attribute
                              :attr_latex example-block :environment)
                             "tcolorbox"))
            (title (or (org-export-read-attribute :attr_latex example-block :title)
                       "Example")))
        (org-latex--wrap-label
         example-block
         (format "\\begin{%s}[colframe=cyan!10!black,fonttitle=\\myfont\\bfseries,fontlower=\\itshape,title=%s]\n%s\\end{%s}"
                 environment
                 title
                 (org-export-format-code-default example-block info)
                 environment)
         info))))

  ;; Set Fancy Checkboxes.
  (defun +org-export-latex-fancy-item-checkboxes (text backend info)
    (when (org-export-derived-backend-p backend 'latex)
      (replace-regexp-in-string
       "\\\\item\\[{$\\\\\\(\\w+\\)$}\\]"
       (lambda (fullmatch)
         (concat "\\\\item[" (pcase (substring fullmatch 9 -3) ; content of capture group
                               ("square"   "\\\\checkboxUnchecked")
                               ("boxminus" "\\\\checkboxTransitive")
                               ("boxtimes" "\\\\checkboxChecked")
                               (_ (substring fullmatch 9 -3))) "]"))
       text)))

  (add-to-list 'org-export-filter-item-functions
               '+org-export-latex-fancy-item-checkboxes)

  ;; Custom Latex Blocks
  (defadvice! org-latex-special-block (special-block contents info)
    "Transcode a SPECIAL-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
    (let ((type (org-element-property :type special-block))
	  (opt (org-export-read-attribute :attr_latex special-block :options))
          (title (or (org-export-read-attribute :attr_latex special-block :title) "Special"))
          (ycolor (or (org-export-read-attribute :attr_latex special-block :ycolor) "green!50!black"))
          (ytfont (or (org-export-read-attribute :attr_latex special-block :ytfont) "\\algreya\\bfseries"))
	  (caption (org-latex--caption/label-string special-block info))
	  (caption-above-p (org-latex--caption-above-p special-block info)))
      (pcase type
        ("ybox"
         (concat
          (format "\\begin{tcolorbox}[colframe=%s,fonttitle=%s,fontlower=\\itshape,title=%s]\n" ycolor ytfont title)
          (and caption-above-p caption)
          contents
          (and (not caption-above-p) caption)
          (format "\\end{%s}" "tcolorbox"))
         )
        (_
         (concat
          (format "\\begin{%s}%s\n" type (or opt ""))
          (and caption-above-p caption)
          contents
          (and (not caption-above-p) caption)
          (format "\\end{%s}" type))
         ))
      ))

  (setq +latex-viewers '(zathura evince))
  (setq org-latex-pdf-process '("latexmk -f -pdf -lualatex -shell-escape -interaction=nonstopmode %f"))
  (setq! org-latex-engraved-preamble
         "\\usepackage{fvextra}

[FVEXTRA-SETUP]

% Make line numbers smaller and grey.
\\renewcommand\\theFancyVerbLine{\\footnotesize\\color{black!40!white}\\arabic{FancyVerbLine}}

% In case engrave-faces-latex-gen-preamble has not been run.
\\providecolor{EfD}{HTML}{f7f7f7}
\\providecolor{EFD}{HTML}{28292e}

% Define a Code environment to prettily wrap the fontified code.
\\DeclareTColorBox[]{Code}{o}%
{colback=EfD!98!EFD, colframe=EfD!95!EFD,
  fontupper=\\footnotesize\\setlength{\\fboxsep}{0pt},
  colupper=EFD,
  IfNoValueTF={#1}%
  {boxsep=2pt, arc=2.5pt, outer arc=2.5pt,
    boxrule=0.5pt, left=2pt}%
  {boxsep=2.5pt, arc=0pt, outer arc=0pt,
    boxrule=0pt, leftrule=1.5pt, left=0.5pt},
  right=2pt, top=1pt, bottom=0.5pt,
  breakable}

[LISTINGS-SETUP]")
  (add-to-list 'org-latex-classes
               '("book"
                 "\\documentclass{book}
    [NO-DEFAULT-PACKAGES]
    [PACKAGES]
    [EXTRA]"
                 ("\\part{%s}" . "\\part*{%s}")
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
  (add-to-list 'org-latex-classes
               '("koma-article"
                 "\\documentclass{scrartcl}
    [NO-DEFAULT-PACKAGES]
    [PACKAGES]
    [EXTRA]"
                 ("\\part{%s}" . "\\part*{%s}")
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
  (add-to-list 'org-latex-classes
               '("koma-book"
                 "\\documentclass{scrbook}
    [NO-DEFAULT-PACKAGES]
    [PACKAGES]
    [EXTRA]"
                 ("\\part{%s}" . "\\part*{%s}")
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
  (add-to-list 'org-latex-classes
               '("memoir"
                 "\\documentclass{memoir}
    [NO-DEFAULT-PACKAGES]
    [PACKAGES]
    [EXTRA]"
                 ("\\part{%s}" . "\\part*{%s}")
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
  (add-to-list 'org-latex-classes
               '("org-plain-latex"
                 "\\documentclass{article}
    [NO-DEFAULT-PACKAGES]
    [PACKAGES]
    [EXTRA]"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(require 'ob-latex)
(with-eval-after-load 'ob-latex
  (defvar org-latex-caption-preamble "
\\usepackage{subcaption}
\\usepackage[hypcap=true]{caption}
% \\setkomafont{caption}{\\sffamily\\small} % Used in Koma Class
% \\setkomafont{captionlabel}{\\upshape\\bfseries} % Used In Koma Class
\\captionsetup{justification=raggedright,singlelinecheck=true}
"
    "Preamble that improves captions.")

  (defvar org-latex-checkbox-preamble "
\\newcommand{\\checkboxUnchecked}{$\\square$}
\\newcommand{\\checkboxTransitive}{\\rlap{\\raisebox{-0.1ex}{\\hspace{0.35ex}\\Large\\textbf -}}$\\square$}
\\newcommand{\\checkboxChecked}{\\rlap{\\raisebox{0.2ex}{\\hspace{0.35ex}\\scriptsize \\ding{52}}}$\\square$}
"
    "Preamble that improves checkboxes.")

  (defvar org-latex-box-preamble "
% chat Environemnt
\\NewCoffin\\Content
\\NewCoffin\\SideRule

\\ExplSyntaxOn
\\NewDocumentEnvironment{chat}{ }
  {
    \\vcoffin_set:Nnw \\Content { \\linewidth }
      \\noindent \\ignorespaces
  }
  {
    \\vcoffin_set_end:
    \\SetHorizontalCoffin\\SideRule{\\color{blue!70!green!70!black}\\rule{1pt}{\\CoffinTotalHeight\\Content}}

    \\JoinCoffins*\\Content[l,t]\\SideRule[l,t](-0.5em,0pt)
    \\noindent\\TypesetCoffin\\Content
    \\vspace*{\\CoffinTotalHeight\\Content}\\bigskip
  }
\\ExplSyntaxOff
"
    "Preamble that provides a macro for chats/quotes ..")

  (defvar org-latex-cmd-preamble "
\\DeclareTotalTCBox{\\commandbox}{ s v }
{verbatim,colupper=white,colback=black!75!white,colframe=black}
{\\IfBooleanT{#1}{\\textcolor{red}{\\bfseries » }}%
\\lstinline[language=command.com,keywordstyle=\\color{yellow!35!white}\\bfseries]^#2^}
"
    "Mark \"Something\" As Command")

  (defvar org-latex-cmdsh-preamble "
\\tcbuselibrary{listings}
\\newtcblisting{commandshell}{colback=blue!20!black,colupper=white,colframe=yellow!75!black,%
listing only,listing options={style=tcblatex,language=bash},%
every listing line={\\textcolor{red}{\\small\\ttfamily\\bfseries \\textit{cmd} \$> }}}
"
    "Mark Each Line In \\begin{commandshell} as a command")

  (defvar org-latex-qq-preamble "
\\usepackage[
    left = \\flqq{},%
    right = \\frqq{},%
    leftsub = \\flq{},%
    rightsub = \\frq{} %
]{dirtytalk}
"
    " Preamble that provides a macro for quotes \say{} ")

  (defvar org-latex-keyword-box "
\\newtcbox{\\myk}{enhanced,nobeforeafter,tcbox raise base, %
boxrule=0.2pt,top=0mm,bottom=0mm,colframe=blue, colback=white!90, %
frame style={opacity=0.12}, interior style={opacity=0.20},shrink tight, extrude by=1mm}
\\directlua{dofile(\"/home/alienx/org/Templates/replMe.lua\")}
% \\AtBeginDocument{%
% \\directlua{luatexbase.add_to_callback (\"process_input_buffer\", replMe.replaceMe, \"replMe.replaceMe\")}}
"
    "Preamble To Supply TODO Keywords With Beautifed Box")

  (defvar org-latex-italic-quotes t
    "Make \"quote\" environments italic.")
  (defvar org-latex-par-sep nil
    "Vertically seperate paragraphs, and remove indentation.")

  ;; Disabled Because We are using Iosevka Which has has enough unicode chars to cover *Updated
  ;; Now all emoji's and unicode support is currently working using LuaLaTeX And Fall Back Fonts :)
  ;; (defvar +org-pdflatex-inputenc-encoded-chars
  ;;   "[[:ascii:]\u00A0-\u01F0\u0218-\u021BȲȳȷˆˇ˜˘˙˛˝\u0400-\u04FFḂḃẞ\u200B\u200C\u2010-\u201E†‡•…‰‱‹›※‽⁄⁎⁒₡₤₦₩₫€₱℃№℗℞℠™Ω℧℮←↑→↓〈〉␢␣◦◯♪⟨⟩Ḡḡ\uFB00-\uFB06\u2500-\u259F]")

  ;; (defun +org-latex-replace-non-ascii-chars (text backend info)
  ;;   "Replace non-ascii chars with \\char\"XYZ forms."
  ;;   (when (and (org-export-derived-backend-p backend 'latex)
  ;;              (string= (plist-get info :latex-compiler) "xelatex"))
  ;;     (let (case-replace)
  ;;       (replace-regexp-in-string "[^[:ascii:]]"
  ;;                                 (lambda (nonascii)
  ;;                                   (if (string-match-p +org-pdflatex-inputenc-encoded-chars nonascii) nonascii
  ;;                                     (or (cdr (assoc nonascii +org-latex-non-ascii-char-substitutions)) "¿")))
  ;;                                 text))))
  ;; (add-to-list 'org-export-filter-plain-text-functions #'+org-latex-replace-non-ascii-chars t)


  (defvar org-latex-conditional-features
    '(("\\[\\[\\(?:file\\|https?\\):\\(?:[^]]\\|\\\\\\]\\)+?\\.\\(?:eps\\|pdf\\|png\\|jpeg\\|jpg\\|jbig2\\)\\]\\]" . image)
      ("\\[\\[\\(?:file\\|https?\\):\\(?:[^]]+?\\|\\\\\\]\\)\\.svg\\]\\]\\|\\\\includesvg" . svg)
      ("^[ \t]*|" . table)
      ("cref:\\|\\cref{\\|\\[\\[[^\\]]+\\]\\]" . cleveref)
      ("[;\\\\]?\\b[A-Z][A-Z]+s?[^A-Za-z]" . acronym)
      ("\\+[^ ].*[^ ]\\+\\|_[^ ].*[^ ]_\\|\\\\uu?line\\|\\\\uwave\\|\\\\sout\\|\\\\xout\\|\\\\dashuline\\|\\dotuline\\|\\markoverwith" . underline)
      (":float wrap" . float-wrap)
      (":float sideways" . rotate)
      ("^[ \t]*#\\+caption:\\|\\\\caption" . caption)
      ("\\[\\[xkcd:" . (image caption))
      ("\\\\commandbox" . cmd-link)
      ("^[ \t]*#\\+KEYS_ENABLE" . keyword-box)
      ("^[ \t]*#\\+TOCT" . toc-box)
      ((and org-latex-italic-quotes "^[ \t]*#\\+begin_quote\\|\\\\begin{quote}") . italic-quotes)
      (org-latex-par-sep . par-sep)
      ("^[ \t]*#\\+begin_chat\\|\\\\begin{chat}" . box-chat)
      ("^[ \t]*#\\+begin_commandshell\\|\\\\begin{commandshell}" . cmd-shell)
      ("^[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\|[A-Za-z]+[.)]\\) \\[[ -X]\\]" . checkbox))

    "Org feature tests and associated LaTeX feature flags.

Alist where the car is a test for the presense of the feature,
and the cdr is either a single feature symbol or list of feature symbols.

When a string, it is used as a regex search in the buffer.
The feature is registered as present when there is a match.

The car can also be a
- symbol, the value of which is fetched
- function, which is called with info as an argument
- list, which is `eval'uated

If the symbol, function, or list produces a string: that is used as a regex
search in the buffer. Otherwise any non-nil return value will indicate the
existance of the feature.")

  (defvar org-latex-feature-implementations
    '((image         :snippet "\\usepackage{graphicx}" :order 2)
      (svg           :snippet "\\usepackage{svg}" :order 2)
      (table         :snippet "\\usepackage{longtable}\n\\usepackage{booktabs}" :order 2)
      (cleveref      :snippet "\\usepackage[capitalize]{cleveref}" :order 1)
      (underline     :snippet "\\usepackage[normalem]{ulem}" :order 0.5)
      (float-wrap    :snippet "\\usepackage{wrapfig}" :order 2)
      (rotate        :snippet "\\usepackage{rotating}" :order 2)
      (toc-box       :snippet "\\usepackage{pgfplots}\n\\pgfplotsset{compat=1.17}" :order 2)
      (caption       :snippet org-latex-caption-preamble :order 2.1) ; % This is for Koma Article class (Using Article!!!)
      (acronym       :snippet "\\newcommand{\\acr}[1]{\\protect\\textls*[110]{\\scshape #1}}\n\\newcommand{\\acrs}{\\protect\\scalebox{.91}[.84]{\\hspace{0.15ex}s}}" :order 0.4)
      (disp-quote    :snippet org-latex-qq-preamble :order 0.5)
      (italic-quotes :requires disp-quote :snippet "\\renewcommand{\\quote}{\\list{}{\\rightmargin\\leftmargin}\\item\\relax\\qquot\\em}\n" :order 0.5)
      (par-sep       :snippet "\\setlength{\\parskip}{\\baselineskip}\n\\setlength{\\parindent}{0pt}\n" :order 0.5)
      (.pifont       :snippet "\\usepackage{pifont}")
      (.xcoffins     :snippet "\\usepackage{xcoffins}")
      (cmd-link      :snippet org-latex-cmd-preamble :order 2.1)
      (cmd-shell     :snippet org-latex-cmdsh-preamble :order 2.1)
      (checkbox      :requires .pifont :snippet org-latex-checkbox-preamble  :order 3)
      (.fancy-box    :requires (.pifont .xcoffins) :snippet org-latex-box-preamble :order 3.9)
      (keyword-box   :snippet org-latex-keyword-box :order 3.9)
      (box-chat      :requires .fancy-box :order 4))

    "LaTeX features and details required to implement them.

List where the car is the feature symbol, and the rest forms a plist with the
following keys:
- :snippet, which may be either
  - a string which should be included in the preamble
  - a symbol, the value of which is included in the preamble
  - a function, which is evaluated with the list of feature flags as its
    single argument. The result of which is included in the preamble
  - a list, which is passed to `eval', with a list of feature flags available
    as \"features\"

- :requires, a feature or list of features that must be available
- :when, a feature or list of features that when all available should cause this
    to be automatically enabled.
- :prevents, a feature or list of features that should be masked
- :order, for when ordering is important. Lower values appear first.
    The default is 0.

Features that start with ! will be eagerly loaded, i.e. without being detected.")
  (defun org-latex-detect-features (&optional buffer info)
    "List features from `org-latex-conditional-features' detected in BUFFER."
    (let ((case-fold-search nil))
      (with-current-buffer (or buffer (current-buffer))
        (delete-dups
         (mapcan (lambda (construct-feature)
                   (when (let ((out (pcase (car construct-feature)
                                      ((pred stringp) (car construct-feature))
                                      ((pred functionp) (funcall (car construct-feature) info))
                                      ((pred listp) (eval (car construct-feature)))
                                      ((pred symbolp) (symbol-value (car construct-feature)))
                                      (_ (user-error "org-latex-conditional-features key %s unable to be used" (car construct-feature))))))
                           (if (stringp out)
                               (save-excursion
                                 (goto-char (point-min))
                                 (re-search-forward out nil t))
                             out))
                     (if (listp (cdr construct-feature)) (cdr construct-feature) (list (cdr construct-feature)))))
                 org-latex-conditional-features)))))
  (defun org-latex-expand-features (features)
    "For each feature in FEATURES process :requires, :when, and :prevents keywords and sort according to :order."
    (dolist (feature features)
      (unless (assoc feature org-latex-feature-implementations)
        (error "Feature %s not provided in org-latex-feature-implementations" feature)))
    (setq current features)
    (while current
      (when-let ((requirements (plist-get (cdr (assq (car current) org-latex-feature-implementations)) :requires)))
        (setcdr current (if (listp requirements)
                            (append requirements (cdr current))
                          (cons requirements (cdr current)))))
      (setq current (cdr current)))
    (dolist (potential-feature
             (append features (delq nil (mapcar (lambda (feat)
                                                  (when (plist-get (cdr feat) :eager)
                                                    (car feat)))
                                                org-latex-feature-implementations))))
      (when-let ((prerequisites (plist-get (cdr (assoc potential-feature org-latex-feature-implementations)) :when)))
        (setf features (if (if (listp prerequisites)
                               (cl-every (lambda (preq) (memq preq features)) prerequisites)
                             (memq prerequisites features))
                           (append (list potential-feature) features)
                         (delq potential-feature features)))))
    (dolist (feature features)
      (when-let ((prevents (plist-get (cdr (assoc feature org-latex-feature-implementations)) :prevents)))
        (setf features (cl-set-difference features (if (listp prevents) prevents (list prevents))))))
    (sort (delete-dups features)
          (lambda (feat1 feat2)
            (if (< (or (plist-get (cdr (assoc feat1 org-latex-feature-implementations)) :order) 1)
                   (or (plist-get (cdr (assoc feat2 org-latex-feature-implementations)) :order) 1))
                t nil))))
  (defun org-latex-generate-features-preamble (features)
    "Generate the LaTeX preamble content required to provide FEATURES.
This is done according to `org-latex-feature-implementations'"
    (let ((expanded-features (org-latex-expand-features features)))
      (concat
       (format "\n%% features: %s\n" expanded-features)
       (mapconcat (lambda (feature)
                    (when-let ((snippet (plist-get (cdr (assoc feature org-latex-feature-implementations)) :snippet)))
                      (concat
                       (pcase snippet
                         ((pred stringp) snippet)
                         ((pred functionp) (funcall snippet features))
                         ((pred listp) (eval `(let ((features ',features)) (,@snippet))))
                         ((pred symbolp) (symbol-value snippet))
                         (_ (user-error "org-latex-feature-implementations :snippet value %s unable to be used" snippet)))
                       "\n")))
                  expanded-features
                  "")
       "% end features\n")))

  (defvar info--tmp nil)

  (defadvice! org-latex-save-info (info &optional t_ s_)
    :before #'org-latex-make-preamble
    (setq info--tmp info))

  (defadvice! org-splice-latex-header-and-generated-preamble-a (orig-fn tpl def-pkg pkg snippets-p &optional extra)
    "Dynamically insert preamble content based on `org-latex-conditional-preambles'."
    :around #'org-splice-latex-header
    (let ((header (funcall orig-fn tpl def-pkg pkg snippets-p extra)))
      (if snippets-p header
        (concat header
                (org-latex-generate-features-preamble (org-latex-detect-features nil info--tmp))
                "\n")))))
