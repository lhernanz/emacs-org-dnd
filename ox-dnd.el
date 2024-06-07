;;; ox-dnd.el --- Write D&D homebrew sheets using org-mode

;; Copyright (C) 2019 Alex Smith <xeals@pm.me>

;; Author: Alex Smith <xeals@pm.me>
;; URL: https://github.com/xeals/emacs-org-dnd

(require 'ox-latex)

(defgroup org-dnd nil
  "Group for customising the org-mode D&D LaTeX export backend.")

(defcustom org-dnd-use-package nil
  "Use the `book' LaTeX class when exporting a D&D org-mode file
  instead of the `dndbook' class."
  :group 'org-dnd)

(defcustom org-dnd-latex-preamble
  "\\documentclass[10pt,twoside,twocolumn,openany[CO],notitlepage,nodeprecatedcode]{%s}
[NO-DEFAULT-PACKAGES]
\\usepackage[AUTO]{babel}
\\usepackage[utf8]{inputenc}
\\usepackage[hidelinks]{hyperref}
\\usepackage{stfloats}
\\usetikzlibrary{intersections}
\\usepackage[singlelinecheck=false]{caption}
\\captionsetup[table]{labelformat=empty,font={sf,sc,bf,},skip=0pt}
\\newcolumntype{H}{>{\\setbox0=\\hbox\\bgroup}c<{\\egroup}@{}}"
  "The common LaTeX preamble that will be added to all dnd classes.
Notice that this is a template that will be used with the format
function where it will get the name of the class"
  :group 'org-dnd
  :type 'string)

(defcustom org-dndbook-latex-preamble
  "\\usepackage{titling}
\\pretitle{\\begin{center}\\DndFontTitle}
\\posttitle{\\par\\end{center}}
\\preauthor{\\begin{center}\\DndFontSubtitle}
\\postauthor{\\end{center}}
"
  "The LaTeX preamble that will be added to the dndbook class.
Notice that this will be inserted after the common part of the template."
  :group 'org-dnd
  :type 'string)

(defcustom org-dnd-latex-toc-command "{\\let\\clearpage\\relax \\tableofcontents}\n\n"
  "LaTeX command to set the table of contents, list of figures, etc.
This command only applies to the table of contents generated with
the toc:nil option, not to those generated with #+TOC keyword."
  :group 'org-dnd
  :type 'string)

(unless (assoc "dndbook" org-latex-classes)
  (add-to-list
   'org-latex-classes
   `("dndbook"
     ,(concat
       (format
        org-dnd-latex-preamble
        (if org-dnd-use-package "book" "dndbook"))
       org-dndbook-latex-preamble
       (when org-dnd-use-package "\\n\\usepackage{dnd}"))
     ("\\part{%s}" . "\\part*{%s}")
     ("\\chapter{%s}" . "\\chapter*{%s}")
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
     )))

(unless (assoc "dndbookbrief" org-latex-classes)
  (add-to-list
   'org-latex-classes
   `("dndbook"
     ,(concat
       (format
        org-dnd-latex-preamble
        (if org-dnd-use-package "book" "dndbook"))
       org-dndbook-latex-preamble
       (when org-dnd-use-package "\\n\\usepackage{dnd}"))
     ("\\chapter{%s}" . "\\chapter*{%s}")
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
   )))

(unless (assoc "dndarticle" org-latex-classes)
   (add-to-list
    'org-latex-classes
   `("dndarticle"
     ,(concat
       (format
        org-dnd-latex-preamble
        "dndarticle")
       )
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     )
   )

(defun ordinal (n)
  (let ((str (if (numberp n) (number-to-string n) n)))
    (concat str
            (pcase (last str)
              ("1" "st")
              ("2" "nd")
              ("3" "rd")
              (_ "th")))))

(defun org-dnd--spell-level (level school)
  (org-trim
   (format "%s%s"
           (if level
               (concat (ordinal level) "-level ")
             "")
           (or (downcase school) ""))))

(defun org-dnd-spell (spell contents info)
  "Transcode a SPELL element from Org to D&D LaTeX.
CONTENTS holds the contents of the block.  INFO is a plist holding
contextual information."
  (let ((name (org-element-property :name spell))
        (level (org-export-read-attribute :attr_spell spell :level))
        (school (org-export-read-attribute :attr_spell spell :school))
        (ct (org-export-read-attribute :attr_spell spell :cast))
        (range (org-export-read-attribute :attr_spell spell :range))
        (mats (org-export-read-attribute :attr_spell spell :comp))
        (dur (org-export-read-attribute :attr_spell spell :duration)))
    (concat "\\DndSpellHeader%\n"
            (format "{%s}\n" (or name ""))
            (format "{%s}\n" (org-dnd--spell-level level school))
            (format "{%s}\n" (or ct ""))
            (format "{%s}\n" (or range ""))
            (format "{%s}\n" (or mats ""))
            (format "{%s}\n" (or dur ""))
            contents
            )))

;; Subtitle is a deprecated option in the latex template
(defun org-dnd--subtitle-block (subtitle contents info)
  "Transcode a SUBTITLE-BLOCK element to D&D LaTeX.
CONTENTS holds the contents of the block.  INFO is a plist holding
contextual information."
  (let ((content (split-string contents "\n" t nil)))
    (format "\\subtitlesection{%s}{%s}"
            (car content)
            (car (cdr content)))))

(defun org-dnd--item-block (item contents info)
  "Transcode a ITEM-BLOCK element to D&D LaTeX.
CONTENTS holds the contents of the block.  INFO is a plist holding
contextual information."
  (let ((content (split-string contents "\n" t nil)))
    (format "\\DndItemHeader{%s}{%s}\n%s"
            (car content)
            (car (cdr content))
            (replace-regexp-in-string
             "\\\\item\\[{\\([^}]+\\)}\\]"
             "\\\\item[\\1 :]"
             (replace-regexp-in-string
              "\\\\begin{description}"
              "\\\\begin{description}[nosep, after = { \\\\vspace{4pt plus 1pt minus 1pt} }]"
              (string-join (cdr (cdr content)) "\n")))
            )))

(defun org-dnd--extract-actions (content)
  (org-trim
   (replace-regexp-in-string
    "\n+"
    "\n"
    (replace-regexp-in-string
     "|"
     "\n"
     (replace-regexp-in-string
      "\\\\\\(begin\\|end\\){\\(description\\|itemize\\)}"
      ""
      (replace-regexp-in-string
       "\\\\item\\[{\\([^}]*\\)}] \\(.+?\\)|\\\\"
       "\\\\DndMonsterAction{\\1}|\\2\\\\par|\\\\"
       (replace-regexp-in-string
        "\\\\item\\[{\\([^}]*\\)}] \\(.+?\\)|\\\\"
        "\\\\DndMonsterAction{\\1}|\\2\\\\par|\\\\"
        (replace-regexp-in-string
         "\\\\item \\([^|]*\\)"
         "\\\\DndMonsterSection{\\1}"
         (replace-regexp-in-string "\n" "|" content)))))))))

(defun org-dnd--add-legendary-action-text (name content)
  (let ((nm (downcase name)))
    (replace-regexp-in-string
     "{Legendary Actions}"
     (format "{Legendary Actions}\nThe %s can take 3 legendary actions, choosing from the options below. Only one legendary action option can be used at a time and only at the end of another creature's turn. The %s regains spent legendary actions at the start of its turn.\n" nm nm)
     content)))

(defun org-dnd-monsterbox (monster contents info)
  "Transcode a monster box from Org to a D&D LaTeX monsterbox.
CONTENTS holds the contents of the table.  INFO is a plist holding
contextual information."
  (let ((name (org-element-property :name monster)))
    (concat
     "\\begin{DndMonster}"
     (if name (format "{%s}" name) "")
     "\n"
     ;; Race and info
     (let ((size (org-export-read-attribute :attr_monster_info monster :size))
           (race (org-export-read-attribute :attr_monster_info monster :race))
           (subrace (org-export-read-attribute :attr_monster_info monster :subrace))
           (alignment (org-export-read-attribute :attr_monster_info monster :alignment)))
       (when (and size race alignment)
         (concat
          "\\DndMonsterType{"
          (format "%s %s" (capitalize size) race)
          (when subrace (format " (%s)" subrace))
          (format ", %s" alignment)
          "}\n")))
     ;; Basics
     (let ((ac (org-export-read-attribute :attr_monster_basics monster :ac))
           (hp (org-export-read-attribute :attr_monster_basics monster :hp))
           (speed (org-export-read-attribute :attr_monster_basics monster :speed))
           ;; Special speed attributes
           (burrow (org-export-read-attribute :attr_monster_basics monster :burrow))
           (climb (org-export-read-attribute :attr_monster_basics monster :climb))
           (fly (org-export-read-attribute :attr_monster_basics monster :fly))
           (hover (org-export-read-attribute :attr_monster_basics monster :hover))
           (swim (org-export-read-attribute :attr_monster_basics monster :swim)))
       (concat "\\DndMonsterBasics[%\n"
               (format "armorclass = %s,\n" (or ac 0))
               (format "hitpoints = %s,\n" (or hp 0))
               (format "speed = {%s ft." (or speed 0))
               (when burrow (format ", burrow %s ft." burrow))
               (when climb (format ", climb %s ft." climb))
               (when fly (format ", fly %s ft." fly))
               (when hover (format ", fly %s ft. (hover)" hover))
               (when swim (format ", swim %s ft." swim))
               "},\n]\n"))
     ;; Stats
     (let ((con (org-export-read-attribute :attr_monster_stats monster :con))
           (str (org-export-read-attribute :attr_monster_stats monster :str))
           (dex (org-export-read-attribute :attr_monster_stats monster :dex))
           (int (org-export-read-attribute :attr_monster_stats monster :int))
           (wis (org-export-read-attribute :attr_monster_stats monster :wis))
           (cha (org-export-read-attribute :attr_monster_stats monster :cha)))
       (concat "\\DndMonsterAbilityScores[%\n"
               (format "CON = %s,\n" (or con 10))
               (format "STR = %s,\n" (or str 10))
               (format "DEX = %s,\n" (or dex 10))
               (format "INT = %s,\n" (or int 10))
               (format "WIS = %s,\n" (or wis 10))
               (format "CHA = %s,\n" (or cha 10))
               "]\n"))
     ;; Details
     (let ((skills (org-export-read-attribute :attr_monster_details monster :skills))
           (saves (org-export-read-attribute :attr_monster_details monster :saves))
           (cim (org-export-read-attribute :attr_monster_details monster :cim))
           (imm (org-export-read-attribute :attr_monster_details monster :imm))
           (res (org-export-read-attribute :attr_monster_details monster :res))
           (vul (org-export-read-attribute :attr_monster_details monster :vul))
           (senses (org-export-read-attribute :attr_monster_details monster :senses))
           (langs (org-export-read-attribute :attr_monster_details monster :langs))
           (cr (org-export-read-attribute :attr_monster_details monster :cr)))
       (concat "\\DndMonsterDetails[%\n"
               (when skills (format "skills = {%s},\n" skills))
               (when saves (format "saving-throws = {%s},\n" saves))
               (when cim (format "condition-immunities = {%s},\n" cim))
               (when imm (format "damage-immunities = {%s},\n" imm))
               (when res (format "damage-resistances = {%s},\n" res))
               (when vul (format "damage-vulnerabilities = {%s},\n" vul))
               (when senses (format "senses = {%s},\n" senses))
               (when langs (format "languages = {%s},\n" langs))
               (format "challenge = {%s},\n" (or cr 0))
               "]\n"))
     ;; Abilities and actions
     (org-dnd--add-legendary-action-text
      name
      (org-dnd--extract-actions contents))
     "\n\\end{DndMonster}")))

(defun org-dnd-headline (headline contents info)
  "Transcode a HEADLINE element from Org to LaTeX.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (let ((text (org-latex-headline headline contents info))
        (tags (org-element-property :tags headline)))
    (if (member "map" tags)
        (progn
          (org-element-put-property headline :tags (remove "map" tags))
          (replace-regexp-in-string
           "\\\\subsubsection{"
           "\\\\DndSubArea{"
          (replace-regexp-in-string
           "\\\\subsection{"
           "\\\\DndArea{"
           (org-latex-headline headline contents info))))
      (org-latex-headline headline contents info))))


;; Horizontal Rule
;; We need to redefine this to constraint the line just to one column
(defun org-dnd-horizontal-rule (horizontal-rule _contents info)
  "Transcode an HORIZONTAL-RULE object from Org to LaTeX.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((attr (org-export-read-attribute :attr_latex horizontal-rule))
	      (prev (org-export-get-previous-element horizontal-rule info)))
    (concat
     ;; Make sure the rule doesn't start at the end of the current
     ;; line by separating it with a blank line from previous element.
     (when (and prev
		            (let ((prev-blank (org-element-property :post-blank prev)))
		              (or (not prev-blank) (zerop prev-blank))))
       "\n")
     (org-latex--wrap-label
      horizontal-rule
      (format "\\noindent\\rule{%s}{%s}"
	            (or (plist-get attr :width) "\\linewidth")
	            (or (plist-get attr :thickness) "0.5pt"))
      info))))


;; HACK There has to be an easier way to add the package options as a derived
;; LaTeX backend.
(defun org-dnd-template (contents info)
  "Return complete document string after LaTeX conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (defun bool (b) (if b "true" "false"))
  (let* ((just (plist-get info :justified))
         (layout (plist-get info :layout))
         (bg (plist-get info :with-bg))
         (no-multitoc (plist-get info :with-no-multitoc))
         (high-contrast (plist-get info :with-high-contrast))
         (class-options
          (concat
           (when just "justified,")
           (when bg (format "bg=%s," bg))
           (when (and org-dnd-use-package layout)
             (format "layout=%s," (bool org-dnd-use-package)))
           (if no-multitoc "nomultitoc")
           (if high-contrast "highcontrast"))))
    (let ((body (org-latex-template contents info)))
      (replace-regexp-in-string
       "\\[CO\\]"
       (concat "," (replace-regexp-in-string
                    ",$" ""
                    class-options))
       body t))))

(defun org-dnd-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to D&D LaTeX.
CONTENTS holds the contents of the block.  INFO is a plist holding
contextual information."
  (let* ((type (downcase (org-element-property :type special-block)))
         (name (org-element-property :name special-block))
         ;; Get the first line of the content as title if name is null
         (title (if name name
                  (car (split-string contents "\n" t nil))
                ))
         ;; Remove first like if being used as title
         (content (if name contents
                    (string-join (cdr (split-string contents "\n" nil nil)) "\n")
                    ))
         )
    (pcase type
      ("commentbox"
       (concat (format "\\begin{%s}{%s}" "DndComment" title)
               (org-latex--caption/label-string special-block info)
               content
               (format "\\end{%s}""DndComment")))
      ("sidebar"
       (concat (format "\\begin{%s}[float=!h]{%s}" "DndSidebar" title)
               (org-latex--caption/label-string special-block info)
               content
               (format "\\end{%s}\n" "DndSidebar")))
      ("readaloud"
       (concat (format "\\begin{%s}\n" "DndReadAloud")
               contents
               (format "\\end{%s}" "DndReadAloud")))
      ("spell" (org-dnd-spell special-block contents info))
      ("item" (org-dnd--item-block special-block contents info))
      ("subtitle" (org-dnd--subtitle-block special-block contents info))
      ("monster" (org-dnd-monsterbox special-block contents info))
      (_ (org-latex-special-block special-block contents info)))))

(defun org-dnd-table (table contents info)
  "Transcode a table from Org to a D&D LaTeX table.
CONTENTS holds the contents of the table.  INFO is a plist holding
contextual information."
  (let* ((header (car (org-element-property :header table)))
         (color (org-export-read-attribute :attr_dnd table :color))
         (lines (org-export-read-attribute :attr_dnd table :lines)) ;; DnDTable is not compatible with lines
         (format (org-export-read-attribute :attr_dnd table :format))
         (separate (org-export-read-attribute :attr_dnd table :separate))
         (table-width (list-length
                       (org-element-map table
                           '(table-row)
                         (lambda (row)
	                         (and (eq (org-element-property :type row) 'standard)
                                (org-element-contents row))) info 'first-match)))
         (column-format (cond ;; ornamental does not support justification
                         ((equal format "ornamental") "l")
                         (t "X")
                         ))
         (default-align (concat "c"
                                (mapconcat 'identity (make-list (1- table-width) column-format) "")))
         (align (cond
                 ((org-export-read-attribute :attr_dnd table :align))
                 ((equal format "long") "")  ;; long does auto-alignment
                 (t default-align)))
         (color-header (if color (format "[color=%s]" color) ""))
         ;; long does not support headers
         (header-header (if (and header (not (equal format "long"))) (format "[header=%s]" header) ""))
         (align-header (if align (format
                                  (cond
                                   ((equal format "long") "[%s]")
                                   (t "{%s}")
                                   )
                                  align) ""))
         (format-header (cond
                         ((equal format "alt") "DndAltTable")
                         ((equal format "ornamental") "ornamentedtabular")
                         ((equal format "long") "dndlongtable")
                         (t "DndTable")
                         ))
        )
    (format
     "%s"
     (replace-regexp-in-string
      "begin{tabular.*"
      (format "begin{%s}%s%s%s"
              format-header
              color-header
              header-header
              align-header
              )
      (replace-regexp-in-string
       "end{tabular.*}"
       (format "end{%s}" format-header)
       (replace-regexp-in-string
        "{table}"
        "{table*}"
        (replace-regexp-in-string
         "\\\\\\(begin\\|end\\){center}\n?"
         ""
         (replace-regexp-in-string
          (format "\\\\centering\n?%s"
                  (if lines "" "\\|\\\\toprule\n?\\|\\\\bottomrule\n?")
                  )
          ""
          (replace-regexp-in-string
           "\\\\hline\n?"
           (if (not separate) ""
             (format "\\\\end{%s}\n\\\\begin{%s}%s%s"
                     format-header
                     format-header
                     align-header
                     color-header))
           (org-latex-table table contents info))))))))))

(org-export-define-derived-backend 'dnd 'latex
  :menu-entry
  '(?l 1
       ((?d "As PDF file (D&D)" org-dnd-export-to-pdf)
        (?D "As PDF file and open (D&D)"
            (lambda (a s v b)
              (if a (org-dnd-export-to-pdf t s v b)
                (org-open-file (org-dnd-export-to-pdf nil s v b)))))))
  :options-alist
  '((:justified nil "justified" t t)
    (:layout nil "layout" t nil)
    (:with-bg nil "bg" "full" t)
    (:with-no-multitoc nil "no-multitoc" nil t)
    (:with-high-contrast nil "high-contrast" nil t)
    (:with-title nil "title" org-export-with-title t)
    (:headline-levels nil "H" 7 t)
    (:latex-caption-above nil nil nil)
    (:latex-toc-command nil nil org-dnd-latex-toc-command)
    )
  :translate-alist '((headline . org-dnd-headline)
                     (horizontal-rule . org-dnd-horizontal-rule)
                     (template . org-dnd-template)
                     (table . org-dnd-table)
                     (special-block . org-dnd-special-block)))

;;;###autoload
(defun org-dnd-export-as-latex
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a D&D buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{document}\" and \"\\end{document}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org D&D Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil.

Sourced from ox-beamer."
  (interactive)
  (org-export-to-buffer 'dnd "*Org D&D Export*"
    async subtreep visible-only body-only ext-plist (lambda () (LaTeX-mode))))

;;;###autoload
(defun org-dnd-export-to-latex
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a D&D LaTeX file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{document}\" and \"\\end{document}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return PDF file's name.

Sourced from ox-beamer."
  (interactive)
  (let ((file (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'dnd file
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun org-dnd-export-to-pdf
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a Beamer presentation (PDF).

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{document}\" and \"\\end{document}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return PDF file's name.

Sourced from ox-beamer."
  (interactive)
  (let ((file (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'dnd file
      async subtreep visible-only body-only ext-plist
      (lambda (file) (org-latex-compile file)))))

(provide 'ox-dnd)

;;; ox-dnd.el ends here
