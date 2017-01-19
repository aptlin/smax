(TeX-add-style-hook
 ".emacs"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("scrartcl" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("sdll" "beaue" "pset" "anon")))
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
   (TeX-run-style-hooks
    "latex2e"
    "scrartcl"
    "scrartcl11"
    "sdll"
    "lineno")
   (TeX-add-symbols
    '("lr" 1)
    '("R" 1)
    '("mbt" 1)
    '("mbv" 1)
    '("mbn" 1)
    '("mbh" 1)
    "LongestHence"
    "LongestName"
    "LongestValue"
    "LongestText")
   (LaTeX-add-labels
    "#1")
   (LaTeX-add-lengths
    "LargestHenceSize"
    "LargestNameSize"
    "LargestValueSize"
    "LargestTextSize"))
 :latex)

