;;; org-pdcite.el --- Pandoc citations in org-mode

;; Copyright (C) 2015 Erik Hetzner

;; Author: Erik Hetzner <egh@e6h.org>
;; Keywords: bib

;; This file is not part of GNU Emacs.

;; org-pdcite.el is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; org-pdcite.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with org-pdcite.el. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'cl-lib)

(defun org-pdcite-skip-whitespace ()
  (while (looking-at "[[:blank:]]")
    (forward-char)))

;; TODO: Make locate dependent.
(defconst org-pdcite-locator-words
  '("book" "books" "chapter" "chapters" "column" "columns" "figure"
    "figures" "folio" "folios" "number" "numbers" "line" "lines" "note"
    "notes" "opus" "opera" "page" "pages" "page" "pages" "paragraph"
    "paragraph" "part" "parts" "section" "sections" "sub verbo" "sub
    verbis" "verse" "verses" "volume" "volumes" "bk." "chap." "col."
    "fig." "f." "no." "l." "n." "op." "p." "pp." "p." "pp." "para." "pt."
    "sec." "s.v." "s.vv." "v." "vv." "vol." "vols." "¶" "¶¶" "§" "§§" )
  "Terms to use as locators.")

(defconst org-pdcite-term-re
  "\\([]\\.,;[:blank:]]\\|$\\)")

(defun org-pdcite-make-alt-parser (&rest seq)
  "Return function to parse any one of a SEQ of expressions.

Each element SEQ is a parse function.  Returns nil if all possible
parses failed and sets the point back to where it was.  If any one
parse succeeds, returns it.  The first parser in SEQ that matches
will be used."
  (lexical-let ((seq seq))
    (lambda ()
      (let ((start (point)))
        (catch 'done
          (dolist (elem seq)
            (let ((next (funcall elem)))
              (if next
                  (throw 'done next))))
          (goto-char start)
          nil)))))

(defun org-pdcite-make-seq-parser (&rest seq)
  "Return function to parse a sequence SEQ of expressions.

Each element SEQ is a parse function.  Returns nil if any parse
failed and sets the point back to where it was.  If the parse
succeeds, returns a list of list returned by the parse functions
or t if all the parse functions returned t."
  (lexical-let ((seq seq))
    (lambda ()
      (let ((start (point))
            elem retval)
        (catch 'break
          (dolist (elem seq)
            (let ((next (funcall elem)))
              (if next
                  (unless (eq t next)
                    ;; only append if not t
                    (setq retval (append retval next)))
                (goto-char start)
                (throw 'break nil))))
          (or retval t))))))

(defun org-pdcite-make-char-parser (char)
  "Return function to parse CHAR."
  (lexical-let ((char char))
    (lambda ()
      (org-pdcite-skip-whitespace)
      (if (eq char (char-after))
          (progn
            (forward-char)
            t)
        nil))))

(defun org-pdcite-make-opt-parser (parser &optional wrapper)
  "Return function to optionally parse with PARSER.

If PARSER does not succeed, returns t.  Otherwise returns the
value returned by PARSER.

If WRAPPER argument is provided, it should be a function that
takes one argument.  It will be called with the results of the
parser and its return value will be returned.  WRAPPER will not be
called if the parser does not match."
  (lexical-let ((parser parser)
                (wrapper wrapper))
    (lambda ()
      (let ((retval (funcall parser)))
        (if retval
            (if wrapper
                (funcall wrapper retval)
              retval)
          t)))))

(defun org-pdcite-make-regexp-parser (regexp &optional wrapper)
  "Return function to parse REGEXP.

If WRAPPER is defined, it should be a function that returns a
structure for the parser to return.  This function can access
`match-data', etc.  It should not return nil."
  (lexical-let ((regexp regexp)
                (wrapper wrapper))
    (lambda ()
      (org-pdcite-skip-whitespace)
      (if (and (looking-at regexp)
               (save-match-data
                 (looking-at (format "%s%s" regexp org-pdcite-term-re))))
          (progn
            (goto-char (match-end 0))
            (if wrapper
                (funcall wrapper)
              (match-string 0)))
        nil))))

(defun org-pdcite-make-zero-or-more-parser (parser &optional wrapper)
  "Return a function that will parse zero or more of PARSER.

Function returns t if zero instances of PARSER parsed, or a list
of the results of PARSER.

If WRAPPER argument is provided, it should be a function that
takes one argument.  It will be called with the results of the
parser and its return value will be returned."
  (lexical-let ((parser parser)
                (wrapper wrapper))
    (lambda ()
      (let (tmp retval)
        (while (setq tmp (funcall parser))
          (setq retval (append retval (list tmp))))
        (if wrapper
            (funcall wrapper (or retval t))
          (or retval t))))))

(defun org-pdcite-make-one-or-more-parser (parser &optional wrapper)
  "Return a function that will parse one or more of PARSER.

Function returned will return a list of the results of PARSER.

If WRAPPER argument is provided, it should be a function that
takes one argument.  It will be called with the results of the
parser and its return value will be returned."
  (lexical-let ((parser parser)
                (wrapper wrapper))
    (lambda ()
      (let (tmp retval)
        (if (setq tmp (funcall parser))
            (progn
              (setq retval (append retval (list tmp)))
              (while (setq tmp (funcall parser))
                (setq retval (append retval (list tmp))))
              (if wrapper
                  (funcall wrapper retval)
                retval))
          nil)))))

(defun org-pdcite-wrap-parser (parser wrapper)
  (lexical-let ((parser parser)
                (wrapper wrapper))
    (lambda ()
      (let ((retval (funcall parser)))
        (if retval
            (funcall wrapper retval)
          nil)))))

(defalias 'org-pdcite-short-cite-parser
  (org-pdcite-make-regexp-parser
   "\\(-?\\)@\\([[:alnum:]:]+\\)"
   (lambda ()
     `(:suppress-author ,(string= "-" (match-string 1))
                        :citekey ,(match-string 2)))))

(defconst org-pdcite-word-char-re
  "[[:alnum:].,\'\"\(\)</>-]")

(defalias 'org-pdcite-greedy-token-parser
  (org-pdcite-make-regexp-parser
   (format "%s+" org-pdcite-word-char-re)))

;; we want to capture: 123, 123A, C22, XVII, 33-44, 22-33; 22-11
(defalias 'org-pdcite-word-with-digits-parser
  (org-pdcite-make-alt-parser
   (org-pdcite-make-regexp-parser
    (format "%s*[0-9]%s*" org-pdcite-word-char-re org-pdcite-word-char-re))
   (org-pdcite-make-regexp-parser "[IVXLCMivxlcm]+")))

(defalias 'org-pdcite-suffix-parser
  (org-pdcite-make-one-or-more-parser
   #'org-pdcite-greedy-token-parser
   (lambda (val)
     `(:suffix ,(mapconcat 'identity val " ")))))

(defalias 'org-pdcite-prefix-parser
  (org-pdcite-make-one-or-more-parser
   #'org-pdcite-greedy-token-parser
   (lambda (val)
     `(:prefix ,(mapconcat 'identity val " ")))))

(defalias 'org-pdcite-locator-word-parser
  (org-pdcite-make-regexp-parser
   (regexp-opt org-pdcite-locator-words)
   (lambda ()
     `(:locator-word ,(match-string 0)))))

(defalias 'org-pdcite-locator-parser
  (org-pdcite-make-seq-parser
   (org-pdcite-make-opt-parser
    (org-pdcite-make-char-parser ?,))
   (org-pdcite-make-alt-parser
    (org-pdcite-make-one-or-more-parser
     #'org-pdcite-word-with-digits-parser
     (lambda (val)
       `(:locator ,@val)))
    (org-pdcite-make-seq-parser
     #'org-pdcite-locator-word-parser
     (org-pdcite-make-one-or-more-parser
      #'org-pdcite-word-with-digits-parser
      (lambda (val)
        `(:locator ,@val)))))))

(defalias 'org-pdcite-full-cite-parser
  (org-pdcite-make-seq-parser
   (org-pdcite-make-alt-parser
    #'org-pdcite-short-cite-parser
    (org-pdcite-make-seq-parser
     #'org-pdcite-prefix-parser
     #'org-pdcite-short-cite-parser))
   (org-pdcite-make-opt-parser
    #'org-pdcite-locator-parser)
   (org-pdcite-make-opt-parser
    #'org-pdcite-suffix-parser)))

(defalias 'org-pdcite-rest-cite-parser
  (org-pdcite-make-seq-parser
   (org-pdcite-make-char-parser ?\;)
   #'org-pdcite-full-cite-parser))

(defalias 'org-pdcite-bracketed-cite-parser
  (org-pdcite-make-seq-parser
   (org-pdcite-make-char-parser ?\[)
   #'org-pdcite-full-cite-parser
   (org-pdcite-make-zero-or-more-parser
    #'org-pdcite-rest-cite-parser)
   (org-pdcite-make-char-parser ?\])))

(defalias 'org-pdcite-short-cite-extra-parser
  (org-pdcite-make-seq-parser
   (org-pdcite-make-char-parser ?\[)
   #'org-pdcite-locator-parser
   (org-pdcite-make-opt-parser
    #'org-pdcite-suffix-parser)
   (org-pdcite-make-zero-or-more-parser
    #'org-pdcite-rest-cite-parser)
   (org-pdcite-make-char-parser ?\])))

(defalias 'org-pdcite-top-cite-parser
  (org-pdcite-make-alt-parser
   #'org-pdcite-bracketed-cite-parser
   (org-pdcite-make-seq-parser
    #'org-pdcite-short-cite-parser
    #'org-pdcite-short-cite-extra-parser)
   (org-pdcite-make-seq-parser
    #'org-pdcite-short-cite-parser
    #'org-pdcite-bracketed-cite-parser)
   #'org-pdcite-short-cite-parser))

(provide 'org-pdcite)
;;; org-pdcite.el ends here
