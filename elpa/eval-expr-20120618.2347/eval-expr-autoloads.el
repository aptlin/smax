;;; eval-expr-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "eval-expr" "eval-expr.el" (22469 14812 118002
;;;;;;  145000))
;;; Generated autoloads from eval-expr.el

(defvar eval-expr-error-message-delay 3 "\
*Amount of time, in seconds, to display in echo area before continuing.")

(custom-autoload 'eval-expr-error-message-delay "eval-expr" t)

(defvar eval-expr-prompt "Eval: " "\
*Prompt used by eval-expr.")

(custom-autoload 'eval-expr-prompt "eval-expr" t)

(defvar eval-expr-honor-debug-on-error t "\
*If non-nil, do not trap evaluation errors.
Instead, allow errors to throw user into the debugger, provided
debug-on-error specifies that the particular error is a debuggable condition.")

(custom-autoload 'eval-expr-honor-debug-on-error "eval-expr" t)

(defvar eval-expr-use-echo-area-or-buffer 1 "\
*Preference for when to use echo area of a temporary buffer for results.

If set to t or `buffer', always put results into a temporary buffer.
If set to `nil' or `echo-area', always display results in echo area.
If an integer N, use the echo area unless the results would require more
than N lines to display; in that case, use a temporary buffer.

Some versions of emacs can display arbitrarily large output in the echo
area by dynamically resizing it, so a temporary buffer is not necessary
unless you expect the output to exceed the limits of the resize thresholds
or want to be able to edit the results.")

(custom-autoload 'eval-expr-use-echo-area-or-buffer "eval-expr" t)

(defvar eval-expr-print-level (cond ((boundp 'eval-expression-print-level) (default-value 'eval-expression-print-level)) ((boundp 'print-level) (default-value 'print-level))) "\
*Like print-level, but affect results printed by `eval-expr' only.")

(custom-autoload 'eval-expr-print-level "eval-expr" t)

(defvar eval-expr-print-length (cond ((boundp 'eval-expression-print-length) (default-value 'eval-expression-print-length)) ((boundp 'print-length) (default-value 'print-length))) "\
*Like print-length, but affect results printed by `eval-expr' only.")

(custom-autoload 'eval-expr-print-length "eval-expr" t)

(defvar eval-expr-print-function (if (fboundp 'pp) 'pp 'prin1) "\
*Function to use for printing objects.
E.g. this can be set to `pp' to generate pretty-printed results,
or `prin1' for unformatted results.")

(custom-autoload 'eval-expr-print-function "eval-expr" t)

(autoload 'eval-expr-install "eval-expr" "\
Replace standard eval-expression command with enhanced eval-expr.

\(fn)" t nil)

(autoload 'eval-expr "eval-expr" "\
Evaluate EXPRESSION and print value in minibuffer, temp, or current buffer.
A temp output buffer is used if there is more than one line in the
evaluated result.
If invoked with a prefix arg, or second lisp argument EE::INSERT-VALUE is
non-nil, then insert final value into the current buffer at point.

Value is also consed on to front of the variable `values'.

\(fn EE::EXPRESSION &optional EE::INSERT-VALUE)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; eval-expr-autoloads.el ends here
