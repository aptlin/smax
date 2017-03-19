(defmacro τ (file keymap key fnc)
  "When FILE is loaded, add to KEYMAP key binding KEY (invoking FNC)."
  `(eval-after-load ',file
     '(define-key
        (symbol-value (intern (concat (symbol-name ',keymap) "-mode-map")))
        (kbd ,key) ,fnc)))
(provide 'smax-builders)
