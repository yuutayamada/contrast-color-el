
;;; Code:
(require 'contrast-color)
(require 'cl-lib)
(require 'ert)

(setq contrast-color-candidates contrast-color-material-colors
      contrast-color--lab-cache nil)

(ert-deftest test-contrast-color ()
  "A test for contrast-color package"
  ;; FIXME: why those tests return different value from my Emacs?
  ;; (but seems fixed value...)
  (should (equal "#64dd17" (contrast-color "black")))
  (should (equal "#263238" (contrast-color "white"))))

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; no-byte-compile: t
;; End:

;;; test.el ends here
