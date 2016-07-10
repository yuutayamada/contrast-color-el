;;; contrast-color.el --- Pick best contrast color for you -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Yuta Yamada

;; Author: Yuta Yamada <cokesboy[at]gmail.com>
;; URL: https://github.com/yuutayamada/contrast-color-el
;; Version: 1.0.0
;; Package-Requires: ((cl-lib "0.5"))
;; Keywords: color, convenience

;;; The MIT License (MIT)

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;;
;; This package only provide a single function that return a contrast
;; color using CIEDE2000 algorithm.  The contrast color will be picked
;; from ‘contrast-color-candidates’ variable.  The default
;; colors are based on Google’s material design palette
;; (https://material.google.com/style/color.html)
;;
;;
;; Usage:
;;
;;   (contrast-color "#ff00ff") ; -> "#4caf50"
;;
;;                  or
;;
;;   (contrast-color "Brightmagenta") ; -> "#4caf50"
;;
;;; Code:

(require 'color)
(require 'cl-lib)

(defgroup contrast-color nil "contrast-color group"
  :group 'convenience)

(defcustom contrast-color-candidates
  ;; license: http://zavoloklom.github.io/material-design-color-palette/license.html
  '("#f44336" "#e91e63" "#9c27b0" ; Red, Pink, Purple
    "#673ab7" "#3f51b5" "#2196f3" ; Deep Purple, Indigo, Blue
    "#03a9f4" "#00bcd4" "#009688" ; Light Blue, Cyan, Teal
    "#4caf50" "#8bc34a" "#cddc39" ; Green, Light Green, Lime
    "#ffeb3b" "#ffc107" "#ff9800" ; Yellow, Amber, Orange
    "#ff5722" "#795548" "#9e9e9e" ; Deep Orange, Brown, Grey
    "#607d8b" "#000000" "#ffffff" ; Blue Grey, Black, White
    )
  "List of colors.  One of those colors is used as the contrast color."
  :group 'contrast-color
  :type '(repeat :tag "list of colors" string))

;; TODO: make this variable saves when users exit Emacs, so don’t need
;; same calculation again.
(defcustom contrast-color-cache nil
  "Alist of specified base color and contrast color."
  :group 'contrast-color
  :type '(choice
          (const :tag "Initial value" nil)
          (repeat :tag "Cons sell of specified color and contrast color"
                  (cons string string))))

(defcustom contrast-color-use-hex-name t
  "If non-nil, returned color name will be hex value."
  :group 'contrast-color
  :type 'bool)

;;;;;;;;;;;;;;;;
;; Functions

(defun contrast-color--get-lab (color)
  "Get CIE l*a*b from COLOR."
  (apply 'color-srgb-to-lab (color-name-to-rgb color)))

(defun contrast-color--compute-core (base-color)
  "Return alist of (color-of-candidate . ciede2000).
As the reference BASE-COLOR will be used to compare on the process."
  (let* ((candidates contrast-color-candidates)
         (b (contrast-color--get-lab base-color))
         (labs-and-colors
          (cl-mapcar
           (lambda (c) (cons (contrast-color--get-lab c) c)) candidates)))
    (cl-mapcar (lambda (pair) (cons (color-cie-de2000 b (car pair)) (cdr pair)))
               labs-and-colors)))

(defun contrast-color--compute (color)
  "Return contrast color against COLOR."
  (cl-loop
   with cie-and-colors = (contrast-color--compute-core color)
   for (cie . c) in cie-and-colors
   for best = (cons cie c) then (if (< (car best) cie) (cons cie c) best)
   finally return (cdr best)))

;;;###autoload
(defun contrast-color (color)
  "Return most contrasted color against COLOR.
The return color picked from ‘contrast-color-candidates’.
The algorithm is used CIEDE2000. See also ‘color-cie-de2000’ function."
  (let ((cached-color (assoc-default color contrast-color-cache)))
    (if cached-color
        cached-color
      (let ((c (contrast-color--format (contrast-color--compute color))))
          (add-to-list 'contrast-color-cache (cons color c))
          c))))

(defun contrast-color--format (color)
  "Format color name.
If ‘contrast-color-use-hex-name’ is non-nil, convert COLOR name to hex form."
  (if (and contrast-color-use-hex-name
           (not (eq ?# (string-to-char color))))
      (apply 'color-rgb-to-hex (color-name-to-rgb color))
    color))

(provide 'contrast-color)
;;; contrast-color.el ends here
