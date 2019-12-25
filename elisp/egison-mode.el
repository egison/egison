;;; egison-mode.el --- Egison editing mode

;; Copyright (C) 2011-2015 Satoshi Egi

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the Software
;; is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included
;; in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR
;; A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
;; CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
;; OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Author: Satoshi Egi <egisatoshi@gmail.com>
;;; URL: https://github.com/egisatoshi/egison3/blob/master/elisp/egison-mode.el
;;; Version: 0.1.5

;;; Commentary:

;; Emacs Mode for Egison
;;
;; Please put it in your load-path of Emacs. Then, add the following
;; lines in your .emacs.
;;
;;   (autoload 'egison-mode "egison-mode" "Major mode for editing Egison code." t)
;;   (setq auto-mode-alist (cons `("\\.egi$" . egison-mode) auto-mode-alist))

;;; Code:

(defconst egison-font-lock-keywords-1
  (eval-when-compile
    (list
     "\\<load\\>"
     "\\<loadFile\\>"

     "\\<let\\>"
     "\\<withSymbols\\>"
     "\\<if\\>"
     "\\<generateArray\\>"
     "\\<arrayBounds\\>"
     "\\<arrayRef\\>"
     "\\<tensor\\>"
     "\\<generateTensor\\>"
     "\\<contract\\>"
     "\\<tensorMap\\>"

     "\\<loop\\>"
     "\\<match\\>"
     "\\<matchDFS\\>"
     "\\<matchAll\\>"
     "\\<matchAllDFS\\>"
     "\\<matcher\\>"
     "\\<algebraicDataMatcher\\>"

     "\\<do\\>"
     "\\<io\\>"
     "\\<seq\\>"

     "\\<undefined\\>"
     "\\<something\\>"

;     ":="
     "::"
     "++"
     "\\\.\\\.\\\."
     "->"
     "#"
;     "'"
     "`"
     "\\\#"
     "|"
     "\\\&"
     "@"
     "!"
     "?"
;     "\\<_\\>"

     "\\<assert\\>"
     "\\<assert-equal\\>"
     ))
  "Subdued expressions to highlight in Egison modes.")

(defconst egison-font-lock-keywords-2
  (append egison-font-lock-keywords-1
   (eval-when-compile
     (list
      (cons "\\\$\\\w*" font-lock-variable-name-face)
      (cons "\\\%\\\w*" font-lock-variable-name-face)
      )))
  "Gaudy expressions to highlight in Egison modes.")

(defvar egison-font-lock-keywords egison-font-lock-keywords-1
  "Default expressions to highlight in Egison modes.")

(defun egison-indent-line ()
  "indent current line as Egison code."
  (interactive)
  )


(defvar egison-mode-map
  (let ((smap (make-sparse-keymap)))
    (define-key smap "\C-j" 'newline-and-indent)
    smap)
  "Keymap for Egison mode.")


(defvar egison-mode-syntax-table
  (let ((table (make-syntax-table)))

    (modify-syntax-entry ?\{  "(}1nb" table)
    (modify-syntax-entry ?\}  "){4nb" table)
    (modify-syntax-entry ?-  "_ 123" table)
    (modify-syntax-entry ?\-  "_ 123" table)
;    (modify-syntax-entry ?\;  "_ 123" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  ;; (copy-syntax-table lisp-mode-syntax-table)
  "Syntax table for Egison mode")

(defun egison-mode-set-variables ()
  (set-syntax-table egison-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults)
       '((egison-font-lock-keywords
          egison-font-lock-keywords-1 egison-font-lock-keywords-2)
         nil t (("+*/=!?%:_~.'∂∇αβγδεζχθικλμνξοπρςστυφχψωΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ" . "w"))
         ))
  (set (make-local-variable 'indent-line-function) 'egison-indent-line)
  (set (make-local-variable 'comment-start) "--")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "{-+ *\\|--+ *")
  (set (make-local-variable 'comment-add) 1)
  (set (make-local-variable 'comment-end-skip) nil)
  )


;;;###autoload
(defun egison-mode ()
  "Major mode for editing Egison code.

Commands:
\\{egison-mode-map}
Entry to this mode calls the value of `egison-mode-hook'
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (setq indent-tabs-mode nil)
  (use-local-map egison-mode-map)
  (setq major-mode 'egison-mode)
  (setq mode-name "Egison")
  (egison-mode-set-variables)
  (run-mode-hooks 'egison-mode-hook))


(defgroup egison nil
  "Editing Egison code."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'lisp)

(defcustom egison-mode-hook nil
  "Normal hook run when entering `egison-mode'.
See `run-hooks'."
  :type 'hook
  :group 'egison)

(provide 'egison-mode)

;;; egison-mode.el ends here
