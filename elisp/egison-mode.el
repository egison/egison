;;; egison-mode.el --- Egison editing mode

;; Copyright (C) 2011-2026 Satoshi Egi

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
;;; URL: https://github.com/egisatoshi/egison/blob/master/elisp/egison-mode.el
;;; Version: 0.3.0

;;; Commentary:

;; Emacs Mode for Egison
;;
;; Please put it in your load-path of Emacs. Then, add the following
;; lines in your .emacs.
;;
;;   (autoload 'egison-mode "egison-mode" "Major mode for editing Egison code." t)
;;   (setq auto-mode-alist (cons `("\\.egi$" . egison-mode) auto-mode-alist))

;;; Code:

;; ============================================================
;; Face definitions
;; ============================================================

(defface egison-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face for Egison keywords."
  :group 'egison)

(defface egison-type-keyword-face
  '((t :inherit font-lock-type-face))
  "Face for Egison type system keywords (class, instance, inductive, etc.)."
  :group 'egison)

(defface egison-builtin-type-face
  '((t :inherit font-lock-type-face))
  "Face for Egison built-in type names."
  :group 'egison)

(defface egison-definition-face
  '((t :inherit font-lock-keyword-face))
  "Face for Egison definition keywords."
  :group 'egison)

(defface egison-pattern-variable-face
  '((t :inherit font-lock-variable-name-face))
  "Face for Egison pattern variables ($x, etc.)."
  :group 'egison)

(defface egison-value-pattern-face
  '((t :inherit font-lock-constant-face))
  "Face for Egison value patterns (#x, etc.)."
  :group 'egison)

(defface egison-constructor-face
  '((t :inherit font-lock-constant-face))
  "Face for Egison data constructors and boolean values."
  :group 'egison)

(defface egison-type-constraint-face
  '((t :inherit font-lock-type-face))
  "Face for Egison type constraints ({Eq a}, etc.)."
  :group 'egison)

;; ============================================================
;; Font-lock keywords level 1 (basic)
;; ============================================================

(defconst egison-font-lock-keywords-1
  (eval-when-compile
    (list
     ;; ----- Type system keywords (highlighted prominently) -----
     (cons (concat "\\<" (regexp-opt
       '("class" "instance" "inductive" "extends" "declare") t)
       "\\>")
       'font-lock-type-face)

     ;; ----- Definition keywords -----
     (cons (concat "\\<" (regexp-opt
       '("def" "let" "in" "where") t)
       "\\>")
       'font-lock-keyword-face)

     ;; ----- Module and loading -----
     (cons (concat "\\<" (regexp-opt
       '("load" "loadFile" "execute") t)
       "\\>")
       'font-lock-builtin-face)

     ;; ----- Control flow -----
     (cons (concat "\\<" (regexp-opt
       '("if" "then" "else") t)
       "\\>")
       'font-lock-keyword-face)

     ;; ----- Pattern matching -----
     (cons (concat "\\<" (regexp-opt
       '("match" "matchDFS" "matchAll" "matchAllDFS"
         "as" "with" "forall" "loop") t)
       "\\>")
       'font-lock-keyword-face)

     ;; ----- Matcher definition -----
     (cons (concat "\\<" (regexp-opt
       '("matcher" "algebraicDataMatcher") t)
       "\\>")
       'font-lock-keyword-face)

     ;; ----- Lambda and special forms -----
     (cons (concat "\\<" (regexp-opt
       '("memoizedLambda" "cambda" "capply"
         "withSymbols" "function") t)
       "\\>")
       'font-lock-keyword-face)

     ;; ----- Tensor operations -----
     (cons (concat "\\<" (regexp-opt
       '("tensor" "generateTensor" "contract"
         "tensorMap" "tensorMap2"
         "transpose" "flipIndices"
         "subrefs" "suprefs" "userRefs") t)
       "\\>")
       'font-lock-builtin-face)

     ;; ----- IO and sequencing -----
     (cons (concat "\\<" (regexp-opt
       '("do" "seq") t)
       "\\>")
       'font-lock-keyword-face)

     ;; ----- Infix declarations -----
     (cons (concat "\\<" (regexp-opt
       '("infixr" "infixl" "infix" "expression" "pattern") t)
       "\\>")
       'font-lock-keyword-face)

     ;; ----- Special values -----
     (cons (concat "\\<" (regexp-opt
       '("undefined" "something") t)
       "\\>")
       'font-lock-constant-face)

     ;; ----- Built-in type names -----
     (cons (concat "\\<" (regexp-opt
       '("Integer" "MathExpr" "Float" "Bool" "Char" "String"
         "IO" "Matcher" "Pattern"
         "Tensor" "Vector" "Matrix" "DiffForm"
         "List") t)
       "\\>")
       'font-lock-type-face)

     ;; ----- Boolean literals -----
     (cons (concat "\\<" (regexp-opt '("True" "False") t) "\\>")
       'font-lock-constant-face)

     ;; ----- Testing primitives -----
     (cons (concat "\\<" (regexp-opt
       '("assert" "assertEqual") t)
       "\\>")
       'font-lock-warning-face)

     ;; ----- Operators and symbols -----
     (cons ":=" 'font-lock-keyword-face)
     (cons "::" 'font-lock-keyword-face)
     (cons "++" 'font-lock-keyword-face)
     (cons "=>" 'font-lock-keyword-face)
     (cons "->" 'font-lock-keyword-face)
     (cons "\\.\\.\\." 'font-lock-keyword-face)
     ))
  "Subdued expressions to highlight in Egison modes.")

;; ============================================================
;; Font-lock keywords level 2 (gaudy - includes type annotations)
;; ============================================================

(defconst egison-font-lock-keywords-2
  (append egison-font-lock-keywords-1
   (eval-when-compile
     (list
      ;; Pattern variables ($x, $pat, etc.)
      (cons "\\$[a-zA-Z_][a-zA-Z0-9_']*" 'font-lock-variable-name-face)

      ;; Value patterns (#x, #(expr), etc.)
      (cons "#[a-zA-Z_][a-zA-Z0-9_']*" 'font-lock-constant-face)

      ;; Type class constraints in braces: {Eq a}, {Eq a, Ord b}
      (cons "{[A-Z][a-zA-Z0-9_',: ]*}" 'font-lock-type-face)

      ;; Type annotations in parameter list: (x: Integer), (x: a)
      (list "(\\([a-zA-Z_][a-zA-Z0-9_']*\\)\\s-*:" 1 'font-lock-variable-name-face)

      ;; Type names after colon in type annotations
      (list ":\\s-*\\([A-Z][a-zA-Z0-9_]*\\)" 1 'font-lock-type-face)

      ;; User-defined type names (uppercase identifiers not already matched)
      ;; in inductive/class/instance declarations
      (list "\\<\\(inductive\\|class\\|instance\\)\\s-+\\([A-Z][a-zA-Z0-9_]*\\)"
            2 'font-lock-type-face)

      ;; Data constructors in inductive definitions (after | at start of line)
      (list "^\\s-*|\\s-+\\([A-Z][a-zA-Z0-9_]*\\)" 1 'font-lock-constant-face)

      ;; "declare symbol" combination
      (list "\\<\\(declare\\)\\s-+\\(symbol\\)\\>"
            (list 1 'font-lock-keyword-face)
            (list 2 'font-lock-keyword-face))

      ;; "inductive pattern" combination
      (list "\\<\\(inductive\\)\\s-+\\(pattern\\)\\>"
            (list 1 'font-lock-type-face)
            (list 2 'font-lock-type-face))

      ;; "def pattern" combination
      (list "\\<\\(def\\)\\s-+\\(pattern\\)\\>"
            (list 1 'font-lock-keyword-face)
            (list 2 'font-lock-keyword-face))

      ;; Function name after "def" keyword
      (list "\\<def\\s-+\\([a-zA-Z_][a-zA-Z0-9_']*\\)" 1 'font-lock-function-name-face)

      ;; Index notation: subscripts and superscripts (e.g., v~i, v_j, T~i~j_k)
      (cons "[a-zA-Z0-9_'][~_][a-zA-Z0-9_']+" 'font-lock-preprocessor-face)
      )))
  "Gaudy expressions to highlight in Egison modes.")

(defvar egison-font-lock-keywords egison-font-lock-keywords-1
  "Default expressions to highlight in Egison modes.")

;; ============================================================
;; Indentation
;; ============================================================

(defun egison-indent-line ()
  "Indent current line as Egison code."
  (interactive)
  (let ((indent (egison-calculate-indent)))
    (when indent
      (save-excursion
        (beginning-of-line)
        (delete-horizontal-space)
        (indent-to indent))
      (when (< (current-column) indent)
        (move-to-column indent)))))

(defun egison-calculate-indent ()
  "Calculate the indentation level for the current line."
  (save-excursion
    (beginning-of-line)
    (cond
     ;; Top-level definitions
     ((looking-at "^\\(def\\|load\\|class\\|instance\\|inductive\\|declare\\|infixl\\|infixr\\|infix\\)\\>")
      0)
     ;; Match clause continuation (lines starting with |)
     ((looking-at "^\\s-*|")
      (save-excursion
        (forward-line -1)
        (cond
         ((looking-at "^\\s-*|")
          (current-indentation))
         ((looking-at ".*\\<with\\>\\s-*$")
          (+ (current-indentation) 2))
         ((looking-at ".*:=\\s-*$")
          (+ (current-indentation) 2))
         (t (current-indentation)))))
     ;; Lines after "where"
     ((save-excursion
        (forward-line -1)
        (looking-at ".*\\<where\\>\\s-*$"))
      (save-excursion
        (forward-line -1)
        (+ (current-indentation) 2)))
     ;; Default: match previous line
     (t
      (save-excursion
        (forward-line -1)
        (current-indentation))))))

;; ============================================================
;; Keymap
;; ============================================================

(defvar egison-mode-map
  (let ((smap (make-sparse-keymap)))
    (define-key smap "\C-j" 'newline-and-indent)
    smap)
  "Keymap for Egison mode.")

;; ============================================================
;; Syntax table
;; ============================================================

(defvar egison-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Block comments: {- ... -}
    (modify-syntax-entry ?\{  "(}1nb" table)
    (modify-syntax-entry ?\}  "){4nb" table)
    (modify-syntax-entry ?-  "_ 123" table)
    (modify-syntax-entry ?\n ">" table)

    ;; String literals
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "\"" table)

    ;; Operators that are part of words
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?~ "w" table)

    ;; Special symbols
    (modify-syntax-entry ?$ "'" table)
    (modify-syntax-entry ?# "'" table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?! "." table)
    (modify-syntax-entry ?? "." table)
    (modify-syntax-entry ?@ "." table)

    table)
  "Syntax table for Egison mode")

;; ============================================================
;; Mode setup
;; ============================================================

(defun egison-mode-set-variables ()
  (set-syntax-table egison-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults)
       '((egison-font-lock-keywords
          egison-font-lock-keywords-1 egison-font-lock-keywords-2)
         nil t
         ;; Include special characters and mathematical symbols as word constituents
         (("+*/=!?%:_~.'∂∇αβγδεζηθικλμνξοπρςστυφχψωΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ" . "w"))
         ))
  (set (make-local-variable 'indent-line-function) 'egison-indent-line)

  ;; Comment settings for -- and {- -}
  (set (make-local-variable 'comment-start) "-- ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "{-+ *\\|--+ *")
  (set (make-local-variable 'comment-add) 1)
  (set (make-local-variable 'comment-end-skip) nil)

  ;; Block comment delimiters
  (set (make-local-variable 'comment-multi-line) t)
  )


;;;###autoload
(defun egison-mode ()
  "Major mode for editing Egison code.

Features:
  - Syntax highlighting for Egison keywords, type annotations,
    type class definitions, inductive types, and pattern matching.
  - Support for type system keywords: class, instance, inductive,
    extends, declare, pattern.
  - Highlighting for pattern variables ($x), value patterns (#x),
    type constraints ({Eq a}), and type annotations (x: Integer).
  - Basic indentation support.
  - Comment support for line comments (--) and block comments ({- -}).

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
