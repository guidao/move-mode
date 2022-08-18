;;; move-mode.el -- A major-mode for editing Move source code -*-lexical-binding: t-*-


;; Version: 0.0.1
;; Author: Guidao
;; Keywords: languages
;; Package-Requires: ((emacs "25.1"))


(eval-when-compile (require 'rx))


(defgroup move-mode nil
  "Support for Move code."
  :group ':languages)


(defcustom move-indent-offset 4
  "Indent Move code by this number of spaces."
  :type 'integer
  :group 'move-mode
  :safe #'integerp)


(defvar move-prettify-symbols-alist
  '(("&&" . ?∧) ("||" . ?∨)
    ("<=" . ?≤)  (">=" . ?≥) ("!=" . ?≠)
    ("INFINITY" . ?∞) ("->" . ?→) ("=>" . ?⇒))
  "Alist of symbol prettifications used for `prettify-symbols-alist'.")


(defvar move-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Operators
    (dolist (i '(?+ ?- ?* ?/ ?% ?& ?| ?^ ?! ?< ?> ?~ ?@))
      (modify-syntax-entry i "." table))

    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)

    ;; Angle brackets.
    (modify-syntax-entry ?< "(>" table)
    (modify-syntax-entry ?> ")<" table)

    table))


(defconst move-keywords
  '("break" "box" "continue"
    "crate" "extern" "fun"
    "public" "entry" "module"
    "impl" "let"  "macro"
    "pub" "return" "yield"
    "super" "where" "unsafe"
    "use" "in" "type" "enum"
    "struct" "union" "as"
    "existential" "mod" "trait"
    "move" "mut" "ref"
    "static" "const" "await"
    "const"))


(defconst move-special-types
  '("isize" "usize" "char"
    "bool" "u8" "u16" "u32"
    "u64" "u128" "f32"
    "f64" "i8" "i16" "i32"
    "i64" "i128" "str"))

(defconst move-re-type-or-constructor
  (rx symbol-start
      (group upper (0+ (any word nonascii digit "_")))
      symbol-end))


(defun move-path-font-lock-matcher (re-ident)
  "Match occurrences of RE-IDENT followed by a double-colon.
Examples include to match names like \"foo::\" or \"Foo::\".
Does not match type annotations of the form \"foo::<\"."
  `(lambda (limit)
     (catch 'rust-path-font-lock-matcher
       (while t
         (let* ((symbol-then-colons (rx-to-string '(seq (group (regexp ,re-ident)) "::")))
                (match (re-search-forward symbol-then-colons limit t)))
           (cond
            ;; If we didn't find a match, there are no more occurrences
            ;; of foo::, so return.
            ((null match) (throw 'rust-path-font-lock-matcher nil))
            ;; If this isn't a type annotation foo::<, we've found a
            ;; match, so a return it!
            ((not (looking-at (rx (0+ space) "<")))
             (throw 'rust-path-font-lock-matcher match))))))))

(defconst move-re-uc-ident "[[:upper:]][[:word:][:multibyte:]_[:digit:]]*")
(defconst move-re-ident "[[:word:][:multibyte:]_][[:word:][:multibyte:]_[:digit:]]*")
(defconst move-re-lc-ident "[[:lower:][:multibyte:]_][[:word:][:multibyte:]_[:digit:]]*")

(defun move-re-grab (inner) (concat "\\(" inner "\\)"))

(defvar move-font-lock-keywords
  `(
    (,(regexp-opt move-keywords 'symbols) . font-lock-keyword-face)
    (,(regexp-opt move-special-types 'symbols) . font-lock-type-face)
    (,move-re-type-or-constructor . font-lock-type-face)
    (,(concat "\\_<\\(?:let\\s-+ref\\|let\\|ref\\|for\\)\\s-+\\(?:mut\\s-+\\)?"
               (move-re-grab move-re-ident)
               "\\_>")
     . font-lock-variable-name-face)
    (,(move-path-font-lock-matcher move-re-uc-ident) . font-lock-type-face)
    ))

(define-derived-mode move-mode prog-mode "Move"
  "Major mode for Move code.

\\{move-mode-map}"
  :group 'move-mode
  :syntax-table move-mode-syntax-table

  (setq-local comment-start "// ")
  (setq-local comment-end  "")
  (setq-local font-lock-defaults '(move-font-lock-keywords))
  (setq-local paragraph-separate paragraph-start)
  (setq prettify-symbols-alist rust-prettify-symbols-alist)
  (setq-local tab-width 4)
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.move\\'" . move-mode))



(provide 'move-mode)
