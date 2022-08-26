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

    ;; Comments
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23n"  table)
    (modify-syntax-entry ?\n "> b"    table)
    (modify-syntax-entry ?\^m "> b"   table)

    table))

(defun move-is-lt-char-operator ()
  (let ((continue t) (lt nil))
    (while continue
      (forward-char)
      (skip-chars-forward "[:space:]\n")
      (cond 
       ((= (following-char) ?>)
	 (setq continue nil)
	 (setq lt nil))
       ((memq (following-char) (string-to-list "[-=!%&*/<>[{(|.^;)}]"))
	 (setq continue nil)
	 (setq lt t))))
    lt))

(defun move-is-gt-char-operator ()
  (let ((continue t) (gt nil))
    (while continue
      (backward-char)
      (skip-chars-backward "[:space:]\n")
      (cond
       ((= (following-char) ?<)
	(setq continue nil)
	(setq gt nil))
       ((memq (following-char) (string-to-list "[-=!%&*/<>[{(|.^;)}]"))
	(setq continue nil)
	(setq gt t))))
    gt))



(defun move-is-lt-pprint ()
  (interactive)
  (message "%s" (move-is-gt-char-operator)))

(defun move-ordinary-lt-gt-p ()
  (save-match-data
    (save-excursion
      (goto-char (match-beginning 0))
      (cond
       ((move-in-string-or-comment-p) t)
       ((= (following-char) ?<)
	(move-is-lt-char-operator))
       ((= (following-char) ?>)
	(move-is-gt-char-operator))
       ))))

(defun move-syntax-propertize (start end)
  (goto-char start)
  (funcall
   (syntax-propertize-rules
    ("<"
     (0 (ignore
	 (when (move-ordinary-lt-gt-p)
	   (put-text-property (match-beginning 0) (match-end 0)
			      'syntax-table (string-to-syntax "."))
	   (goto-char (match-end 0))))))
    (">"
     (0 (ignore
	 (when (move-ordinary-lt-gt-p)
	   (put-text-property (match-beginning 0) (match-end 0)
			      'syntax-table (string-to-syntax "."))
	   (goto-char (match-end 0)))))))
   
   (point) end))


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
    "const" "acquires" "while"
    "loop" "spec" "if"
    "true" "else" "false"))


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


(defmacro move-paren-level ()
  `(car (syntax-ppss)))

(defmacro move-in-string-or-comment-p ()
  `(nth 8 (syntax-ppss)))

(defmacro move-in-string-p ()
  `(nth 3 (syntax-ppss)))

(defmacro go-in-comment-p ()
  `(nth 4 (syntax-ppss)))

(defun move-line-paren-level ()
  (interactive)
  (save-excursion
    (let ((left nil) (right nil))
      (beginning-of-line)
      (setq left (move-paren-level))
      (end-of-line)
      (setq right (move-paren-level))
      (min left right))))

(defun move-line-paren-level-pprint ()
  (interactive)
  (message "%s" (move-line-paren-level)))


(defun move-mode-indent-line ()
  (interactive)
  (let ((origin-paren-level (move-line-paren-level))
	(not-indented t)
	(cur-indent 0))
    (save-excursion
    (while not-indented
      (forward-line -1)
      (if (bobp)
	(setq not-indented nil)
	(if (>= (move-line-paren-level) origin-paren-level)
	  nil
	  (progn
	    (setq cur-indent (+ move-indent-offset (current-indentation)))
	    (setq not-indented nil))))
      ))
    (back-to-indentation)
    (indent-to cur-indent)))

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
(defconst move-identifier-regexp "[[:word:][:multibyte:]]+")
(defconst move-re-generic
  (concat "<[[:space:]]*'" move-re-ident "[[:space:]]*>"))



(defun move-re-grab (inner) (concat "\\(" inner "\\)"))
(defun move-re-word (inner) (concat "\\<" inner "\\>"))
(defun move-re-shy (inner) (concat "\\(?:" inner "\\)"))

(defun move-re-item-def (itype)
  (concat (move-re-word itype)
          (move-re-shy move-re-generic) "?"
          "[[:space:]]+" (move-re-grab move-re-ident)))


(defun move-mode-syntactic-face-function (state)
  "Return face that distinguishes doc and normal comments in given syntax STATE."
  (if (nth 3 state)
      'font-lock-string-face
    (save-excursion
      (goto-char (nth 8 state))
      (if (looking-at "/\\([*][*!][^*!]\\|/[/!][^/!]\\)")
          'font-lock-doc-face
        'font-lock-comment-face))))


(defvar move-font-lock-keywords
  (append `(
	       (,(regexp-opt move-keywords 'symbols) . font-lock-keyword-face)
	       (,(regexp-opt move-special-types 'symbols) . font-lock-type-face)
	       (,move-re-type-or-constructor . font-lock-type-face)
	       (,(concat "\\_<\\(?:let\\s-+ref\\|let\\|ref\\|for\\)\\s-+\\(?:mut\\s-+\\)?"
			 (move-re-grab move-re-ident)
			 "\\_>")
		. font-lock-variable-name-face)
	       (,(move-path-font-lock-matcher move-re-uc-ident) . font-lock-type-face)
	       )
	  (mapcar #'(lambda (x) (list (move-re-item-def (car x)) 1 (cdr x)))
	  '(("module" . font-lock-type-face)
	    ("fun" . font-lock-function-name-face)))))



(define-derived-mode move-mode prog-mode "Move"
  "Major mode for Move code.

\\{move-mode-map}"
  :group 'move-mode
  :syntax-table move-mode-syntax-table

  (setq-local syntax-propertize-function #'move-syntax-propertize)
  (setq-local indent-line-function 'move-mode-indent-line)
  (setq-local comment-start "// ")
  (setq-local comment-end  "")
  (setq-local font-lock-defaults '(move-font-lock-keywords
				   nil nil nil nil
				   (font-lock-syntactic-face-function
				    . move-mode-syntactic-face-function)))
  (setq-local paragraph-separate paragraph-start)
  (setq prettify-symbols-alist rust-prettify-symbols-alist)
  (setq-local tab-width 4)
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.move\\'" . move-mode))



(provide 'move-mode)
