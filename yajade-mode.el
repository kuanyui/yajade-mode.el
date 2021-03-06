;;; yajade-mode.el --- Major mode for editing Jade / Pug files      -*- lexical-binding: t; -*-
;;
;;; Copyright (c) 2017 ono hiroko
;;; Copyright (c) 2014 - 2016 Brian M. Carlson

;; Author: ono hiroko (kuanyui.github.io)
;; Keywords: languages

;;; Forked from Brian M. Carlson's jade-mode:
;;; https://github.com/brianc/jade-mode
;;;
;;; The MIT License (MIT)

;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;;; THE SOFTWARE.

;;; Commentary:

;;

;;; Code:

(require 'font-lock)

(require 'js)

(defvar yajade-tab-width 2)

(defun yajade-debug (string &rest args)
  "Prints a debug message"
  (apply 'message (append (list string) args)))

(defmacro yajade-line-as-string ()
  "Returns the current line as a string."
  `(buffer-substring-no-properties (point-at-bol) (point-at-eol)))

(defun yajade-empty-line-p ()
  "If line is empty or not."
  (= (point-at-eol) (point-at-bol)))

(defun yajade-blank-line-p ()
  "Returns t when line contains only whitespace chars, nil otherwise."
  (string-match-p "^\\s-*$" (yajade-line-as-string)))

(defun yajade-comment-dwim (arg)
  "Comment or uncomment current line or region in a smart way."
  (interactive "*P")
  (require 'newcomment)
  (let ((start (if (region-active-p)

                   ;; when region is active, use beginning of line at
                   ;; beginning of region (this way we don't start
                   ;; commenting in the middle of a line)
                   (progn
                     (save-excursion
                       (goto-char (region-beginning))
                       (point-at-bol)))

                 ;; without a region, just use beginning of current line
                 (point-at-bol)))

        ;; same logic applies for end of line/region
        (end (if (region-active-p)
                 (progn
                   (save-excursion
                     (goto-char (region-end))
                     (point-at-eol)))
               (point-at-eol))))

    ;; once we pick good values for start/end of region, simply use
    ;; `comment-or-uncomment-region' from `newcomment' lib, and skip
    ;; to next line for convenience
    (comment-or-uncomment-region start end)
    (forward-line)))

(setq yajade-syntax-table
      (let ((table (make-syntax-table)))
        (modify-syntax-entry ?\" "\"" table)
        (modify-syntax-entry ?\' "\"" table)
        (modify-syntax-entry ?\` "\"" table)
        (modify-syntax-entry ?= "." table)        ; =    is not a part of a symbol (Don't use -, it will be deleted by `delete-trailing-whitespace')
        (modify-syntax-entry ?$ "." table)        ; $    is not a part of a symbol
        (modify-syntax-entry ?+ "." table)        ; +    is not a part of a symbol
        (modify-syntax-entry ?_ "_" table)        ; _    is part of a symbol
        (modify-syntax-entry ?- "_" table)        ; -    is part of a symbol
        ;;;;(modify-syntax-entry ?/ "< 12" table) ; //   is begin of comment
        (modify-syntax-entry ?/ ". 12" table)     ; //   is begin of comment
        (modify-syntax-entry ?\n "> " table)      ; \n   is end of comment
        ;;(modify-syntax-entry ?  "' 1" table)    ; |    in begin of line is string
        ;;(modify-syntax-entry ?| "' " table)     ; |    in begin of line is string
        ;;(modify-syntax-entry ?   "< 1c" table)
        ;;(modify-syntax-entry ?|  "< 2c" table)
        ;;(modify-syntax-entry ?\n "> c" table)

        table)
      )

(defconst yajade-keywords
  (eval-when-compile
    (regexp-opt
     '("if" "else" "for" "in" "each" "case" "when" "default" "block" "extends" "var" "while"
       "block append" "block prepend" "append" "prepend"
       "include" "yield" "mixin") 'words))
  "Yajade keywords.")

(setq yajade-tag-re "\\(?:^ *\\|#[[]\\)\\([A-z][A-z0-9-_:]*\\)[( .#\n]")

;;[TODO] unused. I think this should not be done with pure regexp.
;; However I have no interest to solve this currently because font-lock's API is so shitty.
(defvar yajade-nested-tag-re "^ *\\(?:[A-z][A-z0-9-]*\\): *\\([A-z][A-z0-9-_:]*\\)*?(") ;  a: span()

(setq yajade-id-re "\\(?:^ *\\|#[[]\\)\\(?:[A-z0-9._:-]*\\)?\\(#[a-zA-Z_-][0-9a-zA-Z_-]*\\)")
(setq yajade-class-re "\\(?:^ *\\|#[[]\\)\\(?:[#A-z0-9_-]*\\)?\\([.][a-zA-Z][0-9a-zA-Z_.-]*\\)")
(setq yajade-mixin-re "\\(?:^ *\\|#[[]\\)[+][a-zA-Z_][0-9a-zA-Z_-]*")
(setq yajade-tag-declaration-char-re "[-a-zA-Z0-9_.#+]")
(setq yajade-attr-re "\\([A-z_:.@-][A-z0-9_:.@-]*\\) *?= *?['\".0-9-A-z(]")

(setq yajade-font-lock-keywords
      `(
        (,yajade-keywords 0 font-lock-keyword-face)
        (,yajade-attr-re 1 font-lock-variable-name-face t) ; order is significant. Don't move it unless you've tested it.
        ("var +\\([A-Za-z0-9_]+\\)" 1 font-lock-variable-name-face t)
        ;; (yajade--font-lock-attr 1 font-lock-variable-name-face)
        ("&attributes" 0 font-lock-preprocessor-face)
        ("<.+?>" . font-lock-function-name-face)
        (,yajade-tag-re 1 font-lock-function-name-face)
        (,yajade-class-re 1 font-lock-type-face t)
        (,yajade-id-re 1 font-lock-keyword-face t)
        (,yajade-mixin-re 0 font-lock-preprocessor-face)
        ("^ *mixin" 0 font-lock-keyword-face t)
        ("^ *mixin +\\([A-z_-][A-z0-9_-]*\\)" 1 font-lock-preprocessor-face t)
        ("disabled" 0 font-lock-warning-face)
        ("\\(?:false\\|null\\|true\\|undefined\\)" 0 font-lock-constant-face)
        ("[-+]?\\(?:[0-9]+[.][0-9]+\\|[.]?[0-9]+\\)" 0 font-lock-constant-face)
        ("^ *\\([=-]\\)" 0 font-lock-preprocessor-face)
        ("^ *\\(|.+\\)" 1 font-lock-string-face t) ;  plain text (start with /^ *\|/)
        ("^ *[-]//\\(.+\\)" 1 font-lock-comment-face t)
        ("^!!!\\|doctype[ ]?.*" 0 font-lock-comment-face t)
        ;;(yajade--font-lock-remove-highlights-in-plain-text 1 nil t)  ;; this will cause mmm-mode error. Fuck.
        ("[#$]{.+?}" 0 font-lock-preprocessor-face t)
        ))

(defun yajade--font-lock-attr (limit)
  ;; See https://www.gnu.org/software/emacs/manual/html_node/elisp/Search_002dbased-Fontification.html
  ;; all following (`looking-at' "\\(\\)") is to return some empty match data to appease the font-lock gods
  "The behavior of font-lock callback is ridiculously shitty. Reserve this shit function as memories."
  (if (not (re-search-forward yajade-attr-re limit :no-error))
      nil  ; nil =====>  stop font-lock loop
    (let ((in-string (nth 3 (syntax-ppss)))
          (in-paren-depth  (nth 0 (syntax-ppss))))
      (if (or in-string
              (not (eq 1 in-paren-depth)))
          (looking-at "\\(\\)"))
      t))) ; continue next font-lock loop

(defun yajade--font-lock-remove-highlights-in-plain-text (limit)
  ;; See https://www.gnu.org/software/emacs/manual/html_node/elisp/Search_002dbased-Fontification.html
  ;; all following (looking-at "\\(\\)") is to return some empty match data to appease the font-lock gods
  "<ex>
  tag-name.class-name(attr='attrValue') plain text
"
  (interactive)
  (if (or (eobp)
          (null (re-search-forward "^\\(\\)" limit :no-error)))  ; if cannot goto next line. limit is important! so don't just forward-line
      nil  ; nil =====>  stop font-lock loop
    (progn
      (let* ((elem (substring-no-properties (thing-at-point 'line t) (current-indentation) -1))
             (elem-from (progn (back-to-indentation) (point))) ; always goto indentation of this line
             (elem-to   (progn (end-of-line) (point))))
        (back-to-indentation)
        (if (or (> (car (syntax-ppss)) 0) ; if in a parentheses
                (eq ?= (char-after))  ;if the first char is any of following
                (eq ?/ (char-after))
                (eq ?- (char-after))
                (eq ?| (char-after))
                (eq ?\n (char-after))
                (eq 32 (char-after)))
            (progn (looking-at "\\(\\)") t)  ; t =======> continue font-lock loop
          (progn
            (while (if (and (not (eolp))
                            (not (eq ?= (char-after)))
                            (not (eq ?\( (char-after)))
                            (or (and (eq 32 (char-after)) (eq ?: (char-before)))  ; a: span()
                                (and (not (eq 32 (char-after))) (not (eq ?\( (char-after)))))) ; not space && not (
                       (progn (right-char) t)  ; continue while
                     nil))  ; stop while
            (if (or (eolp)
                    (eq ?= (char-after)))
                (progn (looking-at "\\(\\)") t) ; t ========> continue font-lock loop
              (progn
                (when (eq ?\( (char-after))  ; point at a paren's beginning
                  (forward-sexp)
                  (setq elem-to (save-excursion (end-of-line) (point))))
                (re-search-forward "\\(.*\\)$" elem-to :no-error)
                t)  ; t ========> continue font-lock loop
              )))))))

(defun yajade-goto-end-of-tag ()
  "Skip ahead over whitespace, tag characters (defined in
`yajade-tag-declaration-char-re'), and paren blocks (using
`forward-sexp') to put point at the end of a full tag declaration (but
before its content). Use when point is inside or to the left of a tag
declaration"
  (interactive)
  ;; skip indentation characters
  (while (looking-at "[ \t]")
    (forward-char 1))

  (while (looking-at yajade-tag-declaration-char-re)
    (forward-char 1))
  (if (looking-at "(")
      (forward-sexp 1)))

(defun yajade-in-parentheses (&optional point)
  "If cursor is in parentheses, return the position of left
parentheses. Otherwise, return nil"
  (let ((pos-list (nth 9 (syntax-ppss point))))
    (if pos-list
        (car (last pos-list)))))

(defun yajade-get-tag-indentation (&optional point)
  "Deal with a nested tag like this:
     hello(foo='aaa'
           bar='bbb')
   Return the indentation of <hello>"
  (save-excursion
    (if point (goto-char point))
    (forward-line -1)
    (while (yajade-empty-line-p) (forward-line -1))
    (let (stop left-paren-pos)
      (while (not stop)
        (back-to-indentation)
        (setq left-paren-pos (yajade-in-parentheses))
        (if left-paren-pos
            (goto-char left-paren-pos)
          (setq stop t)))
      (current-indentation))))

(defun yajade-get-attributes-indentation (&optional point)
  "Two conditions:
  [Condition A]
  hello(
    foo='aaa'
    bar='bbb'
    world='ccc')
  [Condition B]
  hello(foo='aaa'
        bar='bbb'
        world='ccc')
"
  (save-excursion
    (if point (goto-char point))
    (let* ((left-paren-pos (yajade-in-parentheses))
           (first-char-pos (1+ left-paren-pos))
           (first-char (char-after first-char-pos)))
      (if (not left-paren-pos)
          0
        (if (eq first-char (string-to-char "\n"))
            (+ yajade-tab-width (yajade-get-tag-indentation))  ; Condition A
          (yajade-get-point-column first-char-pos) ; Condition B
          )))))

(defun yajade-get-max-indentation (&optional point)
  (+ yajade-tab-width (yajade-get-tag-indentation point)))

(defun yajade-get-point-indentation (&optional point)
  (save-excursion (goto-char (or point (point)))
                  (current-indentation)))

(defun yajade-get-point-column (&optional point)
  (save-excursion (goto-char (or point (point)))
                  (current-column)))

(defun yajade-two-points-in-same-line (p1 p2)
  (eq (line-number-at-pos p1) (line-number-at-pos p2)))

(defun yajade-indent ()
  (interactive)
  (let* ((left-paren-pos (yajade-in-parentheses))
         (at-first-line (yajade-two-points-in-same-line left-paren-pos (point))))
    (if (and left-paren-pos            ; if cursor is in a parentheses
             (not at-first-line))            ; not at first line of a tag
        (indent-line-to (yajade-get-attributes-indentation))
      (indent-line-to (yajade-correct-indentation (min (+ yajade-tab-width (current-indentation))
                                                       (yajade-get-max-indentation)))))))

(defun yajade-correct-indentation (indent)
  (- indent (% indent yajade-tab-width)))

(defun yajade-unindent ()
  "Unindent active region or current line."
  (interactive)
  (indent-line-to (yajade-correct-indentation
                   (max 0 (- (current-indentation) yajade-tab-width)))))

(defun yajade-backspace ()
  "More convenient; and Shift-TAB won't work on new Konsole."
  (interactive)
  (if (and (< (yajade-get-point-column) (yajade-get-point-indentation))
           (not (bolp)))
      (indent-line-to (yajade-get-point-indentation)))
  (if (and (> (yajade-get-point-column) 0)
           (eq (yajade-get-point-column) (yajade-get-point-indentation)))
      (yajade-unindent)
    (call-interactively #'backward-delete-char-untabify)))

(defun yajade-previous-line-indentation ()
  "Get the indentation of the previous (non-blank) line (from point)."
  (interactive)
  (save-excursion
    ;; move up to the nearest non-blank line (or buffer start)
    (while (progn ;; progn used to get do...while control flow
             (forward-line -1)
             (and (yajade-blank-line-p) (not (= (point-at-bol) (point-min))))))
    (let ((prev-line-indent (current-indentation)))
      prev-line-indent)))

(defun yajade-newline-and-indent ()
  "Insert newline and indent to parent's indentation level."
  (interactive)
  (newline)
  (if (yajade-in-parentheses)
      (progn
        (indent-line-to (yajade-get-attributes-indentation))
        (message "%s" (yajade-get-attributes-indentation)))
    (indent-line-to (yajade-get-tag-indentation))))

(defvar yajade-mode-map (make-sparse-keymap))

;;;###autoload
(define-derived-mode yajade-mode fundamental-mode
  "Yajade"
  "Major mode for editing jade node.js templates"
  :syntax-table yajade-syntax-table

  ;; turn off electric indent mode for yajade buffers (by default, at least)
  (when (fboundp 'electric-indent-local-mode)
    (electric-indent-local-mode 0))
  (setq mode-name "Yajade")
  (setq major-mode 'yajade-mode)

  ;; comment syntax
  (set (make-local-variable 'comment-start) "//- ")
  (set (make-local-variable 'comment-start-skip) "//-\\s-*")

  (set (make-local-variable 'yajade-tab-width) 2)
  (set (make-local-variable 'indent-line-function) 'yajade-indent)
  (set (make-local-variable 'indent-region-function) 'yajade-indent-region)
  (set (make-local-variable 'indent-tabs-mode) nil)

  ;; keymap
  (use-local-map yajade-mode-map)

  ;; modify the keymap
  (define-key yajade-mode-map [remap comment-dwim] 'yajade-comment-dwim)
  (define-key yajade-mode-map (kbd "TAB") 'yajade-indent)
  (define-key yajade-mode-map [backtab] 'yajade-unindent)
  (define-key yajade-mode-map (kbd "DEL") 'yajade-backspace)
  (define-key yajade-mode-map (kbd "RET") 'yajade-newline-and-indent)

  ;; highlight keywords, ignore syntactic font-lock
  (setq font-lock-defaults '(yajade-font-lock-keywords nil nil)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.jade\\'" . yajade-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pug\\'" . yajade-mode))


(provide 'yajade-mode)
;;; yajade-mode.el ends here
