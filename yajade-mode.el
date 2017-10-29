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

(defvar yajade-tab-width)

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
        (modify-syntax-entry ?_ "_" table)    ; _    is part of a symbol
        (modify-syntax-entry ?- "_" table)    ; -    is part of a symbol
        ;;;;(modify-syntax-entry ?/ "< 12" table) ; //   is begin of comment
        (modify-syntax-entry ?/ ". 12" table) ; //   is begin of comment
        (modify-syntax-entry ?\n "> " table)   ; \n   is end of comment
        ;;(modify-syntax-entry ?  "' 1" table)   ; |    in begin of line is string
        ;;(modify-syntax-entry ?| "' " table)   ; |    in begin of line is string
        ;;(modify-syntax-entry ?   "< 1c" table)
        ;;(modify-syntax-entry ?|  "< 2c" table)
        ;;(modify-syntax-entry ?\n "> c" table)

        table)
      )

(defconst yajade-keywords
  (eval-when-compile
    (regexp-opt
     '("if" "else" "for" "in" "each" "case" "when" "default" "block" "extends" "var"
       "block append" "block prepend" "append" "prepend"
       "include" "yield" "mixin") 'words))
  "Yajade keywords.")

(defvar yajade-tag-re "^ *\\([A-z][A-z0-9-_:]*\\)[( .#\n]")
(defvar yajade-nested-tag-re "^ *\\(?:[A-z][A-z0-9-]*\\): *\\([A-z][A-z0-9-_:]*\\)*?(") ;  a: span()  [TODO] unused. I think this should not be done with pure regexp.
(defvar yajade-id-re "^ *\\(?:[A-z0-9._:-]*\\)?\\(#[a-zA-Z_-][0-9a-zA-Z_-]*\\)")
(defvar yajade-class-re "^ *\\(?:[#A-z0-9_-]*\\)?\\([.][a-zA-Z][0-9a-zA-Z_.-]*\\)")
(defvar yajade-mixin-re "^ *[+][a-zA-Z][0-9a-zA-Z_-]*")
(defvar yajade-tag-declaration-char-re "[-a-zA-Z0-9_.#+]")
(defvar yajade-attr-re "\\([A-z_:.@-][A-z0-9_:.@-]*\\) *?= *?['\".0-9-A-z(]")

(defvar yajade-font-lock-keywords
  `(
    (,yajade-attr-re 1 font-lock-variable-name-face) ; order is significant. Don't move it unless you've tested it.
    ;; (yajade--font-lock-attr 1 font-lock-variable-name-face)
    ("<.+?>" . font-lock-function-name-face)
    (,yajade-tag-re 1 font-lock-function-name-face)
    (,yajade-double-tag-re 1 font-lock-function-name-face)
    (,yajade-keywords 0 font-lock-keyword-face)
    (,yajade-class-re 1 font-lock-type-face t)
    (,yajade-id-re 1 font-lock-keyword-face t)
    (,yajade-mixin-re 0 font-lock-constant-face)
    ("^ *mixin" 0 font-lock-keyword-face t)
    ("^ *mixin +\\([A-z_-][A-z0-9_-]*\\)" 1 font-lock-constant-face t)
    ;; ("disabled" 0 font-lock-warning-face)
    ("\\(?:false\\|null\\|true\\|undefined\\)" 0 font-lock-constant-face)
    ("[-+]?\\(?:[0-9]+[.][0-9]+\\|[.]?[0-9]+\\)" 0 font-lock-constant-face)
    ("^ *\\([=-]\\)" 0 font-lock-preprocessor-face)
    ("^ *\\(|.+\\)" 1 font-lock-string-face t) ;  plain text (start with /^ *\|/)
    ("^ *[-]//\\(.+\\)" 1 font-lock-comment-face t)
    ("^!!!\\|doctype[ ]?.*" 0 font-lock-comment-face t)
    (yajade--font-lock-remove-highlights-in-plain-text 1 nil t)
    ("#{.+?}" 0 font-lock-preprocessor-face t)
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
  (if (null (re-search-forward "^\\(\\)" limit :no-error))  ; if cannot goto next line. limit is important! so don't just forward-line
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

(defun yajade-region-for-sexp ()
  "Selects the current sexp as the region"
  (interactive)
  (beginning-of-line)
  (let ((ci (current-indentation)))
    (push-mark nil nil t)
    (while (> (yajade-next-line-indentation) ci)
      (forward-line)
      (end-of-line))))

(defun yajade-indent ()
  "Indent current region or line.
Calls `yajade-indent-region' with an active region or `yajade-indent-line'
without."
  (interactive)
  (if (region-active-p)
      (yajade-indent-region

       ;; use beginning of line at region-beginning
       (save-excursion
         (goto-char (region-beginning))
         (line-beginning-position))

       ;; use end of line at region-end
       (save-excursion
         (goto-char (region-end))
         (line-end-position)))
    (yajade-indent-line)))

(defun yajade-indent-line ()
  "Indent current line of jade code.
If the cursor is left of the current indentation, then the first call
will simply jump to the current indent. Subsequent calls will indent
the current line by `yajade-tab-width' until current indentation is
nested one tab-width deeper than its parent tag. At that point, an
additional call will reset indentation to column 0."
  (interactive)
  (let ((left-of-indent (>= (current-column) (current-indentation)))
        (indent (yajade-calculate-indent-target)))
    (if left-of-indent

        ;; if cursor is at or beyond current indent, indent normally
        (indent-line-to indent)

      ;; if cursor is trailing current indent, first indentation should
      ;; jump to the current indentation column (subsequent calls
      ;; will indent normally)
      (indent-line-to (current-indentation)))))

(defun yajade-indent-region (start end)
  "Indent active region according to indentation of region's first
line relative to its parent. Keep region active after command
terminates (to facilitate subsequent indentations of the same region)"
  (interactive "r")
  (save-excursion

    ;; go to start of region so we can find out its target indent
    (goto-char start)

    ;; keep region active after command
    (let* ((deactivate-mark)

           ;; find indent target for first line
           (first-line-indent-target (yajade-calculate-indent-target))

           ;; use current-indentation to turn target indent into
           ;; a relative indent to apply to each line in region
           (first-line-relative-indent
            (- first-line-indent-target (current-indentation))))

      ;; apply relative indent
      (indent-rigidly start end first-line-relative-indent))))

(defun yajade-calculate-indent-target ()
  "Return the column to which the current line should be indented."
  (let ((max-indent (+ (yajade-previous-line-indentation) yajade-tab-width)))
    (if (>= (current-indentation) max-indent) ;; if at max indentation
        0
      (+ (current-indentation) yajade-tab-width))))

(defun yajade-unindent ()
  "Unindent active region or current line."
  (interactive)
  (if (region-active-p)
      (yajade-unindent-region

       ;; use beginning of line at region-beginning
       (save-excursion
         (goto-char (region-beginning))
         (line-beginning-position))

       ;; use end of line at region-end
       (save-excursion
         (goto-char (region-end))
         (line-end-position)))

    ;; when no region is active
    (yajade-unindent-line)))

(defun yajade-unindent-line ()
  "Unindent line under point by `yajade-tab-width'.
Calling when `current-indentation' is 0 will have no effect."
  (indent-line-to
   (max
    (- (current-indentation) yajade-tab-width)
    0)))

(defun yajade-unindent-region (start end)
  "Unindent active region by `yajade-tab-width'.
Follows indentation behavior of `indent-rigidly'."
  (interactive "r")
  (let (deactivate-mark)
    (indent-rigidly start end (- yajade-tab-width))))

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

(defun yajade-next-line-indentation ()
  "Get the indentation of the next (non-blank) line (from point)."
  (interactive)
  (save-excursion

    ;; move down to the next non-blank line (or buffer end)
    (while (progn ;; progn used to get do...while control flow
             (forward-line 1)
             (and (yajade-blank-line-p) (not (= (point-at-eol) (point-max))))))
    (let ((next-line-indent (current-indentation)))
      next-line-indent)))

(defun yajade-newline-and-indent ()
  "Insert newline and indent to parent's indentation level."
  (interactive)
  (newline)
  (indent-line-to (max (yajade-previous-line-indentation) 0)))

(defun yajade-fontify-region-as-js (beg end)
  "Fontify a region between BEG and END using js-mode fontification.
Inspired by (read: stolen from) from `haml-mode'. Note the clever use
of `narrow-to-region' by the author of `haml-mode' to keep syntactic
highlighting (maybe other things too?) from looking beyond the
region defined by BEG and END."
  (save-excursion
    (save-match-data
      (let ((font-lock-keywords js--font-lock-keywords-3)
            (font-lock-syntax-table js-mode-syntax-table)
            (font-lock-syntactic-keywords nil)
            (syntax-propertize-function nil)
            (font-lock-multiline 'undecided)
            (font-lock-dont-widen t)
            font-lock-keywords-only
            font-lock-extend-region-functions
            font-lock-keywords-case-fold-search)
        (when (and (fboundp 'js--update-quick-match-re) (null js--quick-match-re-func))
          (js--update-quick-match-re))
        (save-restriction
          (narrow-to-region beg end)
          (font-lock-fontify-region beg end))))))

(defvar yajade-mode-map (make-sparse-keymap))

;; mode declaration
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
  (set (make-local-variable 'indent-line-function) 'yajade-indent-line)
  (set (make-local-variable 'indent-region-function) 'yajade-indent-region)
  (set (make-local-variable 'indent-tabs-mode) nil)

  ;; keymap
  (use-local-map yajade-mode-map)

  ;; modify the keymap
  (define-key yajade-mode-map [remap comment-dwim] 'yajade-comment-dwim)
  (define-key yajade-mode-map [tab] 'yajade-indent)
  (define-key yajade-mode-map [backtab] 'yajade-unindent)
  (define-key yajade-mode-map (kbd "RET") 'yajade-newline-and-indent)

  ;; highlight keywords, ignore syntactic font-lock
  (setq font-lock-defaults '(yajade-font-lock-keywords nil nil)))


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.jade\\'" . yajade-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pug\\'" . yajade-mode))


(provide 'yajade-mode)
;;; yajade-mode.el ends here
