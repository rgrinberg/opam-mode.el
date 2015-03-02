;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    The MIT License (MIT)    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Copyright (c) 2015 Julien Sagot <ju.sagot@gmail.com>

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;;;;;;;;;;;;;;;;;;;;;;
;;    Description    ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Comments and keywords (field labels) highlighting for opam files.

;; Sources and issue tracker: https://github.com/sagotch/opam-mode.el

;; Version: 0.1

;;;;;;;;;;;;;;;;;;;;;
;;    opam-mode    ;;
;;;;;;;;;;;;;;;;;;;;;

(defun mk-regexp (kws)
  "Build a regexp from a keyword list."
  (concat "\\<" (regexp-opt kws t) "\\>" ) )

(defconst opam-keywords
  (list (cons (mk-regexp
               '("authors" "available" "bug-reports"
                 "build" "build-doc" "build-test" "conflicts"
                 "depends" "depexts" "depopts"
                 "dev-repo" "doc" "homepage" "install"
                 "libraries" "license"
                 "maintainer" "messages" "name"
                 "ocaml-version" "opam-version"
                 "os" "patches" "post-messages"
                 "remove" "subst"
                 "syntax" "tags" "version" ) )
              font-lock-keyword-face) )
  "Keywords for `opam-mode'")

;; "src" "archive" "http" "local" "git" "darcs" "hg" "mirrors" "checksum"
(defconst opam-url-keywords
  (list (cons (mk-regexp
               '("src" "archive" "http" "local" "git"
                 "darcs" "hg" "mirrors" "checksum") )
              font-lock-keyword-face) )
  "Keywords for `opam-url-mode'")

(defvar opam-syntax-table
  (let ((st (make-syntax-table)))

    ;; # Until end of line
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)

    ;; (* OCaml-like comments, may be (* nested *) *)
    (modify-syntax-entry ?\( ". 1bn" st)
    (modify-syntax-entry ?* ". 23b" st)
    (modify-syntax-entry ?\) ". 4bn" st)

    ;; Make keywords parsing easier allowing '-' in words
    (modify-syntax-entry ?- "w" st)
    st )
  "Syntax table for `opam-mode' and `opam-url-mode'." )

;;;###autoload
(defun opam-mode ()
  "Major mode for editing opam files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table opam-syntax-table)
  (setq font-lock-defaults '(opam-keywords))
  (setq major-mode 'opam-mode)
  (setq mode-name "OPAM") )

;;;###autoload
(defun opam-url-mode ()
  "Major mode for editing opam url files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table opam-syntax-table)
  (setq font-lock-defaults '(opam-url-keywords))
  (setq major-mode 'opam-url-mode)
  (setq mode-name "OPAM") )

;;;###autoload
(add-to-list 'auto-mode-alist '("/opam\\'" . opam-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("/url\\'" . opam-url-mode))

(provide 'opam-mode)
