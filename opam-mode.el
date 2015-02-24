
;; Keyword list for opam file
;; "authors" "available" "bug-reports" "build" "build-doc"
;; "build-test" "conflicts" "depends" "depexts" "depopts"
;; "dev-repo" "doc" "homepage" "install" "libraries" "license"
;; "maintainer" "messages" "name" "ocaml-version" "opam-version"
;; "os" "patches" "post-messages" "remove" "version" "subst"
;; "syntax" "tags"
(defvar opam-keywords
  (list '("\\<\\(a\\(?:uthors\\|vailable\\)\\|bu\\(?:g-reports\\|ild\\(?:-\\(?:doc\\|test\\)\\)?\\)\\|conflicts\\|d\\(?:e\\(?:p\\(?:\\(?:e\\(?:nd\\|xt\\)\\|opt\\)s\\)\\|v-repo\\)\\|oc\\)\\|homepage\\|install\\|li\\(?:braries\\|cense\\)\\|m\\(?:aintainer\\|essages\\)\\|name\\|o\\(?:caml-version\\|pam-version\\|s\\)\\|p\\(?:\\(?:atch\\|ost-messag\\)es\\)\\|remove\\|s\\(?:ubst\\|yntax\\)\\|tags\\|version\\)\\>"
          . font-lock-keyword-face) )
  "Syntax highlighting for `opam-mode`")


;; Syntax table (for comments)
;; # until end of line
;; ocaml-like comments (* may be (* nested *) *)
(defvar opam-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\( ". 1bn" st)
    (modify-syntax-entry ?* ". 23b" st)
    (modify-syntax-entry ?\) ". 4bn" st)
    st )
  "Syntax table for `opam-mode'." )

(defun opam-mode ()
  "Major mode for editing opam files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table opam-syntax-table)
  (setq font-lock-defaults '(opam-keywords))
  (setq major-mode 'opam-mode)
  (setq mode-name "OPAM") )

(add-to-list 'auto-mode-alist '("opam$" . opam-mode))

(provide 'opam-mode)
