;; See `tempo-define-template' for the syntax (use-package tempo)
fundamental-mode ;; Available everywhere

(today (format-time-string "%Y-%m-%d"))

lisp-mode emacs-lisp-mode ;; Specify multiple modes

(lambda "(lambda (" p ")" n> r> ")")

emacs-lisp-mode

(__ ";;; " (file-name-nondirectory (buffer-file-name)) " -*- lexical-binding: t; -*-" n
    (ignore (setq lexical-binding t)))
(pt "(point)")
(pkg ";;;###package")
(message "(message \"!! %s\" " r ")" q)
(lambda "(lambda (" p ")" n> r> ")")
(var "(defvar " p "\n  \"" p "\")")
(local "(defvar-local " p "\n  \"" p "\")")
(const "(defconst " p "\n  \"" p "\")")
(custom "(defcustom " p "\n  \"" p "\"" n> ":type '" p ")")
(face "(defface " p " '((t :inherit " p "))\n  \"" p "\")")
(group "(defgroup " p " nil\n  \"" p "\"" n> ":group '" p n> ":prefix \"" p "-\")")
(macro "(defmacro " p " (" p ")\n  \"" p "\"" n> r> ")")
(alias "(defalias '" p " '" p ")")
(fun "(defun " p " (" p ")\n  \"" p "\"" n> r> ")")
(iflet "(if-let (" p ")" n> r> ")")
(whenlet "(when-let (" p ")" n> r> ")")
(iflet* "(if-let* (" p ")" n> r> ")")
(whenlet* "(when-let* (" p ")" n> r> ")")
(andlet* "(and-let* (" p ")" n> r> ")")
(cond "(cond" n "(" q "))" >)
(pcase "(pcase " (p "scrutinee") n "(" q "))" >)
(let "(let (" p ")" n> r> ")")
(let* "(let* (" p ")" n> r> ")")
(rec "(letrec (" p ")" n> r> ")")
(dotimes "(dotimes (" p ")" n> r> ")")
(dolist "(dolist (" p ")" n> r> ")")
(loop "(cl-loop for " p " in " p " do" n> r> ")")
(command "(defun " p " (" p ")\n  \"" p "\"" n> "(interactive" p ")" n> r> ")")
(advice "(defun " (p "adv" name) " (&rest app)" n> p n> "(apply app))" n>
        "(advice-add #'" (p "fun") " " (p ":around") " #'" (s name) ")")
(provide "(provide '" (file-name-base (or (buffer-file-name) (buffer-name))) ")" n
         ";;; " (file-name-nondirectory (or (buffer-file-name) (buffer-name))) " ends here" n)
(letf "(cl-letf* ((this-fn (symbol-function '" (s fun) "))" n>
                 "((symbol-function '" (s fun) ")" n>
                  "(lambda (" p ")" n>
                    p ")))" n>
         r> ")")

sh-mode

(__ "#!/usr/bin/env bash" n)
(for "for " p " in " p "; do" n> r> n "done")
(if "if [ " p " ]; then" n> r> n "fi")

eshell-mode

(for "for " (p "i") " in " p " { " q " }")
(while "while { " p " } { " q " }")
(until "until { " p " } { " q " }")
(if "if { " p " } { " q " }")
(ife "if { " p " } { " p " } { " q " }")
(unl "unless { " p " } { " q " }")
(unle "unless { " p " } { " p " } { " q " }")

ess-r-mode

(h1 "### " (r "Title") " ------" n q)
(h2 "## " (r "Title") " ------" n q)
(fun p " <- function(" p ") {" n> q n "}")
(timer "tstart <- Sys.time()" n r "print(Sys.time() - tstart)")

python-mode

(__ "#!/usr/bin/env python3" n)
(rule "rule " (s name) ":" n
      (_ 4) "input:"  n (_ 8) p n
      (_ 4) "output:" n (_ 8) p n
      (_ 4) "log:"    n (_ 8) d "logs/" (s name) ".log" d n
      (_ 4) "conda:"  n (_ 8) d "envs/" p ".yml" d n
      (_ 4) "threads: " p n
      (_ 4) "shell:"  n (_ 8) d q "2> {log}" d)

text-mode

(asciibox "+-" (make-string (length str) ?-) "-+" n
          "| " (s str)                       " |" n
          "+-" (make-string (length str) ?-) "-+" n)

org-mode

(__ "#+title: " (dtm-file-name-as-title) n)
(title "#+title: " p n "#+author: " p n "#+language: en_GB" n n)
(cap "#+label: " p n "#+caption: " r)
(begin "#+begin_" (s name) n> r> n> "#+end_" (s name))
(center "#+begin_center" n> r> n> "#+end_center")
(comment "#+begin_comment" n> r> n> "#+end_comment")
(quote "#+begin_quote" n> r> n> "#+end_quote")
(example "#+begin_example" n> r> n> "#+end_example")
(notes "#+begin_notes" n> r> n> "#+end_notes")
(src "#+begin_src " p n> r> n> "#+end_src" :post (org-edit-src-code))
(elisp "#+begin_src emacs-lisp" n> r> n "#+end_src" :post (org-edit-src-code))
(attr "#+attr_html: ")
(style (i attr) ":style " r)
(class (i attr) ":class " r)
(2c "#+begin_leftcol" n> r> n> "#+end_leftcol" n n
    "#+begin_rightcol" n> q n> "#+end_rightcol")
(va "#+begin_valign-wrapper" n> r> n> "#+end_valign-wrapper")
(plotly "#+html: <div id=" d (s id) d "></div>" n
        "#+html: <script> myPlots.push({json:" n
        "#+include: " r " export html" n
        "#+html: ,id: " d (s id) d "})</script>" q)
(plotly-init "#+html: <script>var myPlots = [];</script>")
(read-tsv "#+begin_src R :colnames yes :rownames no :exports results :session none" n
          "readr::read_tsv("d r d")" n
          "#+end_src" n n "#+results:" n n)

latex-mode

(begin "\\begin{" (s env) "}" r> n> "\\end{" (s env) "}")
(frac "\\frac{" p "}{" q "}")
(enumerate "\\begin{enumerate}\n\\item " r> n> "\\end{enumerate}")
(itemize "\\begin{itemize}\n\\item " r> n> "\\end{itemize}")

markdown-mode

(src "```{" (p "r") _ (p "name") "}" n q n "```")

;; Local Variables:
;; mode: lisp-data
;; imenu-generic-expression: ((nil "^\\([A-za-z+][A-za-z +-]+\\)" 1))
;; End:
