;;; Compiled snippets and support files for `python-mode'
;;; contents of the .yas-setup.el support file:
;;;
(require 'yasnippet)
(defvar yas-text)

(defun python-split-args (arg-string)
  "Split a python argument string into ((name, default)..) tuples"
  (mapcar (lambda (x)
             (split-string x "[[:blank:]]*=[[:blank:]]*" t))
          (split-string arg-string "[[:blank:]]*,[[:blank:]]*" t)))

(defun python-args-to-docstring ()
  "return docstring format for the python arguments in yas-text"
  (let* ((indent (concat "\n" (make-string (current-column) 32)))
         (args (python-split-args yas-text))
         (max-len (if args (apply 'max (mapcar (lambda (x) (length (nth 0 x))) args)) 0))
         (formatted-args (mapconcat
                (lambda (x)
                   (concat (nth 0 x) (make-string (- max-len (length (nth 0 x))) ? ) " -- "
                           (if (nth 1 x) (concat "\(default " (nth 1 x) "\)"))))
                args
                indent)))
    (unless (string= formatted-args "")
      (mapconcat 'identity (list "Keyword Arguments:" formatted-args) indent))))

(defun python-args-to-docstring-numpy ()
  "return docstring format for the python arguments in yas-text"
  (let* ((args (python-split-args yas-text))
         (format-arg (lambda(arg)
                       (concat (nth 0 arg) " : " (if (nth 1 arg) ", optional") "\n")))
         (formatted-params (mapconcat format-arg args "\n"))
         (formatted-ret (mapconcat format-arg (list (list "out")) "\n")))
    (unless (string= formatted-params "")
      (mapconcat 'identity
                 (list "\nParameters\n----------" formatted-params
                       "\nReturns\n-------" formatted-ret)
                 "\n"))))


;;; Snippet definitions:
;;;
(yas-define-snippets 'python-mode
                     '(("met"
                        "def ${1:method}(self$2) -> ${3:Res}:\n    $0"
                        "method" nil ("object oriented") nil
                        "/home/marce/.emacs.d/snippets/python-mode/method"
                        nil nil)
                       ("init" "def __init__(self$1):\n    $0" "init"
                        nil ("definitions") nil
                        "/home/marce/.emacs.d/snippets/python-mode/init"
                        nil nil)
                       ("ifm"
                        "if __name__ == '__main__':\n    ${1:main()}"
                        "ifmain" nil nil nil
                        "/home/marce/.emacs.d/snippets/python-mode/ifmain"
                        nil nil)
                       ("fd"
                        "def ${1:name}($2):\n \\\"\\\"\\\"$3\n ${2:$(python-args-to-docstring)}\n \\\"\\\"\\\"\n $0"
                        "function_docstring" nil ("definitions") nil
                        "/home/marce/.emacs.d/snippets/python-mode/function_docstring"
                        nil nil)
                       ("f" "def ${1:fun}(${2:args}):\n    $0\n"
                        "function" nil ("definitions") nil
                        "/home/marce/.emacs.d/snippets/python-mode/function"
                        nil nil)
                       ("for" "for ${var} in ${collection}:\n    $0"
                        "for ... in ... : ..." nil
                        ("control structure") nil
                        "/home/marce/.emacs.d/snippets/python-mode/for"
                        nil nil)
                       ("cls" "class ${1:class}:\n    $0\n" "class"
                        nil ("object oriented") nil
                        "/home/marce/.emacs.d/snippets/python-mode/cls"
                        nil nil)
                       ("#!" "#!/usr/bin/env python" "#!" nil nil nil
                        "/home/marce/.emacs.d/snippets/python-mode/bang"
                        nil nil)))


;;; Do not edit! File generated at Sun Sep  1 16:07:42 2024
