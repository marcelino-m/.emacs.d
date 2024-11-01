;;; Compiled snippets and support files for `c-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'c-mode
                     '(("once"
                        "#ifndef ${1:PREFICX_$(upcase yas-text)}${2:`(upcase (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))`}${3:_H$(upcase yas-text)}\n#define $1$2$3\n\n$0\n\n#endif /* $1$2$3 */"
                        "#ifndef XXX; #define XXX; #endif" nil nil nil
                        "/home/marce/.emacs.d/snippets/c-mode/once"
                        nil nil)
                       ("inc" "#include \"$1\"\n" "#include \"...\""
                        nil nil nil
                        "/home/marce/.emacs.d/snippets/c-mode/inc" nil
                        nil)
                       ("if" "if (${1:condition})\n  {\n    $0\n  }"
                        "if (...) { ... }" nil nil nil
                        "/home/marce/.emacs.d/snippets/c-mode/if" nil
                        nil)))


;;; Do not edit! File generated at Sun Sep  1 16:07:42 2024
