;;; Compiled snippets and support files for `c++-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'c++-mode
                     '(("shp" "std::shared_ptr<${1:type}> $0"
                        "shared_ptr" nil nil nil
                        "/home/marce/.emacs.d/snippets/c++-mode/shared_ptr"
                        nil nil)
                       ("sepfd" "//\n//\n//\n//\n$0" "sepfd" nil nil
                        nil
                        "/home/marce/.emacs.d/snippets/c++-mode/sepfd"
                        nil nil)
                       ("sep" "<< ${1:string} $0" "sep" nil nil nil
                        "/home/marce/.emacs.d/snippets/c++-mode/sep"
                        nil nil)
                       ("once"
                        "#ifndef ${1:PREFICX_$(upcase yas-text)}${2:`(upcase (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))`}${3:_H$(upcase yas-text)}\n#define $1$2$3\n\n$0\n\n#endif /* $1$2$3 */"
                        "#ifndef XXX; #define XXX; #endif" nil nil nil
                        "/home/marce/.emacs.d/snippets/c++-mode/once"
                        nil nil)
                       ("nss"
                        "namespace ${1:Namespace} {\n$0\n} // end  $1"
                        "namespace" nil nil nil
                        "/home/marce/.emacs.d/snippets/c++-mode/namespace"
                        nil nil)
                       ("log" "qDebug() << $0" "qerr" nil nil nil
                        "/home/marce/.emacs.d/snippets/c++-mode/log"
                        nil nil)
                       ("inc" "#include \"$1\"\n" "#include \"...\""
                        nil nil nil
                        "/home/marce/.emacs.d/snippets/c++-mode/inc"
                        nil nil)
                       ("nf"
                        "${1:type}\n${2:name}(${3:args})${4: const}\n{\n        $0\n}"
                        "function" nil nil nil
                        "/home/marce/.emacs.d/snippets/c++-mode/function"
                        nil nil)
                       ("fore"
                        "for(${1:auto &}$2 : ${3:conteiner}){\n    $0\n}"
                        "fore" nil nil nil
                        "/home/marce/.emacs.d/snippets/c++-mode/fore"
                        nil nil)
                       ("dox" "/*!\n * @brief ${1:TODO}\n */$0" "dox"
                        nil nil nil
                        "/home/marce/.emacs.d/snippets/c++-mode/dox"
                        nil nil)
                       ("cnf"
                        "${1:type}\n${2:Class}::${3:name}(${4:args})${5: const}\n{\n        $0\n}"
                        "function" nil nil nil
                        "/home/marce/.emacs.d/snippets/c++-mode/cnf"
                        nil nil)))


;;; Do not edit! File generated at Sun Sep  1 16:07:42 2024
