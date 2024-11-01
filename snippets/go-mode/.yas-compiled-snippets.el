;;; Compiled snippets and support files for `go-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'go-mode
                     '(("xml" "\\`xml:\"$1\"\\`" "xml" nil nil nil
                        "/home/marce/.emacs.d/snippets/go-mode/xml"
                        nil nil)
                       ("while" "for $1 {\n    $0\n}\n" "while" nil
                        nil nil
                        "/home/marce/.emacs.d/snippets/go-mode/while"
                        nil nil)
                       ("test" "func Test$1(t *testing.T) {\n	$0\n}"
                        "test" nil nil nil
                        "/home/marce/.emacs.d/snippets/go-mode/test"
                        nil nil)
                       ("struct"
                        "// $1 ${2:...}\ntype ${1:name} struct {\n	$0\n}"
                        "struct" nil nil nil
                        "/home/marce/.emacs.d/snippets/go-mode/struct"
                        nil nil)
                       ("range"
                        "for ${1:key}, ${2:value} := range ${3:target} {\n    $0\n}"
                        "range" nil nil nil
                        "/home/marce/.emacs.d/snippets/go-mode/range"
                        nil nil)
                       ("pkg"
                        "package ${1:`(car (last (split-string (file-name-directory buffer-file-name) \"/\") 2))`}\n"
                        "pkg" nil nil nil
                        "/home/marce/.emacs.d/snippets/go-mode/pkg"
                        nil nil)
                       ("met"
                        "// $2 ...\nfunc (${1:target}) ${2:name}(${3:arguments}) ${4:results} {\n	$0\n}"
                        "met" nil nil nil
                        "/home/marce/.emacs.d/snippets/go-mode/met"
                        nil nil)
                       ("map" "map[${1:keytype}]${2:valuetype}" "map"
                        nil nil nil
                        "/home/marce/.emacs.d/snippets/go-mode/map"
                        nil nil)
                       ("fmtf" "fmt.Printf(\"$2\", $1)$0" "fmt.Printf"
                        nil nil nil
                        "/home/marce/.emacs.d/snippets/go-mode/logf"
                        nil nil)
                       ("fmt" "fmt.Println($1)$0" "debug fmt.Println"
                        nil nil nil
                        "/home/marce/.emacs.d/snippets/go-mode/log"
                        nil nil)
                       ("js" "\\`json:\"$1\"\\`" "js" nil nil nil
                        "/home/marce/.emacs.d/snippets/go-mode/js" nil
                        nil)
                       ("imp" "import (\n	\"$1\"\n)" "imp" nil nil
                        nil
                        "/home/marce/.emacs.d/snippets/go-mode/imp"
                        nil nil)
                       ("ifunc" "func ($1) $2 {\n	$0\n}" "ifunc" nil
                        nil nil
                        "/home/marce/.emacs.d/snippets/go-mode/ifunc"
                        nil nil)
                       ("ifn" "if nil != $1 {\n	$0\n}" "ifn" nil nil
                        nil
                        "/home/marce/.emacs.d/snippets/go-mode/ifn"
                        nil nil)
                       ("iferr" "if err != nil {\n	$0\n}" "iferr" nil
                        nil nil
                        "/home/marce/.emacs.d/snippets/go-mode/iferr"
                        nil nil)
                       ("ifnot" "if !$1 {\n   $0\n}\n" "if not" nil
                        nil nil
                        "/home/marce/.emacs.d/snippets/go-mode/if-not"
                        nil nil)
                       ("if" "if $1 {\n	$0\n}" "if" nil nil nil
                        "/home/marce/.emacs.d/snippets/go-mode/if" nil
                        nil)
                       ("func"
                        "// $1 ${4:...}\nfunc ${1:name}(${2:arguments}) ${3:results} {\n	$0\n}"
                        "func" nil nil nil
                        "/home/marce/.emacs.d/snippets/go-mode/func"
                        nil nil)
                       ("for"
                        "for ${1:i} := ${2:0}; $1 < $3 ;  $1++ {\n	$0\n}"
                        "for (...; ...; ...) { ... }" nil nil nil
                        "/home/marce/.emacs.d/snippets/go-mode/for"
                        nil nil)
                       ("cstring"
                        "c${1:$(capitalize yas/text)} := C.CString($1)\ndefer C.free(unsafe.Pointer(c${1:$(capitalize yas/text)}))\n\n"
                        "CString" nil nil ((yas/indent-line 'fixed))
                        "/home/marce/.emacs.d/snippets/go-mode/cstring"
                        nil nil)
                       ("<<"
                        "${1:theSlice} = append($1, ${2:theValue})$0\n"
                        "<<" nil nil nil
                        "/home/marce/.emacs.d/snippets/go-mode/<<" nil
                        nil)))


;;; Do not edit! File generated at Sun Sep  1 16:07:42 2024
