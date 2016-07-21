(require-package 'exec-path-from-shell)
(require 'exec-path-from-shell)

(exec-path-from-shell-initialize)

(after-load 'exec-path-from-shell
  (dolist (var '("TIMLIB_SRC_ROOT" "LIB_V4D_INCLUDE" "LIB_V4D_TO_LINK" "NVM_BIN"))
    (add-to-list 'exec-path-from-shell-variables var)))

(exec-path-from-shell-initialize)

(provide 'init-exec-path)
