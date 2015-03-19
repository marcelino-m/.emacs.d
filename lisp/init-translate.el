(require-package 'google-translate)
(require 'google-translate)

(setq google-translate-default-source-language "en")
(setq google-translate-default-target-language "es")
(setq google-translate-translation-directions-alist '(("en" . "es")))
(global-set-key (kbd "<f2>") 'google-translate-smooth-translate)

(provide 'init-translate)
