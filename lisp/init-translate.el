(require-package 'google-translate)
(require 'google-translate)
(require 'google-translate-smooth-ui)
(setq google-translate-default-source-language "en")
(setq google-translate-default-target-language "es")
(setq google-translate-translation-directions-alist '(("en" . "es") ("es" . "en")))
(global-set-key (kbd "<f2>") 'google-translate-smooth-translate)

(provide 'init-translate)
