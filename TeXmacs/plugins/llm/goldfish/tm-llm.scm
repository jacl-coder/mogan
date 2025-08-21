;;
;; COPYRIGHT: (C) 2025  Liii Network Inc
;; All rights reverved.
;;

(import (texmacs protocol)
        (liii list)
        (liii string)
        (liii os)
        (liii sys)
        (liii json)
        (liii logging)
        (liii path))

(define llm-plugin-version "v20250821")

(define log (logging "llm-goldfish-log"))
(log :set-path! ((path "/tmp/llm-goldfish-log.log") :to-string))

(set! *load-path*
  (let ((user-path (string-append (getenv "TEXMACS_HOME_PATH") "/plugins/llm/goldfish"))
        (sys-path (string-append (getenv "TEXMACS_PATH") "/plugins/llm/goldfish")))
    (if (file-exists? user-path)
        (cons user-path *load-path*)
        (cons sys-path *load-path*))))

(import (liii llm))

(let* ((choice (last (argv)))
       (key-j (load-llm-key))
       (menu-j (load-llm-menu))
       (choice-profile (menu-j choice))
       (profile-j (if (choice-profile :null?) (menu-j "default") choice-profile))
       (provider (profile-j :get-string "provider" ""))
       (provider-j (key-j provider))
       (mp (model-profile :from-json profile-j provider-j)))
  (llm-welcome mp llm-plugin-version)
  (llm-repl mp))
