;;
;; COPYRIGHT: (C) 2025  Liii Network Inc
;; All rights reverved.
;;

(define-library (liii llm-config)
(import (liii base) (liii json) (liii lang) (liii path) (liii os) (liii logging))
(export model-profile load-home-file load-llm-key load-llm-menu default-maas merge-maas)
(begin

(define (load-home-file x . options)
  (let* ((mode (if (null? options) 'prefer-user (car options)))
         (user-path (path :from-env "TEXMACS_HOME_PATH" :/ "plugins" :/ "llm" :/ "data" :/ x))
         (system-path (path :from-env "TEXMACS_PATH" :/ "plugins" :/ "llm" :/ "data" :/ x)))
    (case mode
      ((user-only)
       (if (user-path :exists?) (option (user-path :read-text)) (none)))
      ((system-only)
       (if (system-path :exists?) (option (system-path :read-text)) (none)))
      ((prefer-user)
       (cond ((user-path :exists?) (option (user-path :read-text)))
             ((system-path :exists?) (option (system-path :read-text)))
             (else (none))))
      (else (error "Invalid mode for load-home-file")))))

(define default-maas
  (json '(("version" . 2)
          ("siliconflow.cn" .
           (("api_key" . "")
            ("base_url" . "https://api.siliconflow.cn/v1")))
          ("deepseek.com" .
           (("api_key" . "")
            ("base_url" . "https://api.deepseek.com/v1"))))))

(define (load-llm-key)
  (define-constant SILICONFLOW-DEFAULT-KEY "sk-ukaarlrgpzkpqpacvxzcykhvdzqkahscocitkyhwroxyzwsv")
  (define-constant DEEPSEEK-DEFAULT-KEY "sk-b37ca428d3634ff39ab49d2766d8efce")
  
  ;; Load system-maas from system-path, or use default-maas with default keys
  (let* ((system-maas-opt ((load-home-file "liii_llm_key.json" 'system-only)
                           :map (@ json :parse _)))
         (system-maas-base (system-maas-opt :get-or-else (lambda () default-maas)))
         (system-maas ((system-maas-base :set "siliconflow.cn" "api_key" SILICONFLOW-DEFAULT-KEY)
                       :set "deepseek.com" "api_key" DEEPSEEK-DEFAULT-KEY))
         ;; Load user-maas from user-path
         (user-maas-opt ((load-home-file "liii_llm_key.json" 'user-only)
                         :map (@ json :parse _))))
    ;; Merge user-maas into system-maas if user-maas exists, otherwise return system-maas
    (user-maas-opt :map (lambda (user-maas) (merge-maas system-maas user-maas))
                   :get-or-else (lambda () system-maas))))

(define (load-llm-menu)
  ;; Load system-menu from system-path, or use default menu
  (let* ((system-menu-opt ((load-home-file "liii_llm_menu.json" 'system-only)
                           :map (@ json :parse _)))
         (system-menu (system-menu-opt :get-or-else 
                       (lambda () 
                         (json '(("default" .
                                  (("name" . "8B")
                                   ("model" . "Qwen/Qwen3-8B")
                                   ("provider" . "siliconflow.cn")
                                   ("max_tokens" . 4096)))
                                 ("DeepSeek-V3@SiliconFlow" .
                                  (("name" . "V3")
                                   ("model" . "deepseek-ai/DeepSeek-V3")
                                   ("provider" . "siliconflow.cn")
                                   ("max_tokens" . 4096)))
                                 ("DeepSeek-V3" .
                                  (("name" . "V3")
                                   ("model" . "deepseek-chat")
                                   ("provider" . "deepseek.com")
                                   ("max_tokens" . 4096))))))))
         ;; Load user-menu from user-path
         (user-menu-opt ((load-home-file "liii_llm_menu.json" 'user-only)
                         :map (@ json :parse _))))
    ;; Merge user-menu into system-menu if user-menu exists, otherwise return system-menu
    (user-menu-opt :map (lambda (user-menu) (merge-maas system-menu user-menu))
                   :get-or-else (lambda () system-menu))))

(define (merge-maas system-maas user-maas)
  ($ (user-maas :keys) :fold system-maas
     (lambda (key result-maas)
       (let ((data (user-maas key)))
         (if (result-maas :contains-key? key)
             (result-maas :set key data)
             (result-maas :push key data))))))




)
)
