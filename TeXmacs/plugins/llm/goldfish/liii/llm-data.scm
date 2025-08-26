;;
;; COPYRIGHT: (C) 2025  Liii Network Inc
;; All rights reverved.
;;

(define-library (liii llm-data)
(import (liii base) (liii string) (liii json) (liii path) (liii base64)
        (liii llm-config))
(export llm-message vlm-message message? image-content text-content payload model-profile)
(begin

(define-case-class model-profile
  ((name string?)
   (model string?)
   (provider string?)
   (base-url string?)
   (max-tokens integer?)
   (temperature float?)
   (stream boolean?)
   (key string?)
   (proxy any?)
   (default-system string?))

  (define (@from-json profile-j provider-j)
    (let ((name (profile-j :get-string "name" ""))
          (model (profile-j :get-string "model" ""))
          (provider (profile-j :get-string "provider" ""))
          ; 优先采用模型单独配置的base_url，如果没有提供，那么使用供应商配置文件中的base_url
          (base-url (profile-j :get-string "base_url"
                               (provider-j :get-string "base_url" "")))
          (max-tokens (profile-j :get-number "max_tokens" 512))
          (temperature (profile-j :get-number "temperature" 0.7))
          (stream #t)
          (key (provider-j :get-string "api_key" ""))
          (proxy ((provider-j "proxy") :get-or-else '()))
          (default-system (provider-j :get-string "default_system" "")))
      (model-profile name model provider base-url max-tokens temperature stream key proxy default-system))))

(define-case-class llm-message
  ((role string?)
   (content string?))

  (define (%to-json)
    (json `(("role" . ,role) ("content" . ,content)))))

(define-case-class image-content
  ((url string?)
   (detail string? "auto"))
   
  (define (%to-json)
    (json `(("image_url" . (("detail" . ,detail)
                            ("url" . ,url)))
            ("type" . "image_url"))))
  
  (define (@from-base64 data format)
    (image-content
      (string-append
        "data:image/" format ";base64," data)))
            
  (define (@from-png path)
    (let ((image-data (path-read-bytes path)))
      (image-content
        (string-append "data:image/png;base64,"
          (utf8->string (bytevector-base64-encode image-data))))))
)

(define-case-class text-content
  ((text string?))
  
  (define (%to-json)
    (json `(("text" . ,text)
            ("type" . "text")))))

(define-case-class vlm-message
  ((role string?)
   (content vector?))
   
  (define (%to-json)
    (json `(("role" . ,role)
            ("content" . ,(vector-map (lambda (x) ((x :to-json) :get)) content))))))

(define (message? obj)
  (or (obj :is-instance-of 'llm-message)
      (obj :is-instance-of 'vlm-message)))

(define-case-class payload
  ((messages vector? #()))
  
  (define (%to-json mp)
    (let ((model (mp 'model))
          (max-tokens (mp 'max-tokens))
          (temperature (mp 'temperature))
          (stream (mp 'stream)))
    (json `(("model" . ,model)
            ("messages" . ,(vector-map (lambda (x) ((x :to-json) :get)) messages))
            ("temperature" . ,temperature)
            ("stream" . ,(if stream 'true 'false))
            (,(if (or (string-contains model "o1") (string-contains model "o3"))
                   "max_completion_tokens"
                   "max_tokens") . ,max-tokens)))))
  
  (define (%to-json-string model )
    ((%to-json model) :to-string))
  
  (define (%append msg)
    (payload :messages (vector-append messages (vector msg)))))

) ; end of begin
) ; end of define-library
