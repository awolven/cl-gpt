(defsystem :cl-gpt
  :description "A client for OpenAI's GPT models in Common Lisp"
  :depends-on (:st-json :drakma)
  :author "Andrew K Wolven <awolven@gmail.com>"
  :license "MIT"
  :serial t
  :components
  ((:file "package")
   (:file "client")))
