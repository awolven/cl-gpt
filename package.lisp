(in-package :cl-user)

(defpackage :gpt
  (:use :cl)
  (:export #:*default-key-pathname*
	   #:*default-version*
	   #:*default-server*
	   #:*key*
	   #:set-key-path
	   #:list-models
	   #:get-model
	   #:create-completion
	   #:create-chat-completion
	   #:create-edit
	   #:create-image
	   #:create-image-edit
	   #:create-image-variation
	   #:create-embeddings
	   #:create-transcription
	   #:create-translation
	   #:list-files
	   #:upload-file
	   #:gpt-delete-file
	   #:get-file
	   #:get-file-contents-as-stream
	   #:get-file-contents
	   #:create-fine-tune
	   #:list-fine-tune
	   #:get-fine-tune
	   #:cancel-fine-tune
	   #:list-fine-tune-events-as-stream
	   #:list-fine-tune-events
	   #:delete-fine-tune
	   #:create-moderation))
	   
	   
