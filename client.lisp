(in-package :gpt)

(defparameter *default-key-pathname* "~/openai/key")

(defvar *default-version* :v1)
(defvar *default-server* "api.openai.com")

(defvar *key*)

(defun read-key-file (filename)
  (with-open-file (fs filename :direction :input :element-type 'character)
    (loop with char
	  with string = (make-array 64 :adjustable t :fill-pointer 0 :element-type 'character)
	  do (setq char (read-char fs nil :eof))
	  when (eq char :eof)
	    do (return (string-trim '(#\Return #\Newline #\Space) string))
	  do (vector-push-extend char string))))

(defun set-key-path (&optional (pathname *default-key-pathname*))
  (setf *key* (read-key-file pathname)))

(set-key-path)


(defun validate-response-format-parameter (arg)
  (cond ((or (string-equal arg "url")
	     (eq arg :url))
	 "url")
	((or (string-equal arg "b64_json")
	     (string-equal arg "b64-json")
	     (eq :b64_json arg)
	     (eq :b64-json arg))
	 "b64_json")
	((or (string-equal arg "json")
	     (eq arg :json))
	 "json")
	((or (string-equal arg "verbose-json")
	     (string-equal arg "verbose_json")
	     (eq arg :verbose-json))
	 "verbose_json")
	((or (string-equal arg "srt")
	     (eq arg :srt))
	 "srt")
	((or (string-equal arg "vtt")
	     (eq arg :vtt))
	 "vtt")
	(t (error "invalid response-format ~S" arg))))
	  

(defun make-request-url (server version service-point &rest args)
  (apply #'concatenate 'string "https://" server "/" (string-downcase (format nil "~A" version))
	 (apply #'format nil service-point args)))

(defun make-request-arguments (key &optional (content nil content-present-p))
  (cond ((null content-present-p) ;; GET request
	 (list :additional-headers (list (cons :authorization (concatenate 'string "Bearer " key)))
	       :want-stream t))
	(t (list :method :post
		 :additional-headers (list (cons :authorization (concatenate 'string "Bearer " key)))
		 :want-stream t
		 :content-type :application/json
		 :content (st-json:write-json-to-string content)))))

(defun stringify (thing)
  (string-downcase (format nil "~A" thing)))

(defun list-models (&key
		      (version *default-version*)
		      (server  *default-server*)
		      (key *key*)
		    &aux (service-point "/models"))
  "Lists the currently available models, and provides basic information about each one such as the owner and availability."
  (st-json:read-json
   (apply #'drakma:http-request (make-request-url server version service-point) (make-request-arguments key))))

(defun get-model (model &key
			  (version *default-version*)
			  (server  *default-server*)
			  (key *key*)
		  &aux (service-point "/models/~A"))
  "Retrieves a model instance, providing basic information about the model such as the owner and permissioning.
arguments:
`model' The ID of the model to use for this request, a string or a symbol."
  (assert (or (symbolp model) (stringp model)))
  (st-json:read-json
   (apply #'drakma:http-request (make-request-url server version service-point (stringify model)) (make-request-arguments key))))
	  

(defun create-completion (model &key
				  (prompt            nil prompt-present-p)
				  (suffix            nil suffix-present-p)
				  (max-tokens        nil max-tokens-present-p)
				  (temperature       nil temperature-present-p)
				  (top-p             nil top-p-present-p)
				  (n                 nil n-present-p)
				  (stream            nil stream-present-p)
				  (logprobs          nil logprobs-present-p)
				  (echo              nil echo-present-p)
				  (stop              nil stop-present-p)
				  (presence-penalty  nil presence-penalty-present-p)
				  (frequency-penalty nil frequency-penalty-present-p)
				  (best-of           nil best-of-present-p)
				  (logit-bias        nil logit-bias-present-p)
				  (user              nil user-present-p)
				  (version           *default-version*)
				  (server            *default-server*)
				  (key *key*)
			  &aux (service-point "/completions"))
  "Creates a completion for the provided prompt and parameters

`model'
string or symbol
Required
ID of the model to use. You can use the List models API to see all of your available models, or see our Model overview for descriptions of them.

`prompt'
string or sequence
Optional
Defaults to \"<|endoftext|>\"
The prompt(s) to generate completions for, encoded as a string, array of strings, array of tokens, or array of token arrays.

Note that <|endoftext|> is the document separator that the model sees during training, so if a prompt is not specified the model will generate as if from the beginning of a new document.

`suffix'
string
Optional
Defaults to nil
The suffix that comes after a completion of inserted text.

`max-tokens'
integer
Optional
Defaults to 16
The maximum number of tokens to generate in the completion.

The token count of your prompt plus max-tokens cannot exceed the model's context length. Most models have a context length of 2048 tokens (except for the newest models, which support 4096).

`temperature'
number
Optional
Defaults to 1
What sampling temperature to use, between 0 and 2. Higher values like 0.8 will make the output more random, while lower values like 0.2 will make it more focused and deterministic.

We generally recommend altering this or top_p but not both.

`top-p'
number
Optional
Defaults to 1
An alternative to sampling with temperature, called nucleus sampling, where the model considers the results of the tokens with top_p probability mass. So 0.1 means only the tokens comprising the top 10% probability mass are considered.

We generally recommend altering this or temperature but not both.

`n'
integer
Optional
Defaults to 1
How many completions to generate for each prompt.

Note: Because this parameter generates many completions, it can quickly consume your token quota. Use carefully and ensure that you have reasonable settings for max_tokens and stop.

`stream'
boolean
Optional
Defaults to nil
Whether to stream back partial progress. If set, tokens will be sent as data-only server-sent events as they become available, with the stream terminated by a data: [DONE] message.

`logprobs'
integer
Optional
Defaults to nil
Include the log probabilities on the logprobs most likely tokens, as well the chosen tokens. For example, if logprobs is 5, the API will return a list of the 5 most likely tokens. The API will always return the logprob of the sampled token, so there may be up to logprobs+1 elements in the response.

The maximum value for logprobs is 5. If you need more than this, please contact us through our Help center and describe your use case.

`echo'
boolean
Optional
Defaults to nil
Echo back the prompt in addition to the completion

`stop'
string or sequence
Optional
Defaults to nil
Up to 4 sequences where the API will stop generating further tokens. The returned text will not contain the stop sequence.

`presence-penalty'
number
Optional
Defaults to 0
Number between -2.0 and 2.0. Positive values penalize new tokens based on whether they appear in the text so far, increasing the model's likelihood to talk about new topics.

See more information about frequency and presence penalties.

`frequency-penalty'
number
Optional
Defaults to 0
Number between -2.0 and 2.0. Positive values penalize new tokens based on their existing frequency in the text so far, decreasing the model's likelihood to repeat the same line verbatim.

See more information about frequency and presence penalties.

`best-of'
integer
Optional
Defaults to 1
Generates `best-of' completions server-side and returns the \"best\" (the one with the highest log probability per token). Results cannot be streamed.

When used with `n', `best-of' controls the number of candidate completions and `n' specifies how many to return `best-of' must be greater than `n'.

Note: Because this parameter generates many completions, it can quickly consume your token quota. Use carefully and ensure that you have reasonable settings for max_tokens and stop.

`logit-bias'
plist
Optional
Defaults to nil
Modify the likelihood of specified tokens appearing in the completion.

Accepts a plist that maps tokens (specified by their token ID in the GPT tokenizer) to an associated bias value from -100 to 100. You can use this tokenizer tool (which works for both GPT-2 and GPT-3) to convert text to token IDs. Mathematically, the bias is added to the logits generated by the model prior to sampling. The exact effect will vary per model, but values between -1 and 1 should decrease or increase likelihood of selection; values like -100 or 100 should result in a ban or exclusive selection of the relevant token.

As an example, you can pass (list \"50256\" -100) to prevent the \"<|endoftext|>\" token from being generated.

`user'
string
Optional
A unique identifier representing your end-user, which can help OpenAI to monitor and detect abuse."
  (assert (or (symbolp model) (stringp model)))
  (let* ((content (apply #'st-json:jso
			 "model" (stringify model)
			 (append
			  (when prompt-present-p
			    (assert (typep prompt 'sequence))
			    (list "prompt" (format nil "~A" prompt)))
			  (when suffix-present-p
			    (assert (stringp suffix))
			    (list "suffix" (format nil "~A" suffix)))
			  (when max-tokens-present-p
			    (assert (integerp max-tokens))
			    (list "max_tokens" max-tokens))
			  (when temperature-present-p
			    (assert (numberp temperature))
			    (assert (<= 0 temperature 2))
			    (list "temperature" temperature))
			  (when top-p-present-p
			    (assert (numberp top-p))
			    (list "top_p" top-p))
			  (when n-present-p
			    (assert (integerp n))
			    (list "n" n))
			  (when stream-present-p
			    (list "stream" stream))
			  (when logprobs-present-p
			    (assert (integerp logprobs))
			    (list "logprobs" logprobs))
			  (when echo-present-p
			    (list "echo" echo))
			  (when stop-present-p
			    (assert (typep stop 'sequence))
			    (list "stop" stop))
			  (when presence-penalty-present-p
			    (assert (numberp presence-penalty))
			    (list "presence_penalty" presence-penalty))
			  (when frequency-penalty-present-p
			    (assert (numberp frequency-penalty))
			    (list "frequency_penalty" frequency-penalty))
			  (when best-of-present-p
			    (assert (integerp logit-bias))
			    (list "best_of" best-of))
			  (when logit-bias-present-p
			    (assert (listp logit-bias))
			    (list "logit_bias" logit-bias))
			  (when user-present-p
			    (assert (stringp user))
			    (list "user" user)))))
	 (response-stream
	   (apply #'drakma:http-request (make-request-url server version service-point) (make-request-arguments key content))))
    (st-json:read-json response-stream)))

			       
			    

(defun create-chat-completion (model messages &key
						(temperature       nil temperature-present-p)
						(top-p             nil top-p-present-p)
						(n                 nil n-present-p)
						(stream            nil stream-present-p)
						(stop              nil stop-present-p)
						(max-tokens        nil max-tokens-present-p)
						(presence-penalty  nil presence-penalty-present-p)
						(frequency-penalty nil frequency-penalty-present-p)
						(logit-bias        nil logit-bias-present-p)
						(user              nil user-present-p)
						(version           *default-version*)
						(server            *default-server*)
						(key *key*)
			       &aux (service-point "/chat/completions"))
  "Creates a completion for the chat message.

`model'
string or symbol
Required
ID of the model to use. See the model endpoint compatibility table for details on which models work with the Chat API.

`messages'
sequence
Required
The messages to generate chat completions for, in the chat format.

`temperature'
number
Optional
Defaults to 1
What sampling temperature to use, between 0 and 2. Higher values like 0.8 will make the output more random, while lower values like 0.2 will make it more focused and deterministic.

We generally recommend altering this or top-p but not both.

`top-p'
number
Optional
Defaults to 1
An alternative to sampling with temperature, called nucleus sampling, where the model considers the results of the tokens with top-p probability mass. So 0.1 means only the tokens comprising the top 10% probability mass are considered.

We generally recommend altering this or temperature but not both.

`n'
integer
Optional
Defaults to 1
How many chat completion choices to generate for each input message.

`stream'
boolean
Optional
Defaults to nil
If set, partial message deltas will be sent, like in ChatGPT. Tokens will be sent as data-only server-sent events as they become available, with the stream terminated by a data: [DONE] message. See the OpenAI Cookbook for example code.

`stop'
string or sequence
Optional
Defaults to nil
Up to 4 sequences where the API will stop generating further tokens.

`max-tokens'
integer
Optional
Defaults to infinity
The maximum number of tokens to generate in the chat completion.

The total length of input tokens and generated tokens is limited by the model's context length.

`presence-penalty'
number
Optional
Defaults to 0
Number between -2.0 and 2.0. Positive values penalize new tokens based on whether they appear in the text so far, increasing the model's likelihood to talk about new topics.

See more information about frequency and presence penalties.

`frequency-penalty'
number
Optional
Defaults to 0
Number between -2.0 and 2.0. Positive values penalize new tokens based on their existing frequency in the text so far, decreasing the model's likelihood to repeat the same line verbatim.

See more information about frequency and presence penalties.

`logit-bias'
plist
Optional
Defaults to nil
Modify the likelihood of specified tokens appearing in the completion.

Accepts a plist that maps tokens (specified by their token ID in the tokenizer) to an associated bias value from -100 to 100. Mathematically, the bias is added to the logits generated by the model prior to sampling. The exact effect will vary per model, but values between -1 and 1 should decrease or increase likelihood of selection; values like -100 or 100 should result in a ban or exclusive selection of the relevant token.

`user'
string
Optional
A unique identifier representing your end-user, which can help OpenAI to monitor and detect abuse."
  (assert (or (symbolp model) (stringp model)))
  (assert (typep messages 'sequence))
  (let* ((content (apply #'st-json:jso
			 "model" (stringify model)
			 "messages" (st-json:jso messages)
			 (append
			  (when temperature-present-p
			    (assert (numberp temperature))
			    (assert (<= 0 temperature 2))
			    (list "temperature" temperature))
			  (when top-p-present-p
			    (assert (numberp top-p))
			    (list "top_p" top-p))
			  (when n-present-p
			    (assert (integerp n))
			    (list "n" n))
			  (when stream-present-p
			    (list "stream" (if stream :true :false)))
			  (when stop-present-p
			    (assert (typep stop 'sequence))
			    (list "stop" stop))
			  (when max-tokens-present-p
			    (assert (integerp max-tokens))
			    (list "max_tokens" max-tokens))
			  (when presence-penalty-present-p
			    (assert (numberp presence-penalty))
			    (list "presence_penalty" presence-penalty))
			  (when frequency-penalty-present-p
			    (assert (numberp frequency-penalty))
			    (list "frequency_penalty" frequency-penalty))
			  (when logit-bias-present-p
			    (assert (listp logit-bias))
			    (list "logit_bias" logit-bias))
			  (when user-present-p
			    (assert (stringp user))
			    (list "user" user)))))
	 (response-stream
	   (apply #'drakma:http-request (make-request-url server version service-point) (make-request-arguments key content))))
    (if stream
	response-stream
	(st-json:read-json response-stream))))
  
  

(defun create-edit (instruction &key
				  (model :text-davici-edit-001)
				  (input       nil input-present-p)
				  (temperature nil temperature-present-p)
				  (n           nil n-present-p)					
				  (top-p       nil top-p-present-p)
				  (version     *default-version*)
				  (server      *default-server*)
				  (key *key*)
		    &aux (service-point "/edits"))
  "Creates a new edit for the provided input, instruction, and parameters.

`model'
string
Required
ID of the model to use. You can use the text-davinci-edit-001 or code-davinci-edit-001 model with this endpoint.

`input'
string
Optional
Defaults to \"\"
The input text to use as a starting point for the edit.

`instruction'
string
Required
The instruction that tells the model how to edit the prompt.

`n'
integer
Optional
Defaults to 1
How many edits to generate for the input and instruction.

`temperature'
number
Optional
Defaults to 1
What sampling temperature to use, between 0 and 2. Higher values like 0.8 will make the output more random, while lower values like 0.2 will make it more focused and deterministic.

We generally recommend altering this or `top-p' but not both.

`top-p'
number
Optional
Defaults to 1
An alternative to sampling with temperature, called nucleus sampling, where the model considers the results of the tokens with top_p probability mass. So 0.1 means only the tokens comprising the top 10% probability mass are considered.

We generally recommend altering this or `temperature' but not both."
  (assert (or (symbolp model) (stringp model)))
  (assert (stringp instruction))
  (let* ((content (apply #'st-json:jso
			 "instruction" instruction
			 "model" (stringify model)
			 (append
			  (when input-present-p
			    (assert (stringp input))
			    (list "input" input))
			  (when temperature-present-p
			    (assert (numberp temperature))
			    (assert (<= 0 temperature 2))
			    (list "temperature" temperature))
			  (when n-present-p
			    (assert (integerp n))
			    (list "n" n))
			  (when top-p-present-p
			    (assert (numberp top-p))
			    (list "top_p" top-p)))))
	 (response-stream
	   (apply #'drakma:http-request (make-request-url server version service-point) (make-request-arguments key content))))
    (st-json:read-json response-stream)))
  
	 

(defun create-image (prompt &key
			      (n               nil n-present-p)
			      (size            nil size-present-p)
			      (response-format nil response-format-present-p)
			      (user            nil user-present-p)
			      (version         *default-version*)
			      (server          *default-server*)
			      (key *key*)
		     &aux (service-point "/images/generations"))
  "Creates an image given a prompt.

`prompt'
string
Required
A text description of the desired image(s). The maximum length is 1000 characters.

`n'
integer
Optional
Defaults to 1
The number of images to generate. Must be between 1 and 10.

`size'
string
Optional
Defaults to \"1024x1024\"
The size of the generated images. Must be one of \"256x256\", \"512x512\", or \"1024x1024\".

`response-format'
string or symbol
Optional
Defaults to :url
The format in which the generated images are returned. Must be one of :url or :b64-json.

`user'
string
Optional
A unique identifier representing your end-user, which can help OpenAI to monitor and detect abuse."
  (assert (stringp prompt))
  (let* ((content (apply #'st-json:jso
			 "prompt" prompt
			 (append
			  (when n-present-p
			    (assert (integerp n))
			    (assert (<= 0 n 10))
			    (list "n" n))
			  (when size-present-p
			    (unless (stringp size)
			      (setq size (format nil "~Ax~A" (elt size 0) (elt size 1))))
			    (list "size" size))
			  (when response-format-present-p
			    (list "response_format" (validate-response-format-parameter response-format)))
			  (when user-present-p
			    (assert (stringp user))
			    (list "user" user)))))
	 (response-stream
	   (apply #'drakma:http-request (make-request-url server version service-point) (make-request-arguments key content))))
    (st-json:read-json response-stream)))
  

(defun create-image-edit (image prompt &key
					 (mask            nil mask-present-p)
					 (n               nil n-present-p)
					 (size            nil size-present-p)
					 (response-format nil response-format-present-p)
					 (user            nil user-present-p)
					 (version         *default-version*)
					 (server          *default-server*)
					 (key *key*)
			  &aux (service-point "/images/edits"))
  "Creates an edited or extended image given an original image and a prompt.

`image'
string
Required
The image to edit. Must be a valid PNG file, less than 4MB, and square. If mask is not provided, image must have transparency, which will be used as the mask.

`mask'
string
Optional
An additional image whose fully transparent areas (e.g. where alpha is zero) indicate where image should be edited. Must be a valid PNG file, less than 4MB, and have the same dimensions as image.

`prompt'
string
Required
A text description of the desired image(s). The maximum length is 1000 characters.

`n'
integer
Optional
Defaults to 1
The number of images to generate. Must be between 1 and 10.

`size'
string
Optional
Defaults to \"1024x1024\"
The size of the generated images. Must be one of \"256x256\", \"512x512\", or \"1024x1024\".

`response-format'
string or symbol
Optional
Defaults to :url
The format in which the generated images are returned. Must be one of :url or :b64-json.

`user'
string
Optional
A unique identifier representing your end-user, which can help OpenAI to monitor and detect abuse."
  (assert (stringp image))
  (assert (stringp prompt))
  (let* ((content (apply #'st-json:jso
			 "image" image
			 "prompt" prompt
			 (append
			  (when mask-present-p
			    (assert (stringp mask))
			    (list "mask" mask))
			  (when n-present-p
			    (assert (integerp n))
			    (list "n" n))
			  (when size-present-p
			    (unless (stringp size)
			      (setq size (format nil "~Ax~A" (elt size 0) (elt size 1))))
			    (list "size" size))
			  (when response-format-present-p
			    (list "response_format" (validate-response-format-parameter response-format)))
			  (when user-present-p
			    (assert (stringp user))
			    (list "user" user))))))
    (st-json:read-json
     (apply #'drakma:http-request
	    (make-request-url server version service-point) (make-request-arguments key content)))))
  

(defun create-image-variation (image &key
				       (n               nil n-present-p)
				       (size            nil size-present-p)
				       (response-format nil response-format-present-p)
				       (user            nil user-present-p)
				       (version         *default-version*)
				       (server          *default-server*)
				       (key *key*)
			       &aux (service-point "/images/variations"))
  "Creates a variation of a given image.

`image'
string
Required
The image to use as the basis for the variation(s). Must be a valid PNG file, less than 4MB, and square.

`n'
integer
Optional
Defaults to 1
The number of images to generate. Must be between 1 and 10.

`size'
string
Optional
Defaults to \"1024x1024\"
The size of the generated images. Must be one of \"256x256\", \"512x512\", or \"1024x1024\".

`response-format'
string or symbol
Optional
Defaults to :url
The format in which the generated images are returned. Must be one of :url or :b64-json.

`user'
string
Optional
A unique identifier representing your end-user, which can help OpenAI to monitor and detect abuse."
  (assert (stringp image))
  (let* ((content (apply #'st-json:jso
			 "image" image
			 (append
			  (when n-present-p
			    (assert (integerp n))
			    (list "n" n))
			  (when size-present-p
			    (unless (stringp size)
			      (setq size (format nil "~Ax~A" (elt size 0) (elt size 1))))
			    (list "size" size))
			  (when response-format-present-p
			    (list "response_format" (validate-response-format-parameter response-format)))
			  (when user-present-p
			    (assert (stringp user))
			    (list "user" user))))))
    (st-json:read-json
     (apply #'drakma:http-request
	    (make-request-url server version service-point) (make-request-arguments key content)))))
  

(defun create-embeddings (model input &key
					(user    nil user-present-p)
					(version *default-version*)
					(server  *default-server*)
					(key *key*)
			  &aux (service-point "/embeddings"))
  "Creates an embedding vector representing the input text.

`model'
string
Required
ID of the model to use. You can use the List models API to see all of your available models, or see our Model overview for descriptions of them.

`input'
string or sequence
Required
Input text to get embeddings for, encoded as a string or array of tokens. To get embeddings for multiple inputs in a single request, pass an array of strings or array of token arrays. Each input must not exceed 8192 tokens in length.

`user'
string
Optional
A unique identifier representing your end-user, which can help OpenAI to monitor and detect abuse."
  (assert (stringp model))
  (assert (typep input 'sequence))
  (let* ((content (apply #'st-json:jso
			 "model" (stringify model)
			 "input" input
			 (append
			  (when user-present-p
			    (assert (stringp user))
			    (list "user" user))))))
    (st-json:read-json
     (apply #'drakma:http-request
	    (make-request-url server version service-point) (make-request-arguments key content)))))
			 
			

(defun create-transcription (file &key
				    (model           :whisper-1)
				    (prompt          nil prompt-present-p)
				    (response-format nil response-format-present-p)
				    (temperature     nil temperature-present-p)
				    (language        nil language-present-p)
				    (version         *default-version*)
				    (server          *default-server*)
				    (key *key*)
			     &aux (service-point "/audio/transcription"))
  "Transcribes audio into the input language.

`file'
string
Required
The audio file to transcribe, in one of these formats: mp3, mp4, mpeg, mpga, m4a, wav, or webm.

`model'
string
Required
ID of the model to use. Only whisper-1 is currently available.

`prompt'
string
Optional
An optional text to guide the model's style or continue a previous audio segment. The prompt should match the audio language.

`response-format'
string
Optional
Defaults to :json
The format of the transcript output, in one of these options: :json, :text, :srt, :verbose-json, or :vtt.

`temperature'
number
Optional
Defaults to 0
The sampling temperature, between 0 and 1. Higher values like 0.8 will make the output more random, while lower values like 0.2 will make it more focused and deterministic. If set to 0, the model will use log probability to automatically increase the temperature until certain thresholds are hit.

`language'
string
Optional
The language of the input audio. Supplying the input language in ISO-639-1 format will improve accuracy and latency."
  (assert (stringp file))
  (assert (or (symbolp model) (stringp model)))
  (let* ((content (apply #'st-json:jso
			 "file" file
			 "model" (stringify model)
			 (append
			  (when prompt-present-p
			    (assert (stringp prompt))
			    (list "prompt" prompt))
			  (when response-format-present-p
			    (list "response_format" (validate-response-format-parameter response-format)))
			  (when temperature-present-p
			    (assert (numberp temperature))
			    (assert (<= 0 temperature 1))
			    (list "temperature" temperature))
			  (when language-present-p
			    (assert (stringp language))
			    (list "language" language))))))
    (st-json:read-json
     (apply #'drakma:http-request
	    (make-request-url server version service-point) (make-request-arguments key content)))))

  
  

(defun create-translation (file &key
				  (model           :whisper-1)
				  (prompt          nil prompt-present-p)
				  (response-format nil response-format-present-p)
				  (temperature     nil temperature-present-p)
				  (version         *default-version*)
				  (server          *default-server*)
				  (key *key*)
			   &aux (service-point "/audio/translations"))
  "Translates audio into into English.

`file'
string
Required
The audio file to translate, in one of these formats: mp3, mp4, mpeg, mpga, m4a, wav, or webm.

`model'
string
Required
ID of the model to use. Only whisper-1 is currently available.

`prompt'
string
Optional
An optional text to guide the model's style or continue a previous audio segment. The prompt should be in English.

`response-format'
string
Optional
Defaults to :json
The format of the transcript output, in one of these options: :json, :text, :srt, :verbose-json, or :vtt.

`temperature'
number
Optional
Defaults to 0
The sampling temperature, between 0 and 1. Higher values like 0.8 will make the output more random, while lower values like 0.2 will make it more focused and deterministic. If set to 0, the model will use log probability to automatically increase the temperature until certain thresholds are hit."
  
  (assert (stringp file))
  (assert (or (stringp model) (stringp model)))
  (let* ((content (apply #'st-json:jso
			 "file" file
			 "model" (stringify model)
			 (append
			  (when prompt-present-p
			    (assert (stringp prompt))
			    (list "prompt" prompt))
			  (when response-format-present-p
			    (list "response_format" (validate-response-format-parameter response-format)))
			  (when temperature-present-p
			    (assert (numberp temperature))
			    (assert (<= 0 temperature 1))
			    (list "temperature" temperature))))))
    (st-json:read-json
     (apply #'drakma:http-request
	    (make-request-url server version service-point) (make-request-arguments key content)))))
			 

(defun list-files (&key
		     (version *default-version*)
		     (server  *default-server*)
		     (key *key*)
		   &aux (service-point "/files"))
  "Returns a list of files that belong to the user's organization."
  (st-json:read-json
   (apply #'drakma:http-request (make-request-url server version service-point) (make-request-arguments key))))
  

(defun upload-file (file &key
			   (purpose :fine-tune)
			   (version *default-version*)
			   (server  *default-server*)
			   (key *key*)
		    &aux (service-point "/files"))
  "Upload a file that contains document(s) to be used across various endpoints/features. Currently, the size of all the files uploaded by one organization can be up to 1 GB. Please contact us if you need to increase the storage limit.

`file'
string, pathname, or stream
Required
The JSON Lines file to be uploaded.

If the purpose is set to :fine-tune, each line is a JSON record with \"prompt\" and \"completion\" fields representing your training examples.

`purpose'
string or symbol
Optional
The intended purpose of the uploaded documents. Defaults to :fine-tune.

Use :fine-tune for Fine-tuning. This allows us to validate the format of the uploaded file."
  (assert (typep file '(or pathname string stream)))
  (assert (stringp purpose))
  (flet ((stream-to-lines (stream)
	   (let ((lines ()))
	     (loop with char
		   with line = (make-array 64 :adjustable t :fill-pointer 0 :element-type 'character)
		   do (setq char (read-char stream nil :eof))
		   when (eq char :eof)
		     do (return)
		   when (char= char #\Newline)
		     do (push line lines)
			(setq line (make-array 64 :adjustable t :fill-pointer 0 :element-type 'character))
		   unless (or (char= char #\Return) (char= char #\Newline))
		     do (vector-push-extend char line)
		   finally (return (nreverse lines))))))
    (let ((content (st-json:jso
		    "purpose" (stringify purpose)
		    "file"
		    (if (streamp file)
			(stream-to-lines file)
			(with-open-file (stream file :direction :input :element-type 'character)
			  (stream-to-lines stream))))))
      (st-json:read-json
       (apply #'drakma:http-request (make-request-url server version service-point) (make-request-arguments key content))))))
	

(defun gpt-delete-file (file-id &key
			      (version *default-version*)
			      (server  *default-server*)
			      (key *key*)
			&aux (service-point "/files/~A"))
  "Delete a file.

`file-id'
string
Required
The ID of the file to use for this request"
  (assert (stringp file-id))
  (st-json:read-json
   (drakma:http-request (make-request-url server version service-point file-id)
			:method :delete
			:additional-headers (list (cons :authorization (concatenate 'string "Bearer " key)))
			:want-stream t)))
			    

(defun get-file (file-id &key
			   (version *default-version*)
			   (server  *default-server*)
			   (key *key*)
		 &aux (service-point "/files/~A"))
  "Returns information about a specific file.

`file-id'
string
Required
The ID of the file to use for this request"
  (assert (stringp file-id))
  (st-json:read-json
   (apply #'drakma:http-request (make-request-url server version service-point file-id) (make-request-arguments key))))


(defun get-file-content-as-stream (file-id 
				&key
				  (version *default-version*)
				  (server  *default-server*)
				  (key *key*)
				   &aux (service-point "/files/~A/content"))
  "Returns the contents of the specified file as a stream.

`file-id'
string
Required
The ID of the file to use for this request"
  (assert (stringp file-id))
  (apply #'drakma:http-request (make-request-url server version service-point file-id) (make-request-arguments key)))

(defun get-file-contents (file-id 
			  &key
			    (version *default-version*)
			    (server  *default-server*)
			    (key *key*)
			    (alloc 4096))
  "Returns the contents of the specified file.

`file-id'
string
Required
The ID of the file to use for this request"
  (assert (typep alloc '(integer 0)))
  (let ((stream (get-file-content-as-stream file-id :version version :server server :key key))
	(string (make-array alloc :element-type 'character :adjustable t :fill-pointer 0)))
    (loop with char
	  do (setq char (read-char stream nil :eof))
	  when (eq char :eof)
	    do (return string)
	  do (vector-push-extend char string)
	  finally (return string))))

(defun create-fine-tune (training-file &key
					 (validation-file nil validation-file-present-p)
					 (model nil model-present-p)
					 (n-epochs nil n-epochs-present-p)
					 (batch-size nil batch-size-present-p)
					 (learning-rate-multiplier nil learning-rate-multiplier-present-p)
					 (prompt-loss-weight nil prompt-loss-weight-present-p)
					 (compute-classification-metrics
					  nil compute-classification-metrics-present-p)
					 (classification-n-classes nil classification-n-classes-present-p)
					 (classification-positive-class
					  nil classification-positive-class-present-p)
					 (classification-betas nil classification-betas-present-p)
					 (suffix nil suffix-present-p)
					 (version *default-version*)
					 (server  *default-server*)
					 (key *key*)
			 &aux (service-point "/fine-tunes"))
  "Creates a job that fine-tunes a specified model from a given dataset.

Response includes details of the enqueued job including job status and the name of the fine-tuned models once complete.

`training-file'
string
Required
The ID of an uploaded file that contains training data.

See upload-file for how to upload a file.

Your dataset must be formatted as a JSONL file, where each training example is a JSON object with the keys \"prompt\" and \"completion\". Additionally, you must upload your file with the purpose fine-tune.

See the fine-tuning guide for more details.

`validation-file'
string
Optional
The ID of an uploaded file that contains validation data.

If you provide this file, the data is used to generate validation metrics periodically during fine-tuning. These metrics can be viewed in the fine-tuning results file. Your train and validation data should be mutually exclusive.

Your dataset must be formatted as a JSONL file, where each validation example is a JSON object with the keys \"prompt\" and \"completion\". Additionally, you must upload your file with the purpose fine-tune.

See the fine-tuning guide for more details.

`model'
string or symbol
Optional
Defaults to curie
The name of the base model to fine-tune. You can select one of \"ada\", \"babbage\", \"curie\", \"davinci\", or a fine-tuned model created after 2022-04-21. To learn more about these models, see the Models documentation.

`n-epochs'
integer
Optional
Defaults to 4
The number of epochs to train the model for. An epoch refers to one full cycle through the training dataset.

`batch-size'
integer
Optional
Defaults to nil
The batch size to use for training. The batch size is the number of training examples used to train a single forward and backward pass.

By default, the batch size will be dynamically configured to be ~0.2% of the number of examples in the training set, capped at 256 - in general, we've found that larger batch sizes tend to work better for larger datasets.

`learning-rate-multiplier'
number
Optional
Defaults to nil
The learning rate multiplier to use for training. The fine-tuning learning rate is the original learning rate used for pretraining multiplied by this value.

By default, the learning rate multiplier is the 0.05, 0.1, or 0.2 depending on final batch_size (larger learning rates tend to perform better with larger batch sizes). We recommend experimenting with values in the range 0.02 to 0.2 to see what produces the best results.

`prompt-loss-weight'
number
Optional
Defaults to 0.01
The weight to use for loss on the prompt tokens. This controls how much the model tries to learn to generate the prompt (as compared to the completion which always has a weight of 1.0), and can add a stabilizing effect to training when completions are short.

If prompts are extremely long (relative to completions), it may make sense to reduce this weight so as to avoid over-prioritizing learning the prompt.

`compute-classification-metrics'
boolean
Optional
Defaults to false
If set, we calculate classification-specific metrics such as accuracy and F-1 score using the validation set at the end of every epoch. These metrics can be viewed in the results file.

In order to compute classification metrics, you must provide a validation_file. Additionally, you must specify classification_n_classes for multiclass classification or classification_positive_class for binary classification.

`classification-n-classes'
integer
Optional
Defaults to nil
The number of classes in a classification task.

This parameter is required for multiclass classification.

`classification-positive-class'
string
Optional
Defaults to nil
The positive class in binary classification.

This parameter is needed to generate precision, recall, and F1 metrics when doing binary classification.

`classification_betas'
array
Optional
Defaults to nil
If this is provided, we calculate F-beta scores at the specified beta values. The F-beta score is a generalization of F-1 score. This is only used for binary classification.

With a beta of 1 (i.e. the F-1 score), precision and recall are given the same weight. A larger beta score puts more weight on recall and less on precision. A smaller beta score puts more weight on precision and less on recall.

`suffix'
string
Optional
Defaults to nil
A string of up to 40 characters that will be added to your fine-tuned model name.

For example, a suffix of \"custom-model-name\" would produce a model name like ada:ft-your-org:custom-model-name-2022-02-15-04-21-04."
  (assert (stringp training-file))
  (let* ((content (apply #'st-json:jso
			 "training_file" training-file
			 (when validation-file-present-p
			   (list "validation_file" validation-file))
			 (when model-present-p
			   (list "model" (stringify model)))
			 (when n-epochs-present-p
			   (list "n_epochs" n-epochs))
			 (when batch-size-present-p
			   (list "batch_size" batch-size))
			 (when learning-rate-multiplier-present-p
			   (list "learning_rate_multiplier"
				 learning-rate-multiplier))
			 (when prompt-loss-weight-present-p
			   (list "prompt_loss_weight" prompt-loss-weight))
			 (when compute-classification-metrics-present-p
			   (list "compute_classification_metrics"
				 compute-classification-metrics))
			 (when classification-n-classes-present-p
			   (list "classification_n_classes"
				 classification-n-classes))
			 (when classification-positive-class-present-p
			   (list "classification_positive_class"
				 classification-positive-class))
			 (when classification-betas-present-p
			   (list "classification_betas"
				 classification-betas))
			 (when suffix-present-p
			   (list "suffix" suffix)))))
    (st-json:read-json
     (apply #'drakma:http-request
	    (make-request-url server version service-point) (make-request-arguments key content)))))
    


(defun list-fine-tunes (&key
			  (version *default-version*)
			  (server  *default-server*)
			  (key *key*)
			&aux (service-point "/fine-tunes"))
  "List your organization's fine-tuning jobs."
  (st-json:read-json
   (apply #'drakma:http-request (make-request-url server version service-point) (make-request-arguments key))))

(defun get-fine-tune (fine-tune-id &key
				     (version *default-version*)
				     (server  *default-server*)
				     (key *key*)
		      &aux (service-point "/fine-tunes/~A"))
  "Gets info about the fine-tune job.

`fine-tune-id'
string
Required
The ID of the fine-tune job"  
  (assert (stringp fine-tune-id))
  (st-json:read-json
   (apply #'drakma:http-request (make-request-url server version service-point fine-tune-id) (make-request-arguments key))))

(defun cancel-fine-tune (fine-tune-id &key
					(version *default-version*)
					(server  *default-server*)
					(key *key*)
			 &aux (service-point "/fine-tunes/~A/cancel"))
  "Immediately cancel a fine-tune job.

`fine-tune-id'
string
Required
The ID of the fine-tune job to cancel"
  (assert (stringp fine-tune-id))
  (st-json:read-json
   (apply #'drakma:http-request (make-request-url server version service-point fine-tune-id) (make-request-arguments key))))

(defun list-fine-tune-events-as-stream (fine-tune-id &key
						    (version *default-version*)
						    (server  *default-server*)
						    (key *key*)
					&aux (service-point "/fine-tunes/~A/cancel?stream=true"))
  "Get fine-grained status updates for a fine-tune job as a stream.

`fine-tune-id'
string
Required
The ID of the fine-tune job to get events for."
  (assert (stringp fine-tune-id))
  (apply #'drakma:http-request
	 (make-request-url server version service-point fine-tune-id)
	 (make-request-arguments key)))

(defun list-fine-tune-events (fine-tune-id &key
					     (stream nil)
					     (version *default-version*)
					     (server  *default-server*)
					     (key *key*)
			      &aux (service-point "/fine-tunes/~A/cancel?stream=~A"))
  "Get fine-grained status updates for a fine-tune job.

`fine-tune-id'
string
Required
The ID of the fine-tune job to get events for.

`stream'
boolean
Optional
Defaults to nil
Whether to stream events for the fine-tune job. If set to true, events will be sent as data-only server-sent events as they become available. The stream will terminate with a data: [DONE] message when the job is finished (succeeded, cancelled, or failed).

If set to false, only events generated so far will be returned."
  (assert (stringp fine-tune-id))
  (if stream
      (list-fine-tune-events-as-stream fine-tune-id :version version :server server :key key)
      (st-json:read-json
       (apply #'drakma:http-request
	      (make-request-url server version service-point fine-tune-id "false")
	      (make-request-arguments key)))))

(defun delete-fine-tune (model &key  
				 (version *default-version*)
				 (server  *default-server*)
				 (key *key*)
			 &aux (service-point "/models/~A"))
  "Delete a fine-tuned model. You must have the Owner role in your organization.

`model'
string or symbol
Required
The model to delete"
  (st-json:read-json
   (drakma:http-request (make-request-url server version service-point (stringify model))
			:method :delete
			:additional-headers (list (cons :authorization (concatenate 'string "Bearer " key)))
			:want-stream t)))

(defun create-moderation (input &key
				  (model nil model-present-p)
				  (version *default-version*)
				  (server  *default-server*)
				  (key *key*)
			  &aux (service-point "/moderations"))
  "Classifies if text violates OpenAI's Content Policy

`input'
string or sequence of strings
Required
The input text to classify

`model'
string or symbol
Optional
Defaults to :text-moderation-latest
Two content moderations models are available: text-moderation-stable and text-moderation-latest.

The default is text-moderation-latest which will be automatically upgraded over time. This ensures you are always using our most accurate model. If you use text-moderation-stable, we will provide advanced notice before updating the model. Accuracy of text-moderation-stable may be slightly lower than for text-moderation-latest."
  (assert (typep input 'sequence))
  (let* ((content (apply #'st-json:jso
			 "input" input
			 (when model-present-p
			   (list "model" (stringify model))))))
    (st-json:read-json
     (apply #'drakma:http-request
	    (make-request-url server version service-point) (make-request-arguments key content)))))
