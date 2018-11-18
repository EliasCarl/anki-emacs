(require 'dash)
(require 'request)
(require 'json)

;; https://github.com/louietan/anki-editor/blob/master/anki-editor.el
;; https://github.com/glutanimate/anki-connect

(defun anki--mk-action (action &optional params)
   (let ((ls '()))
     (when params
       (push `(params . ,params) ls))
     (push `(action . ,action) ls)))

(defun anki--mk-params (deck model front back &rest tags)
  `((note . ((deckName . ,deck)
              (modelName . ,model)
              (fields . ((Front . ,front)
                         (Back . ,back)))
              (tags . ,tags)))))

(json-encode
 (anki--mk-params "aws" "Basic" "front" "back" "tag1"))

;; http://tkf.github.io/emacs-request/manual.html
(defun anki--send (body)
  (let ((res nil)
        (err nil))
    (request
     "http://127.0.0.1:8765/"
     :type "POST"
     :data (encode-coding-string body 'utf-8)
     :parser 'json-read
     :sync t
     :success (function*
               (lambda (&key data &allow-other-keys)
                (setq res data)))
     :error (function*
             (lambda (&key _ &key error-thrown &allow-other-keys)
              (setq err error-thrown))))
    (when (not res)
      ;; If the action does not exist anki connect may respond with just null
      (error "Error from anki connect: %S" err))
    res))

(json-encode
 (anki--mk-action
  "addNote"
  (anki--mk-params "aws" "basic" "front" "back" "tag1" "tag2")))

(let ((body (json-encode
             (anki--mk-action
              "addNote"
              (anki--mk-params "AWS" "Basic" "front" "back" "networking")))))
  (message body)
  (message "%S" (anki--send body))
  )


