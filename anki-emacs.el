(require 'dash)
(require 'request)
(require 'json)

;; TODO Check if anki is running before doing anything

;; https://github.com/louietan/anki-editor/blob/master/anki-editor.el
;; https://github.com/glutanimate/anki-connect

;; Elisp help
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Using-Interactive.html#Using-Interactive

;; See emacs simple.el kill-region implementation for reference
;; (buffer-substring begin end)
;; (filter-buffer-substring begin end)
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Using-Interactive.html

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

(defun anki-emacs--test (beg end)
  (interactive "r")
  (message "%S" (buffer-substring-no-properties beg end)))

(defun anki-emacs--input-test (deck front back)
  (interactive "sDeck: \nsFront: \nsBack:")
  (message "%S %S %S" deck front back))

(json-encode
 (anki--mk-action
  "addNote"
  (anki--mk-params "aws" "basic" "front" "back" "tag1" "tag2")))

(let ((body (json-encode
             (anki--mk-action
              "addNote"
              (anki--mk-params "AWS" "Basic" "front" "back" "networking")))))
  (message body)
  (message "%S" (anki--send body)))



