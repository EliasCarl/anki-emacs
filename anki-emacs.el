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
;; https://stackoverflow.com/questions/9617183/how-to-find-which-file-provided-the-feature-in-emacs-elisp/9620373#9620373
;; https://stackoverflow.com/questions/9646088/emacs-interactive-commands-with-default-value
;; (thing-at-point)
;; (read-string)

(defun eanki--mk-action (action &optional params)
  (let ((ls '()))
    (when params
      (push `(params . ,params) ls))
    (push `(action . ,action) ls)))

(defun eanki--mk-params (model deck front back &rest tags)
  `((note . ((deckName . ,deck)
              (modelName . ,model)
              (fields . ((Front . ,front)
                         (Back . ,back)))
             (tags . ,tags)))))

(defun eanki--current-deck ()
  (save-excursion
    (goto-char (point-min))
    (buffer-substring-no-properties
     (line-beginning-position)
     (line-end-position))))

(defun eanki--add-basic (deck front back tag &rest tags)
  (interactive
   (let ((text (buffer-substring-no-properties
                (region-beginning)
                (region-end))))
     (list (read-string (format "deck (%s): " (anki--current-deck)) nil nil (anki--current-deck))
           (read-string "front: " nil nil "")
           (read-string (format "back (%s): " text) nil nil text)
           (read-string "tag: " nil nil))))
  (let ((body (json-encode
               (eanki--mk-action
                "addNote"
                (eanki--mk-params "Basic" deck front back tag)))))
    (message "%S" (anki--send body))))

;; http://tkf.github.io/emacs-request/manual.html
(defun eanki--send (body)
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

(defun eanki--test (beg end)
  (interactive "r")
  (message "%S" (buffer-substring-no-properties beg end)))

;; The region is the text between the point and the mark
(defun eanki--create-card-region (deck front back)
  (interactive
   (let ((text (buffer-substring-no-properties
		(region-beginning)
		(region-end))))
     (list (read-string "deck: " nil nil "")
	   (read-string "front: " nil nil "")
	   (read-string (format "back (%s): " text) nil nil text))))
  (message "%S %S %S" deck front back))

(defun eanki--create-card-line (deck front back)
  (interactive
   (let ((line (thing-at-point 'line t)))
     (list (read-string "deck: " nil nil "")
	   (read-string "front: " nil nil "")
	   (read-string "back (%s): " line) nil nil line)))
  (message "%S %S %S" deck front back))

