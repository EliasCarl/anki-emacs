(require 'dash)
(require 'request)
(require 'json)

;; TODO Check if anki is running before doing anything

;; https://github.com/louietan/anki-editor/blob/master/anki-editor.el
;; https://github.com/glutanimate/anki-connect
;; https://foosoft.net/projects/anki-connect/

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
;; (org-element-at-point)
;; org-export-to-buffer
;; https://orgmode.org/manual/Exporting.html#Exporting
;; org-html-export-as-html

(defun eanki--mk-action (action &optional params)
  "Create an association list to be json-encoded and sent to anki
   over anki-connect."
  (let ((ls '()))
    (when params
      (push `(params . ,params) ls))
    (push `(action . ,action) ls)))

(defun eanki--mk-params (model deck front back &optional tags)
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

(defun eanki--html-linebreaks (str)
  (message
   (replace-regexp-in-string "\n" "<br>" str)))

(defun eanki--org-to-html ()
  (interactive)
  (unless (org-region-active-p) (user-error "No active region to replace"))
  (let ((region (buffer-substring-no-properties (region-beginning) (region-end))))
    (org-export-string-as region 'html t)))

(defun eanki--read-tagstring ()
  (let ((tags (buffer-substring-no-properties
               (line-beginning-position)
               (line-end-position))))
    (if (string-prefix-p "anki-tags:" tags)
        (cadr (split-string tags ":"))
      "")))

(defun eanki--read-current-level-tags ()
  (save-excursion
    (org-previous-visible-heading 1)
    (forward-line)
    (eanki--read-tagstring)))

(defun eanki--read-parents-level-tags ()
  (save-excursion
    ;; Change to "org-up-heading-safe", should be faster
    (if (ignore-errors (outline-up-heading 1))
        (progn
          (forward-line)
          (concat (eanki--read-tagstring)
                  ","
                  (eanki--read-parents-level-tags)))
      "")))

(defun eanki--read-default-tags ()
  "Read default tags as first line in each org mode level."
  (concat (eanki--read-current-level-tags)
          ","
          (eanki--read-parents-level-tags)))

(defun eanki--read-tags ()
  (let* ((defaults (eanki--read-default-tags))
         (other (read-string (format "tags (%s): " defaults) nil nil "")))
    ;; The variable "defaults" ends with a comma here, ugly but cant be bothered
    ;; to fix now since it works.
    (split-string (concat defaults other) ",")))

;; CLEAN UP!!!!!
(defun eanki--add-basic (deck front back tags)
  (interactive
   (let* ((text (buffer-substring-no-properties (region-beginning) (region-end)))
          (current-deck (eanki--current-deck)))
     (list (read-string (format "deck (%s): " current-deck) nil nil current-deck)
           (read-string "front: " nil nil "")
           (read-string (format "back (%s): " text) nil nil (org-export-string-as text 'html t))
           (eanki--read-tags))))
  (let ((body (json-encode
               (eanki--mk-action
                "addNote"
                (eanki--mk-params "Basic"
                                  deck
                                  front
                                  back
                                  tags)))))
    (message (eanki--send body))))

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

