;;; har-viewer.el --- HAR file viewer/major mode -*- lexical-binding: t -*-
;;;
;;; Author: Gregory Newman <bozoslivehere@protonmail.com>
;;; Keywords: HAR HTTP Archive
;;;
;;; Commentary:
;;; For viewing HAR files.  Easily extract requests and responses.
;;; Adds a command to open the HAR viewer when files with the extension
;;; .har are open in the current buffer: C-c v

;;; Code:
(require 'evil)
(require 'json)
(require 'url-parse)

(define-derived-mode har-viewer-mode tabulated-list-mode "HAR Viewer"
  "Major mode for viewing HTTP Archive (HAR) files."
  (setq tabulated-list-format [("Protocol" 8 t)
                               ("Method" 6 t)
                               ("Domain" 25 t)
                               ("Path" 65 t)
                               ("Status" 7 t)
                               ("Content-Type" 20 t)
                               ("Size" 5 t)
                               ("Time" 10 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header)
  (define-key har-viewer-mode-map (kbd "RET") 'har-display-headers)
  (define-key har-viewer-mode-map (kbd "C-c R") 'har-display-request-body)
  (define-key har-viewer-mode-map (kbd "C-c r") 'har-display-response-body))

(defun harmode-parse-url (url)
  "Parse a blob URL, treating it as a blob plus a regular URL."
  (if (string-prefix-p "blob:" url)
      (let* ((inner-url (substring url 5))
             (parsed-url (url-generic-parse-url inner-url))
             (protocol (concat (url-type parsed-url) ":blob")))
        (setf (url-type parsed-url) protocol)
        parsed-url)
    (url-generic-parse-url url)))

(defvar har-entries-headers nil
  "A list of headers, stored as pairs of request and response headers.")

(defvar har-response-bodies nil
  "A list of response bodies.")

(defvar har-request-bodies nil
  "A list of request bodies.")

(defvar har-entries nil
  "All the entries!")

(defun parse-har-file ()
  "Parse the HAR file at FILE-PATH and extract relevant data."
  ;; (with-temp-buffer
  (save-excursion
    ;; (insert-file-contents file-path)
    (goto-char (point-min))
    (let* ((json-object-type 'alist)
           (json (json-read))
           (entry-id 0) ; Initialize a counter for unique IDs
           (entries (alist-get 'entries (alist-get 'log json))))
      (setq har-entries entries)
      (setq har-entries-headers ()) ; Reset headers storage for new file
      (setq har-response-bodies ())
      (setq har-request-bodies ())
      (setq har-entries ())
      (let ((parsed-entries
             (mapcar (lambda (entry)
                       (setq entry-id (1+ entry-id)) ; Increment the counter
                       (let* ((request (alist-get 'request entry))
                              (response (alist-get 'response entry))
                              (url (harmode-parse-url (alist-get 'url request)))
                              (status (number-to-string (alist-get 'status response)))
                              (method (alist-get 'method request))
                              (protocol (url-type url))
                              (domain (url-host url))
                              (path (url-filename url))
                              (content-type (or (alist-get 'mimeType (alist-get 'content response)) "[no content]"))
                              (size (alist-get 'size (alist-get 'content response)))
                              (time (alist-get 'time entry))
                              (request-headers (alist-get 'headers request))
                              (response-headers (alist-get 'headers response))
                              (req-headers-str (if request-headers
                                                   (mapcar (lambda (h)
                                                             (format "%s: %s" (alist-get 'name h) (alist-get 'value h)))
                                                           request-headers)
                                                 '("No request headers.")))
                              (res-headers-str (if response-headers
                                                   (mapcar (lambda (h)
                                                             (format "%s: %s" (alist-get 'name h) (alist-get 'value h)))
                                                           response-headers)
                                                 '("No response headers.")))
                              (request-post-mime-type (if (assoc 'postData request)
                                                          (alist-get 'mimeType (alist-get 'postData request))
                                                        ""))
                              (request-post-data (if (assoc 'postData request)
                                                     (if (> size 0)
                                                         (if (assoc 'text (alist-get 'postData request))
                                                             (cons request-post-mime-type (alist-get 'text (alist-get 'postData request)))
                                                           (cons request-post-mime-type "[no data]"))
                                                       (cons "Empty" "[Empty request]"))
                                                   (cons "" "Nothing to show here...")))
                              (response-body (if (and (> size 0) (assoc 'text (alist-get 'content response)))
                                                 (progn
                                                   (cons content-type (alist-get 'text (alist-get 'content response))))
                                               (cons "Empty" "[empty response]"))))

                         (push (list req-headers-str res-headers-str) har-entries-headers)
                         (push response-body har-response-bodies)
                         (push request-post-data har-request-bodies)
                         (list entry-id (vector protocol
                                                method
                                                domain
                                                path
                                                status
                                                content-type
                                                (format "%s" size)
                                                (format "%.0f ms" time)))))
                     entries)))
        (setq har-entries-headers (reverse har-entries-headers)) ; Reverse to maintain order
        (setq har-response-bodies (reverse har-response-bodies))
        (setq har-request-bodies (reverse har-request-bodies))
        parsed-entries))))


(defun har-view ()
  "View the HAR file (FILE-PATH) in `har-viewer-mode'."
  (interactive)
  (let* ((entries (parse-har-file))
         (buf (get-buffer-create "*HAR Viewer*")))
    (with-current-buffer buf
      (har-viewer-mode)
      (setq tabulated-list-entries entries)
      (tabulated-list-print t)
      (switch-to-buffer buf))))

(defun har-open-details-window ()
  "Open or switch to the *HAR Details* window."
  (interactive)
  (let ((details-window (get-buffer-window "*HAR Details*")))
    (if details-window
        (select-window details-window)
      (let ((details-buffer (get-buffer-create "*HAR Details*")))
        (display-buffer-below-selected details-buffer '((window.height . 0.3)))))))

(defun har-display-headers ()
  "Display the headers for the selected HAR entry."
  (interactive)
  (let* ((entry-id (1- (tabulated-list-get-id)))
         (headers-pair (nth entry-id har-entries-headers))
         (request-headers (car headers-pair))
         (response-headers (cadr headers-pair))
         (current-window (selected-window)))
    (har-open-details-window)
    (with-current-buffer "*HAR Details*"
      (read-only-mode -1)
      (erase-buffer)
      (insert "# Request Headers:\n")
      ;; Check if request-headers is a non-empty list of strings
      (if (and (listp request-headers) (stringp (car request-headers)))
          (dolist (header request-headers)
            (insert header "\n"))
        (insert "# No request headers.\n"))
      (insert "\n# Response Headers:\n")
      (if (and (listp response-headers) (stringp (car response-headers)))
          (dolist (header response-headers)
            (insert header "\n"))
        (insert "# No response headers.\n"))
      (goto-char (point-min))
      (restclient-mode)
      (select-window current-window))))

(defun har-display-response-body ()
  "Display the response body with in its corresponding major mode."
  (interactive)
  (let* ((entry-id (1- (tabulated-list-get-id)))
         (response-body (nth entry-id har-response-bodies))
         (mime-type (car response-body))
         (body-text (cdr response-body))
         (current-window (selected-window))
         (mode (mime-type-to-major-mode mime-type)))
    (har-open-details-window)
    (with-current-buffer "*HAR Details*"
      (read-only-mode -1)
      (erase-buffer)
      (insert body-text)
      (funcall mode)
      (goto-char (point-min))
      (beautify-by-mime-type mime-type)
      (select-window current-window))))

(defun har-display-request-body ()
  "Display the request body."
  (interactive)
  (let* ((entry-id (1- (tabulated-list-get-id)))
         (request-body (nth entry-id har-request-bodies))
         (mime-type (car request-body))
         (body-text (cdr request-body))
         (current-window (selected-window))
         (mode (mime-type-to-major-mode mime-type)))
    (har-open-details-window)
    (with-current-buffer "*HAR Details*"
      (read-only-mode -1)
      (erase-buffer)
      (insert body-text)
      (funcall mode)
      (goto-char (point-min))
      (beautify-by-mime-type mime-type)
      (select-window current-window))))

(defun mime-type-to-major-mode (mime-type)
  "Return the major mode associated with a given MIME-TYPE, if available."
  (cond ((string-match "javascript" mime-type) 'js-mode)
        ((string-match "json" mime-type) 'json-mode)
        ((string-match "html" mime-type) 'html-mode)
        ((string-match "xml" mime-type) 'xml-mode)
        ((string-match "css" mime-type) 'css-mode)
        (t 'text-mode)))

(defvar har-beautify-content-buffers nil)

(defun beautify-by-mime-type (mime-type)
  "Beautifies the buffer based on Content-Type's associated MIME-TYPE."
  (when (and (featurep 'web-beautify) (and (boundp 'har-beautify-content-buffers) har-beautify-content-buffers))
    (cond ((string-match "javascript" mime-type) (web-beautify-js-buffer))
          ((string-match "json" mime-type) (json-pretty-print-buffer))
          ((string-match "html" mime-type) (web-beautify-html-buffer))
          ((string-match "xml" mime-type) (web-beautify-html-buffer))
          ((string-match "css" mime-type) (web-beautify-css-buffer)))))


(defun my-har-viewer-mode-setup ()
  "Setup custom keybindings for `har-viewer-mode' in `evil-mode'."
  ;; Only setup if `evil-mode' is enabled
  (add-hook 'json-mode-hook 'my-har-viewer-mode-setup)
    (when (bound-and-true-p evil-mode)
      (evil-define-key 'normal har-viewer-mode-map (kbd "RET") 'har-display-headers)))

;; Ensure `my-har-viewer-mode-setup` is called when `har-viewer-mode` is entered
(add-hook 'har-viewer-mode-hook 'my-har-viewer-mode-setup)

;; (add-to-list 'magic-mode-alist '(is-har-file . har-viewer-mode))
(defun is-har-file ()
  "Check if the current buffer is a HAR file by checking its initial content."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "\\`\\s-*{" nil t)
        t  ; It looks like the start of a JSON object.
      nil)))  ; Not a HAR file.

(defun check-and-activate-har-viewer-command ()
  "Activate har-viewer-mode for .har files if is-har-file is true."
  (when (string-match "\\.har\\'" (buffer-file-name))
    (local-set-key (kbd "C-c v") 'har-view)))

(add-hook 'find-file-hook 'check-and-activate-har-viewer-command)

(provide 'har-viewer-mode)
;;; har-viewer.el ends here
