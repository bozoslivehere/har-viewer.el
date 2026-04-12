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
  (define-key har-viewer-mode-map (kbd "RET")   #'har-display-headers)
  (define-key har-viewer-mode-map (kbd "C-c R") #'har-display-request-body)
  (define-key har-viewer-mode-map (kbd "C-c c") #'har-copy-as-curl)
  (define-key har-viewer-mode-map (kbd "C-c n") #'har-narrow-to-regex)
  (define-key har-viewer-mode-map (kbd "C-c r") #'har-display-response-body))

;;; Buffer-local state

(defvar-local har-viewer--source-buffer nil
  "The buffer from which this HAR viewer was opened.")

(defvar-local har-viewer--entries-headers nil
  "List of (REQUEST-HEADERS RESPONSE-HEADERS) string lists, one per entry.")

(defvar-local har-viewer--response-bodies nil
  "List of (MIME-TYPE . TEXT) cons cells, one per entry.")

(defvar-local har-viewer--request-bodies nil
  "List of (MIME-TYPE . TEXT) cons cells, one per entry.")

;;; Configuration

(defvar har-viewer-beautify-bodies nil
  "When non-nil, auto-format body buffers using `web-beautify' if available.")

;;; Internal helpers

(defun har-viewer--parse-url (url)
  "Parse URL, handling blob: URLs by preserving the blob marker in the type."
  (if (string-prefix-p "blob:" url)
      (let* ((inner-url   (substring url 5))
             (parsed-url  (url-generic-parse-url inner-url))
             (protocol    (concat (url-type parsed-url) ":blob")))
        (setf (url-type parsed-url) protocol)
        parsed-url)
    (url-generic-parse-url url)))

(defun har-viewer--parse-file ()
  "Parse the HAR JSON in the current buffer.
Returns a plist with keys :entries, :headers, :request-bodies,
and :response-bodies."
  (save-excursion
    (goto-char (point-min))
    (let* ((json-object-type 'alist)
           (json    (json-read))
           (entries (alist-get 'entries (alist-get 'log json)))
           (entry-id 0)
           all-headers all-responses all-requests
           (parsed-entries
            (mapcar
             (lambda (entry)
               (setq entry-id (1+ entry-id))
               (let* ((request      (alist-get 'request entry))
                      (response     (alist-get 'response entry))
                      (url          (har-viewer--parse-url (alist-get 'url request)))
                      (status       (number-to-string (alist-get 'status response)))
                      (method       (alist-get 'method request))
                      (protocol     (url-type url))
                      (domain       (url-host url))
                      (path         (url-filename url))
                      (content-type (or (alist-get 'mimeType (alist-get 'content response))
                                        "[no content]"))
                      (size         (alist-get 'size (alist-get 'content response)))
                      (time         (alist-get 'time entry))
                      (req-headers  (alist-get 'headers request))
                      (res-headers  (alist-get 'headers response))
                      (req-headers-str
                       (if req-headers
                           (mapcar (lambda (h)
                                     (format "%s: %s"
                                             (alist-get 'name h)
                                             (alist-get 'value h)))
                                   req-headers)
                         '("No request headers.")))
                      (res-headers-str
                       (if res-headers
                           (mapcar (lambda (h)
                                     (format "%s: %s"
                                             (alist-get 'name h)
                                             (alist-get 'value h)))
                                   res-headers)
                         '("No response headers.")))
                      (post-mime (if (assoc 'postData request)
                                     (alist-get 'mimeType (alist-get 'postData request))
                                   ""))
                      (request-body
                       (if (assoc 'postData request)
                           (if (> size 0)
                               (if (assoc 'text (alist-get 'postData request))
                                   (cons post-mime
                                         (alist-get 'text (alist-get 'postData request)))
                                 (cons post-mime "[no data]"))
                             (cons "Empty" "[Empty request]"))
                         (cons "" "Nothing to show here...")))
                      (response-body
                       (if (and (> size 0) (assoc 'text (alist-get 'content response)))
                           (cons content-type
                                 (alist-get 'text (alist-get 'content response)))
                         (cons "Empty" "[empty response]"))))
                 (push (list req-headers-str res-headers-str) all-headers)
                 (push response-body all-responses)
                 (push request-body all-requests)
                 (list entry-id (vector protocol method domain path status
                                        content-type
                                        (format "%s" size)
                                        (format "%.0f ms" time)))))
             entries)))
      (list :entries        (nreverse parsed-entries)
            :headers        (nreverse all-headers)
            :response-bodies (nreverse all-responses)
            :request-bodies  (nreverse all-requests)))))

(defun har-viewer--mime-type-to-mode (mime-type)
  "Return the major mode symbol appropriate for MIME-TYPE."
  (cond ((string-match "javascript" mime-type) 'js-mode)
        ((string-match "json"       mime-type) 'json-mode)
        ((string-match "html"       mime-type) 'html-mode)
        ((string-match "xml"        mime-type) 'xml-mode)
        ((string-match "css"        mime-type) 'css-mode)
        (t 'text-mode)))

(defun har-viewer--beautify (mime-type)
  "Format the current buffer according to MIME-TYPE using web-beautify.
Does nothing unless `har-viewer-beautify-bodies' is non-nil and
the `web-beautify' package is loaded."
  (when (and (featurep 'web-beautify) har-viewer-beautify-bodies)
    (cond ((string-match "javascript" mime-type) (web-beautify-js-buffer))
          ((string-match "json"       mime-type) (json-pretty-print-buffer))
          ((string-match "html"       mime-type) (web-beautify-html-buffer))
          ((string-match "xml"        mime-type) (web-beautify-html-buffer))
          ((string-match "css"        mime-type) (web-beautify-css-buffer)))))

(defun har-viewer--open-details-window ()
  "Switch to the *HAR Details* window, creating it below if needed."
  (let ((details-window (get-buffer-window "*HAR Details*")))
    (if details-window
        (select-window details-window)
      (display-buffer-below-selected
       (get-buffer-create "*HAR Details*")
       '((window-height . 0.3))))))

(defun har-viewer--setup-evil-keys ()
  "Add evil normal-state bindings for `har-viewer-mode' when evil is active."
  (when (bound-and-true-p evil-mode)
    (evil-define-key 'normal har-viewer-mode-map
      (kbd "RET") #'har-display-headers
      (kbd "yc")  #'har-copy-as-curl)))

;;; Commands

;;;###autoload
(defun har-view ()
  "Open a HAR viewer for the current buffer."
  (interactive)
  (let* ((source-buf (current-buffer))
         (data       (har-viewer--parse-file))
         (buf        (get-buffer-create "*HAR Viewer*")))
    (with-current-buffer buf
      (har-viewer-mode)
      (setq har-viewer--source-buffer  source-buf)
      (setq har-viewer--entries-headers  (plist-get data :headers))
      (setq har-viewer--response-bodies  (plist-get data :response-bodies))
      (setq har-viewer--request-bodies   (plist-get data :request-bodies))
      (setq tabulated-list-entries       (plist-get data :entries))
      (tabulated-list-print t)
      (switch-to-buffer buf))))

(defun har-display-headers ()
  "Display the headers for the HAR entry at point."
  (interactive)
  (let* ((entry-id        (1- (tabulated-list-get-id)))
         (headers-pair    (nth entry-id har-viewer--entries-headers))
         (request-headers (car headers-pair))
         (response-headers (cadr headers-pair))
         (current-window  (selected-window)))
    (har-viewer--open-details-window)
    (with-current-buffer "*HAR Details*"
      (read-only-mode -1)
      (erase-buffer)
      (insert "# Request Headers:\n")
      (if (and (listp request-headers) (stringp (car request-headers)))
          (dolist (header request-headers) (insert header "\n"))
        (insert "# No request headers.\n"))
      (insert "\n# Response Headers:\n")
      (if (and (listp response-headers) (stringp (car response-headers)))
          (dolist (header response-headers) (insert header "\n"))
        (insert "# No response headers.\n"))
      (goto-char (point-min))
      (when (fboundp 'restclient-mode) (restclient-mode))
      (select-window current-window))))

(defun har-display-response-body ()
  "Display the response body for the HAR entry at point."
  (interactive)
  (let* ((entry-id     (1- (tabulated-list-get-id)))
         (response     (nth entry-id har-viewer--response-bodies))
         (mime-type    (car response))
         (body-text    (cdr response))
         (current-window (selected-window))
         (mode         (har-viewer--mime-type-to-mode mime-type)))
    (har-viewer--open-details-window)
    (with-current-buffer "*HAR Details*"
      (read-only-mode -1)
      (erase-buffer)
      (insert body-text)
      (funcall mode)
      (goto-char (point-min))
      (har-viewer--beautify mime-type)
      (select-window current-window))))

(defun har-display-request-body ()
  "Display the request body for the HAR entry at point."
  (interactive)
  (let* ((entry-id     (1- (tabulated-list-get-id)))
         (request      (nth entry-id har-viewer--request-bodies))
         (mime-type    (car request))
         (body-text    (cdr request))
         (current-window (selected-window))
         (mode         (har-viewer--mime-type-to-mode mime-type)))
    (har-viewer--open-details-window)
    (with-current-buffer "*HAR Details*"
      (read-only-mode -1)
      (erase-buffer)
      (insert body-text)
      (funcall mode)
      (goto-char (point-min))
      (har-viewer--beautify mime-type)
      (select-window current-window))))

(defun har-copy-as-curl ()
  "Copy the HAR entry at point as a cURL command to the kill ring."
  (interactive)
  (let* ((entry-id      (1- (tabulated-list-get-id)))
         (entry-data    (tabulated-list-get-entry))
         (protocol      (replace-regexp-in-string ":blob$" "" (aref entry-data 0)))
         (method        (aref entry-data 1))
         (domain        (aref entry-data 2))
         (path          (aref entry-data 3))
         (headers-pair  (nth entry-id har-viewer--entries-headers))
         (req-headers   (car headers-pair))
         (request       (nth entry-id har-viewer--request-bodies))
         (body-text     (cdr request))
         (url           (format "%s://%s%s" protocol domain path))
         (parts         (list (format "curl -X '%s' \\\n  '%s'" method url))))
    (when (and (listp req-headers) (stringp (car req-headers)))
      (dolist (header req-headers)
        (setq parts (append parts
                            (list (format "  -H '%s'"
                                          (replace-regexp-in-string
                                           "'" "'\\''" header t t)))))))
    (when (and body-text
               (not (string= body-text "Nothing to show here..."))
               (not (string= body-text "[Empty request]"))
               (not (string= body-text "[no data]")))
      (setq parts (append parts
                          (list (format "  --data-raw '%s'"
                                        (replace-regexp-in-string
                                         "'" "'\\''" body-text t t))))))
    (kill-new (mapconcat #'identity parts " \\\n"))
    (message "Copied cURL command to kill ring.")))

(defun har-narrow-to-regex (regex)
  "Filter the HAR viewer to entries whose URL matches REGEX.
The full entry list is re-read from the source buffer so the
filter can be changed or cleared by calling this command again."
  (interactive "sNarrow to URL regex: ")
  (let* ((data    (with-current-buffer har-viewer--source-buffer
                    (har-viewer--parse-file)))
         (all     (plist-get data :entries))
         (filtered (seq-filter
                    (lambda (entry)
                      (let* ((v           (cadr entry))
                             (domain-path (concat (aref v 2) (aref v 3))))
                        (string-match-p regex domain-path)))
                    all)))
    (setq har-viewer--entries-headers  (plist-get data :headers))
    (setq har-viewer--response-bodies  (plist-get data :response-bodies))
    (setq har-viewer--request-bodies   (plist-get data :request-bodies))
    (setq tabulated-list-entries filtered)
    (tabulated-list-print t)))

;;; Integration hooks

(add-hook 'har-viewer-mode-hook #'har-viewer--setup-evil-keys)

(with-eval-after-load 'evil
  (declare-function evil-define-key "evil-core"))

;;;###autoload
(defun har-viewer--activate-for-file ()
  "Bind C-c v to `har-view' when the current file has a .har extension."
  (when (and (buffer-file-name)
             (string-match-p "\\.har\\'" (buffer-file-name)))
    (local-set-key (kbd "C-c v") #'har-view)))

;;;###autoload
(add-hook 'find-file-hook #'har-viewer--activate-for-file)

(provide 'har-viewer)
;;; har-viewer.el ends here
