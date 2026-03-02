;;; llm-chat-widgets.el --- Collapsible widgets for llm-chat -*- lexical-binding: t; -*-
;;
;; Provides collapsible tool call and reasoning widgets using overlays
;; for body visibility (immune to font-lock property stripping).

;;; Code:

(require 'cl-lib)

(defvar-local llm-chat-widgets nil
  "List of widget plists in the current buffer.")

(defvar-local llm-chat-widgets--counter 0
  "Auto-incrementing widget ID counter.")

(defvar llm-chat-widget-header-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'llm-chat-widget-toggle-at-point)
    (define-key map [tab] #'llm-chat-widget-toggle-at-point)
    (define-key map (kbd "RET") #'llm-chat-widget-toggle-at-point)
    (define-key map [mouse-1] #'llm-chat-widget-toggle-at-point)
    map)
  "Keymap for widget headers.")

;; Faces

(defface llm-chat-widget-header-face
  '((t :inherit font-lock-comment-face :weight bold))
  "Face for widget headers."
  :group 'llm-chat)

(defface llm-chat-widget-header-success-face
  '((t :inherit success :weight bold))
  "Face for successful widget headers."
  :group 'llm-chat)

(defface llm-chat-widget-header-error-face
  '((t :inherit error :weight bold))
  "Face for error widget headers."
  :group 'llm-chat)

(defface llm-chat-widget-body-face
  '((t :inherit font-lock-doc-face))
  "Face for widget body text."
  :group 'llm-chat)

;; Internal helpers

(defun llm-chat-widget--next-id ()
  "Return the next widget ID."
  (cl-incf llm-chat-widgets--counter))

(defun llm-chat-widget--find (id)
  "Find widget with ID in current buffer."
  (cl-find id llm-chat-widgets :key (lambda (w) (plist-get w :id))))

(defun llm-chat-widget--header-face (status)
  "Return face for widget header with STATUS."
  (pcase status
    (:success 'llm-chat-widget-header-success-face)
    (:error 'llm-chat-widget-header-error-face)
    (_ 'llm-chat-widget-header-face)))

(defun llm-chat-widget--format-tool-header (name status args-preview collapsed)
  "Format tool widget header for NAME, STATUS, ARGS-PREVIEW, COLLAPSED state."
  (let ((chevron (if collapsed ">" "v"))
        (status-str (pcase status
                      (:running "running...")
                      (:success "done")
                      (:error "error")
                      (_ "pending"))))
    (format "[%s %s: %s] %s" chevron name status-str
            (if (and args-preview (> (length args-preview) 0))
                (format "\"%s\"" (truncate-string-to-width args-preview 50))
              ""))))

(defun llm-chat-widget--format-reasoning-header (collapsed &optional token-count)
  "Format reasoning widget header for COLLAPSED state with optional TOKEN-COUNT."
  (let ((chevron (if collapsed ">" "v")))
    (if (and token-count (> token-count 0))
        (format "[%s Thinking (%d tokens)]" chevron token-count)
      (format "[%s Thinking...]" chevron))))

(defun llm-chat-widget--insert-header (start-pos header-text face id)
  "Insert HEADER-TEXT at START-POS with FACE and widget ID.
Moves point past the inserted text. Returns point after insertion."
  (goto-char start-pos)
  (let ((beg (point)))
    (insert header-text "\n")
    (put-text-property beg (point) 'face face)
    (put-text-property beg (point) 'keymap llm-chat-widget-header-keymap)
    (put-text-property beg (point) 'llm-chat-widget-id id)
    (put-text-property beg (point) 'rear-nonsticky
                        '(face keymap llm-chat-widget-id))
    (point)))

(defun llm-chat-widget--replace-header (widget header-text face)
  "Replace WIDGET header with HEADER-TEXT and FACE."
  (let* ((start (plist-get widget :start))
         (id (plist-get widget :id)))
    (save-excursion
      (goto-char start)
      (let ((header-end (line-end-position)))
        (delete-region start (1+ header-end))
        (llm-chat-widget--insert-header start header-text face id)))))

(defun llm-chat-widget--make-body-overlay (beg end collapsed)
  "Create an overlay from BEG to END for a widget body.
When COLLAPSED is non-nil, the overlay hides the body text."
  (let ((ov (make-overlay beg end nil t nil)))
    (overlay-put ov 'face 'llm-chat-widget-body-face)
    (overlay-put ov 'llm-chat-widget-body t)
    (when collapsed
      (overlay-put ov 'invisible t))
    ov))

;; Tool widgets

(defun llm-chat-widget-create-tool (buffer position name args-str)
  "Create a tool call widget in BUFFER at POSITION.
NAME is the tool name, ARGS-STR is the raw arguments string.
Returns the widget plist."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((id (llm-chat-widget--next-id))
             (args-preview (condition-case nil
                               (let* ((parsed (json-parse-string args-str
                                                                 :object-type 'plist))
                                      (first-val (cadr parsed)))
                                 (if (stringp first-val) first-val args-str))
                             (error args-str)))
             (header-text (llm-chat-widget--format-tool-header
                           name :running args-preview t))
             (body-text (format "Arguments: %s\nResult: (pending)\n" args-str))
             (start (make-marker))
             (body-ov nil)
             (inhibit-read-only t))
        (save-excursion
          (goto-char position)
          (set-marker start (point))
          ;; Header
          (llm-chat-widget--insert-header (point) header-text
                                          'llm-chat-widget-header-face id)
          ;; Body
          (let ((body-start (point)))
            (insert body-text)
            (setq body-ov (llm-chat-widget--make-body-overlay
                           body-start (point) t))))
        (set-marker-insertion-type start nil)
        (let ((widget (list :id id
                            :type 'tool
                            :name name
                            :status :running
                            :collapsed t
                            :args-str args-str
                            :args-preview args-preview
                            :start start
                            :body-ov body-ov)))
          (push widget llm-chat-widgets)
          widget)))))

(defun llm-chat-widget-update-tool (widget status result-str)
  "Update tool WIDGET with STATUS (:success or :error) and RESULT-STR."
  (let* ((start (plist-get widget :start))
         (body-ov (plist-get widget :body-ov))
         (buffer (and (markerp start) (marker-buffer start))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (name (plist-get widget :name))
              (args-preview (plist-get widget :args-preview))
              (collapsed (plist-get widget :collapsed))
              (face (llm-chat-widget--header-face status)))
          (plist-put widget :status status)
          ;; Replace header
          (llm-chat-widget--replace-header
           widget
           (llm-chat-widget--format-tool-header name status args-preview collapsed)
           face)
          ;; Replace body using overlay boundaries
          (let ((old-beg (overlay-start body-ov))
                (old-end (overlay-end body-ov)))
            (delete-overlay body-ov)
            (save-excursion
              (goto-char old-beg)
              (delete-region old-beg old-end)
              (let ((new-body (format "Arguments: %s\nResult: %s\n"
                                      (plist-get widget :args-str)
                                      (or result-str "(none)")))
                    (body-start (point)))
                (insert new-body)
                (plist-put widget :body-ov
                           (llm-chat-widget--make-body-overlay
                            body-start (point) collapsed))))))))))

;; Reasoning widgets

(defun llm-chat-widget-create-reasoning (buffer position)
  "Create a reasoning widget in BUFFER at POSITION.
Returns the widget plist."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((id (llm-chat-widget--next-id))
             (header-text (llm-chat-widget--format-reasoning-header t))
             (start (make-marker))
             (body-ov nil)
             (inhibit-read-only t))
        (save-excursion
          (goto-char position)
          (set-marker start (point))
          (llm-chat-widget--insert-header (point) header-text
                                          'llm-chat-widget-header-face id)
          ;; Create zero-length overlay at point — will grow as text is appended
          ;; front-advance=nil initially (so overlay grows when text is inserted at start)
          ;; rear-advance=t so appended text extends the overlay
          (setq body-ov (make-overlay (point) (point) nil nil t))
          (overlay-put body-ov 'face 'llm-chat-widget-body-face)
          (overlay-put body-ov 'llm-chat-widget-body t)
          (overlay-put body-ov 'invisible t))
        (set-marker-insertion-type start nil)
        (let ((widget (list :id id
                            :type 'reasoning
                            :collapsed t
                            :token-count 0
                            :start start
                            :body-ov body-ov)))
          (push widget llm-chat-widgets)
          widget)))))

(defun llm-chat-widget-append-reasoning (widget text)
  "Append TEXT to reasoning WIDGET body."
  (let* ((body-ov (plist-get widget :body-ov))
         (buffer (and body-ov (overlay-buffer body-ov))))
    (when (and buffer (buffer-live-p buffer))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (save-excursion
            ;; Insert at overlay end — overlay extends automatically (rear-advance t)
            (goto-char (overlay-end body-ov))
            (insert text))))
      ;; Approximate token count (words)
      (plist-put widget :token-count
                 (+ (or (plist-get widget :token-count) 0)
                    (length (split-string text "[ \t\n]+" t)))))))

(defun llm-chat-widget-finalize-reasoning (widget)
  "Finalize reasoning WIDGET — update header with token count."
  (let* ((start (plist-get widget :start))
         (body-ov (plist-get widget :body-ov))
         (buffer (and (markerp start) (marker-buffer start))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (collapsed (plist-get widget :collapsed))
              (token-count (or (plist-get widget :token-count) 0)))
          ;; Replace header with final token count
          (llm-chat-widget--replace-header
           widget
           (llm-chat-widget--format-reasoning-header collapsed token-count)
           'llm-chat-widget-header-face)
          ;; Fix overlay start — header replacement may have shifted it.
          ;; Body starts right after the header line.
          (save-excursion
            (goto-char (marker-position start))
            (forward-line 1)
            (let ((body-start (point))
                  (body-end (overlay-end body-ov)))
              ;; Ensure body ends with newline
              (when (and (> body-end body-start)
                         (not (= (char-before body-end) ?\n)))
                (goto-char body-end)
                (insert "\n")
                (setq body-end (point)))
              ;; Fix overlay to exact body region and stop growing
              (move-overlay body-ov body-start body-end))))))))

;; Toggle

(defun llm-chat-widget-toggle-at-point ()
  "Toggle the widget at point."
  (interactive)
  (let ((id (get-text-property (point) 'llm-chat-widget-id)))
    (when id
      (let ((widget (llm-chat-widget--find id)))
        (when widget
          (llm-chat-widget-toggle widget))))))

(defun llm-chat-widget-toggle (widget)
  "Toggle collapse state of WIDGET."
  (let* ((start (plist-get widget :start))
         (body-ov (plist-get widget :body-ov))
         (buffer (and (markerp start) (marker-buffer start))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let* ((inhibit-read-only t)
               (new-collapsed (not (plist-get widget :collapsed)))
               (type (plist-get widget :type))
               (id (plist-get widget :id)))
          (plist-put widget :collapsed new-collapsed)
          ;; Toggle body overlay visibility
          (when body-ov
            (if new-collapsed
                (overlay-put body-ov 'invisible t)
              (overlay-put body-ov 'invisible nil)))
          ;; Update header chevron
          (let* ((new-header
                  (if (eq type 'tool)
                      (llm-chat-widget--format-tool-header
                       (plist-get widget :name)
                       (plist-get widget :status)
                       (plist-get widget :args-preview)
                       new-collapsed)
                    (llm-chat-widget--format-reasoning-header
                     new-collapsed
                     (let ((tc (plist-get widget :token-count)))
                       (when (and tc (> tc 0)) tc)))))
                 (face (if (eq type 'tool)
                           (llm-chat-widget--header-face (plist-get widget :status))
                         'llm-chat-widget-header-face)))
            (llm-chat-widget--replace-header widget new-header face)))))))

(provide 'llm-chat-widgets)
;;; llm-chat-widgets.el ends here
