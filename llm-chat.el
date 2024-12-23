;;; ../sync/doom.d/lib/llm-chat/llm-chat.el -*- lexical-binding: t; -*-

(require 'llm-api)

(defcustom llm-chat-buffer-name "*llm*" "Default llm-chat buffer."
  :group 'llm-chat
  :type 'string)

(defcustom llm-chat-user-nick "User" "User nick in logs."
  :group 'llm-chat
  :type 'string)

(defcustom llm-chat-assistant-nick "LLM" "Assistant nick in logs."
  :group 'llm-chat
  :type 'string)

(defcustom llm-chat-buffer-mode 'markdown-mode "Major mode for llm-chat logs."
  :group 'llm-chat
  :type 'function)

(defcustom llm-chat-spinner-type 'progress-bar "Spinner type for llm."
  :group 'llm-chat
  :type 'symbol)

(defvar llm-chat--buffer nil)
(defvar llm-chat--enabled-platforms '())
(defvar llm-chat--active-platform nil)

;; Message markers and properties
(defvar llm-chat--message-markers nil
  "List of message markers in the current buffer.
Each element is a plist with :type (user or assistant), :start and :end markers.")

(defun llm-chat--create-marker-pair (start end type)
  "Create marker pair for message of TYPE between START and END positions."
  (let ((start-marker (make-marker))
        (end-marker (make-marker)))
    (set-marker start-marker start)
    (set-marker end-marker end)
    (list :type type :start start-marker :end end-marker)))

(defun llm-chat--add-message-markers (start end type)
  "Add message markers for a message of TYPE between START and END."
  (push (llm-chat--create-marker-pair start end type)
        llm-chat--message-markers))

(defun llm-chat--clear-message-markers ()
  "Clear all message markers in the current buffer."
  (dolist (marker-pair llm-chat--message-markers)
    (set-marker (plist-get marker-pair :start) nil)
    (set-marker (plist-get marker-pair :end) nil))
  (setq llm-chat--message-markers nil))

(defun llm-chat--extract-messages ()
  "Extract all messages from the current buffer using markers.
Returns the history in the format expected by llm-api."
  (let (history
        (sorted-markers (sort (copy-sequence llm-chat--message-markers)
                              (lambda (a b)
                                (< (marker-position (plist-get a :start))
                                   (marker-position (plist-get b :start)))))))
    (dolist (marker sorted-markers)
      (let* ((type (plist-get marker :type))
             (start (plist-get marker :start))
             (end (plist-get marker :end))
             (content (string-trim (buffer-substring-no-properties start end)))
             (role (if (eq type 'user) :user :assistant)))
        (push `((:role . ,role)
                (:content . ,content))
              history)))
    (nreverse history)))

(defun llm-chat-commit-changes ()
  "Read current buffer state and update the chat history."
  (interactive)
  (let* ((history (llm-chat--extract-messages))
         (platform llm-chat--active-platform))
    (setf (llm-api--platform-history platform) history)
    (message "Chat history updated successfully")))

(defun llm-chat--msg (platform prompt)
  "Send PROMPT to llm PLATFORM."
  (let ((display-buffer-alist
         '(("\\*llm\\*"
            (llm-chat--display-buffer-reuse-llm-api-or-ellama-or-scratch)
            (reusable-frames . visible)))))
    (llm-chat--send platform prompt)))

(defun llm-chat--about (platform prompt)
  "Ask PLATFORM about PROMPT regarding the selected region or current buffer."
  (let ((text (if (region-active-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (buffer-substring-no-properties (point-min) (point-max)))))
    ;; (llm-chat--msg platform (format "Text:\n%s\nRegarding this text, %s" text prompt))
    (llm-chat--msg platform (format "Regarding this text, %s:\n\n```text\n%s\n```" prompt text))
    ))

(defun llm-chat--change (platform change-prompt)
  "Change selected text with PLATFORM according to provided CHANGE-PROMPT."
  (let* ((beg (if (region-active-p)
                  (region-beginning)
                (point-min)))
         (end (if (region-active-p)
                  (region-end)
                (point-max)))
         (text (buffer-substring-no-properties beg end)))
    (kill-region beg end)
    (llm-chat--stream platform
                      (format "Change the following text, %s, just output the final text without additional quotes around it:\n%s"
                              change-prompt text)
                      :on-finish (lambda (_buffer &rest args)
                                   (let ((end (plist-get args :end)))
                                     (when (string-match "\n\\'" text)
                                       ;; insert one \n if the regiond ended with one
                                       (save-excursion
                                         (goto-char end)
                                         (insert "\n")))))
                      :point beg)))

(defun llm-chat--clear-history (platform)
  "Clear chat history for PLATFORM."
  (let ((buffer llm-chat--buffer))
    (llm-api--clear-history platform)
    (llm-chat--clear-message-markers)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (erase-buffer)))
    (message "History cleared")))

(defun llm-chat--regenerate (platform)
  (llm-api--remove-last-from-history platform)
  (llm-chat--insert-platform-header platform)
  (llm-chat--msg platform "")
  (message "Regenerating"))

(defun llm-chat--select-model (platform)
  "Select a different model for PLATFORM."
  (let* ((models (llm-api--get-available-models platform))
         (choice (completing-read "Model: " models)))
    (llm-api--set-selected-model platform choice)
    (llm-chat--clear-history platform)
    (message "Model changed to %s" choice)))

(defun llm-chat--kill-process (platform)
  "Kill PLATFORM llm-api process."
  (let ((process (llm-api--platform-process platform))
        (buffer llm-chat--buffer))
    (when (process-live-p process)
      (delete-process process)
      (setf (llm-api--platform-process platform) nil)
      (when (buffer-live-p (get-buffer buffer))
        (with-current-buffer buffer
          (spinner-stop)))
      (message "Process killed"))))

;; keys

(defun llm-chat--keymap (platform buffer)
  "Keyjap for llm-api chat with PLATFORM in BUFFER."
  (with-current-buffer buffer
    (let ((quit-llm (lambda ()
                      (interactive)
                      (quit-window t)
                      (llm-chat--kill-process platform)
                      (llm-chat--clear-history platform)
                      (kill-buffer buffer)))
          (send-llm (lambda (p)
                      (interactive "s> ")
                      (llm-chat--msg platform p)))
          (clear-history (lambda () (llm-chat--clear-history platform)))
          (regenerate (lambda ()
                        (interactive)
                        (llm-chat--regenerate platform)))
          (commit-changes (lambda ()
                            (interactive)
                            (llm-chat-commit-changes))))
      (if (featurep 'evil)
          ;; evil
          (progn
            (evil-local-set-key 'normal (kbd "RET") send-llm)
            (evil-local-set-key 'normal (kbd "q") quit-llm)
            (evil-local-set-key 'normal (kbd "r") regenerate)
            (evil-local-set-key 'normal (kbd ";") commit-changes)
            (evil-local-set-key 'normal (kbd "C-c C-;") commit-changes)
            (evil-local-set-key 'normal (kbd "<backspace>") clear-history))
        ;; not evil
        (let ((chat-keymap (copy-keymap (current-local-map))))
          (use-local-map chat-keymap)
          (local-set-key (kbd "RET") send-llm)
          (local-set-key (kbd "q") quit-llm)
          (local-set-key (kbd "r") regenerate)
          (local-set-key (kbd "C-c C-;") commit-changes)
          (local-set-key (kbd "<backspace>") clear-history))))))

;; private functions

(defun llm-chat--display-buffer-reuse-llm-api-or-ellama-or-scratch (buffer alist)
  "Display BUFFER ALIST.
Preferring a window showing '*llm*', '*poe*', '*ellama*' or '*scratch*'. Scrolls
to the end to make the answer visible."
  (let ((window (or
                 ;; Try to reuse a window that's already showing *poe*.
                 (display-buffer-reuse-window buffer alist)
                 ;; If no window is showing *ellama*/*poe*, try to reuse one showing *scratch* or *ellama*.
                 (let ((scratch-window (or (get-buffer-window "*ellama*" 'visible)
                                           (get-buffer-window "*poe*" 'visible)
                                           (get-buffer-window "*scratch*" 'visible))))
                   (when scratch-window
                     ;; If we found a window, display the buffer there.
                     (window--display-buffer buffer scratch-window 'reuse alist)))
                 ;; If no suitable window is found, fallback to default behavior.
                 (let ((display-buffer-alist nil))
                   (display-buffer-pop-up-window buffer alist)))))
    ;; Scroll to make the last line of the buffer the first line in the window.
    (when window
      (with-selected-window window
        (goto-char (point-max))
        (set-window-start window (point-max))))
    window))

(defun llm-chat--send (platform prompt)
  "Send PROMPT to PLATFORM."
  (let ((buf-name (or llm-chat-buffer-name "*llm*"))
        (buffer llm-chat--buffer))
    (when (not (buffer-live-p buffer))
      (setq llm-chat--buffer (get-buffer-create buf-name))
      (setq buffer llm-chat--buffer))
    (with-current-buffer llm-chat--buffer
      (display-buffer llm-chat--buffer)
      (llm-chat--keymap platform llm-chat--buffer)
      ;; (goto-char (point-max))
      (unless (eq major-mode llm-chat-buffer-mode)
        (funcall llm-chat-buffer-mode))
      (when (not (string-empty-p prompt))

        ;; Insert user message header
        (goto-char (point-max))
        (insert (format "## %s:\n\n" llm-chat-user-nick))

        ;; Add markers for user message content
        (let ((start (point)))
          (insert prompt "\n\n")
          (llm-chat--add-message-markers start (point) 'user))

        ;; Insert assistant header
        (insert (format "## %s (%s):\n\n"
                        llm-chat-assistant-nick
                        (llm-api--get-model-name platform
                                                 (llm-api--platform-selected-model platform))))
        
        ;; Add markers for assistant message (content will be streamed)
        (goto-char (point-max))
        (let ((assist-start (point)))
          (llm-chat--add-message-markers assist-start assist-start 'assistant))

        (let ((windows (get-buffer-window-list buffer nil t)))
          (dolist (window windows)
            (set-window-point window (point-max))
            (with-selected-window window (recenter 3)))))

      (let* ((on-insert (lambda (buffer &rest args)
                          ;; Update the end marker of the last assistant message
                          (when-let ((last-marker (car llm-chat--message-markers)))
                            (when (eq (plist-get last-marker :type) 'assistant)
                              (set-marker (plist-get last-marker :end) (point-max))))
                          ;; NOTE: this code was commented to disable automatic scroll when generating
                          ;; (when-let (window (get-buffer-window buffer 'visible))
                          ;;   (with-selected-window window
                          ;;     (goto-char (point-max))))
                          ))
             (on-finish (lambda (buffer &rest args)
                          (with-current-buffer buffer
                            (save-excursion
                              (goto-char (point-max))
                              (insert "\n\n"))
                            (funcall on-insert buffer)
                            (spinner-stop)))))
        (llm-chat--stream platform
                          prompt
                          :point (point-max)
                          :on-insert on-insert
                          :on-finish on-finish)))))

(defun llm-chat--insert-platform-header (platform)
  "Insert PLATFORM header."
  (let ((buf-name (or llm-chat-buffer-name "*llm*"))
        (buffer llm-chat--buffer))
    (when (not (buffer-live-p buffer))
      (setq llm-chat--buffer (get-buffer-create buf-name)))
    (with-current-buffer llm-chat--buffer
      (insert (format "## %s (%s):\n\n"
                      llm-chat-assistant-nick
                      (llm-api--get-model-name platform
                                               (llm-api--platform-selected-model platform)))))))

(defun llm-chat--stream (platform prompt &rest args)
  "Query PLATFORM for PROMPT.
ARGS contains keys for fine control.

:buffer BUFFER -- BUFFER is the buffer (or `buffer-name') to insert ellama reply
in. Default value is (current-buffer).

:point POINT -- POINT is the point in buffer to insert ellama reaply at."
  (let* ((buffer (or (plist-get args :buffer) (current-buffer)))
         (point (or (plist-get args :point)
                    (with-current-buffer buffer (point))))
         (on-insert (plist-get args :on-insert))
         (on-finish (plist-get args :on-finish)))
    (with-current-buffer buffer
      (save-excursion
        (let* ((start (make-marker))
               (end (make-marker))
               (insert-text (lambda (text)
                              ;; Remove the annoying leading space
                              (when (= start end)
                                (setq text (s-trim-left text)))
                              ;; Insert buffer at the end
                              (with-current-buffer (marker-buffer end)
                                (save-excursion
                                  (goto-char end)
                                  (insert text))
                                ;; scrolls the window if visible
                                (when on-insert (funcall on-insert buffer
                                                         :start start
                                                         :end end
                                                         :text text))))))
          (set-marker start point)
          (set-marker end point)
          (set-marker-insertion-type start nil)
          (set-marker-insertion-type end t)
          (spinner-start llm-chat-spinner-type)
          (llm-api--generate-streaming platform
                                       prompt
                                       :on-data insert-text
                                       :on-finish (lambda ()
                                                    ;; (message "last:" (llm-api--platform-last-response platform))
                                                    (llm-api--add-generated-message-to-history platform)
                                                    (llm-api--on-generation-finish-hook platform)
                                                    ;; (message "HISTORY: %s" (llm-api--platform-history platform))
                                                    (with-current-buffer buffer
                                                      (when on-finish
                                                        (funcall on-finish buffer
                                                                 :start start
                                                                 :end end
                                                                 :text (buffer-substring-no-properties start end)))
                                                      (spinner-stop)))))))))

;; user interface

(defun llm-chat-set-platforms (platforms)
  (setq llm-chat--enabled-platforms platforms)
  (setq llm-chat--active-platform (cadr platforms)))

(defun llm-chat-select-platform ()
  (interactive)
  (let ((platforms (cl-loop for (key nil) on llm-chat--enabled-platforms by #'cddr
                            collect key)))
    (let ((choice (completing-read "> " platforms)))
      (setq llm-chat--active-platform (plist-get llm-chat--enabled-platforms (intern choice))))))

(defun llm-chat-msg (prompt)
  (llm-chat--msg llm-chat--active-platform prompt))

(defun llm-chat-regenerate ()
  (interactive)
  (llm-chat--regenerate llm-chat--active-platform))

(defun llm-chat-about (prompt)
  (llm-chat--about llm-chat--active-platform prompt))

(defun llm-chat-change (change-prompt)
  (llm-chat--change llm-chat--active-platform change-prompt))

(defun llm-chat-clear-history ()
  (interactive)
  (llm-chat--clear-history llm-chat--active-platform))

(defun llm-chat-kill-process ()
  (interactive)
  (llm-chat--kill-process llm-chat--active-platform))

(defun llm-chat-select-model ()
  (interactive)
  (llm-chat--select-model llm-chat--active-platform))

(defun llm-chat-interactive-chat-about ()
  (interactive)
  (let ((prompt (read-string "> ")))
    (llm-chat--about llm-chat--active-platform prompt)))

(defun llm-chat-set-system-prompt ()
  (interactive)
  (let ((prompt (read-string "system> ")))
    (setf (llm-api--platform-system-prompt llm-chat--active-platform) prompt)))

(defun llm-chat-get-system-prompt ()
  (message "SYSTEM: %s" (llm-api--platform-system-prompt llm-chat--active-platform)))

(defun llm-chat-show-system-prompt ()
  (interactive)
  (message "SYSTEM: %s" (llm-api--platform-system-prompt llm-chat--active-platform)))

(defun llm-chat-set-temperature ()
  (interactive)
  (let ((prompt (read-string "temp> ")))
    (setf (llm-api--platform-params llm-chat--active-platform)
          (plist-put (llm-api--platform-params llm-chat--active-platform) :temperature (string-to-number prompt)))))
