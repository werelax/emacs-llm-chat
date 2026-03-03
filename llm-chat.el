;;; ../sync/doom.d/lib/llm-chat/llm-chat.el -*- lexical-binding: t; -*-

(require 'llm-api)

;; Load widgets from same directory
(let ((base-dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "llm-chat-widgets.el" base-dir)))

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

(defcustom llm-chat-default-platform-key :minimax
  "Preferred default platform key when initializing platforms."
  :group 'llm-chat
  :type 'symbol)

(defvar llm-chat--buffer nil)
(defvar llm-chat--enabled-platforms '())
(defvar llm-chat--active-platform nil)

(defvar llm-chat--branches-by-platform (make-hash-table :test 'eq)
  "Branch states keyed by platform object.")

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

;; Branching

(defun llm-chat--history-copy (history)
  "Deep-copy chat HISTORY list."
  (copy-tree history))

(defun llm-chat--branch-init-state (platform)
  "Create initial branch state for PLATFORM."
  (let ((branches (make-hash-table :test 'eql)))
    (puthash 1 (list :id 1
                     :parent-id nil
                     :name "main"
                     :history (llm-chat--history-copy (llm-api--platform-history platform)))
             branches)
    (list :next-id 2 :current-id 1 :branches branches)))

(defun llm-chat--branch-state (platform)
  "Get or initialize branch state for PLATFORM."
  (or (gethash platform llm-chat--branches-by-platform)
      (let ((state (llm-chat--branch-init-state platform)))
        (puthash platform state llm-chat--branches-by-platform)
        state)))

(defun llm-chat--branch-current (platform)
  "Get current branch plist for PLATFORM."
  (let* ((state (llm-chat--branch-state platform))
         (branches (plist-get state :branches))
         (current-id (plist-get state :current-id)))
    (gethash current-id branches)))

(defun llm-chat--format-token-count (n)
  "Format token count N as compact human string."
  (cond
   ((not (numberp n)) "?")
   ((>= n 1000000) (format "%.1fM" (/ n 1000000.0)))
   ((>= n 1000) (format "%.1fk" (/ n 1000.0)))
   (t (number-to-string n))))

(defun llm-chat--update-header-line (platform)
  "Update chat buffer header-line metadata for PLATFORM."
  (when (buffer-live-p llm-chat--buffer)
    (with-current-buffer llm-chat--buffer
      (if (not platform)
          (setq-local header-line-format nil)
        (let* ((selected-model (llm-api--platform-selected-model platform))
               (model-name (if selected-model
                               (llm-api--get-model-name platform selected-model)
                             "-"))
               (platform-name (llm-api--platform-name platform))
               (branch (llm-chat--branch-current platform))
               (branch-id (or (plist-get branch :id) "?"))
               (branch-name (or (plist-get branch :name) "main"))
               (ctx (llm-api--get-model-context-window platform selected-model))
               (ctx-str (if (numberp ctx)
                            (format " | Ctx:%s" (llm-chat--format-token-count ctx))
                          "")))
          (setq-local header-line-format
                      (format " LLM:%s | Model:%s%s | Branch:%s (#%s)"
                              platform-name model-name ctx-str branch-name branch-id)))))))

(defun llm-chat--sync-platform-history-from-chat-buffer (platform)
  "Sync PLATFORM history from `llm-chat--buffer' marker extraction."
  (when (and platform (buffer-live-p llm-chat--buffer))
    (with-current-buffer llm-chat--buffer
      (setf (llm-api--platform-history platform) (llm-chat--extract-messages))
      (llm-chat--branch-save-current platform))))

(defun llm-chat--branch-save-current (platform)
  "Save PLATFORM current history snapshot into the active branch."
  (when platform
    (let* ((state (llm-chat--branch-state platform))
           (branches (plist-get state :branches))
           (current-id (plist-get state :current-id))
           (branch (gethash current-id branches)))
      (when branch
        (puthash current-id
                 (plist-put (copy-tree branch)
                            :history (llm-chat--history-copy (llm-api--platform-history platform)))
                 branches)))))

(defun llm-chat--render-history (platform)
  "Render PLATFORM history into `llm-chat--buffer', rebuilding markers."
  (let ((buf-name (or llm-chat-buffer-name "*llm*"))
        (history (llm-api--platform-history platform)))
    (unless (buffer-live-p llm-chat--buffer)
      (setq llm-chat--buffer (get-buffer-create buf-name)))
    (with-current-buffer llm-chat--buffer
      (let ((window (display-buffer llm-chat--buffer))
            (inhibit-read-only t))
        (when (window-live-p window)
          (select-window window))
        (unless (eq major-mode llm-chat-buffer-mode)
          (funcall llm-chat-buffer-mode)
          (setq-local font-lock-extra-managed-props
                      (seq-difference font-lock-extra-managed-props
                                      '(invisible keymap rear-nonsticky))))
        (llm-chat--keymap platform llm-chat--buffer)
        (llm-chat--update-header-line platform)
        (when (fboundp 'llm-chat-widget-clear-all)
          (llm-chat-widget-clear-all (current-buffer)))
        (erase-buffer)
        (llm-chat--clear-message-markers)
        (dolist (msg history)
          (let ((role (alist-get :role msg))
                (content (alist-get :content msg)))
            (cond
             ((and (eq role :user) (stringp content))
              (insert (format "## %s:\n\n" llm-chat-user-nick))
              (let ((start (point)))
                (insert content "\n\n")
                (llm-chat--add-message-markers start (point) 'user)))
             ((and (eq role :assistant) (stringp content))
              (insert (format "## %s (%s):\n\n"
                              llm-chat-assistant-nick
                              (llm-api--get-model-name platform
                                                       (llm-api--platform-selected-model platform))))
              (let ((start (point)))
                (insert content "\n\n")
                (llm-chat--add-message-markers start (point) 'assistant))))))
        (goto-char (point-max))))))

(defun llm-chat--branch-create (platform &optional name)
  "Create and switch to a new branch on PLATFORM."
  (let* ((state (llm-chat--branch-state platform))
         (branches (plist-get state :branches))
         (parent-id (plist-get state :current-id))
         (id (plist-get state :next-id))
         (branch-name (if (and name (not (string-empty-p name)))
                          name
                        (format "branch-%d" id)))
         (branch (list :id id
                       :parent-id parent-id
                       :name branch-name
                       :history (llm-chat--history-copy (llm-api--platform-history platform)))))
    (puthash id branch branches)
    (plist-put state :next-id (1+ id))
    (plist-put state :current-id id)
    branch))

(defun llm-chat--branch-switch-to (platform branch-id)
  "Switch PLATFORM to BRANCH-ID and render its history."
  (let* ((state (llm-chat--branch-state platform))
         (branches (plist-get state :branches))
         (branch (gethash branch-id branches)))
    (when branch
      (llm-chat--branch-save-current platform)
      (plist-put state :current-id branch-id)
      (setf (llm-api--platform-history platform)
            (llm-chat--history-copy (plist-get branch :history)))
      (llm-chat--render-history platform)
      branch)))

(defun llm-chat--extract-assistant-content (start end)
  "Extract assistant content between START and END, excluding widget regions."
  (let ((segments '())
        (seg-start nil)
        (pos start))
    (while (< pos end)
      (let ((widget-char-p (or (get-text-property pos 'llm-chat-widget-id)
                               (get-char-property pos 'llm-chat-widget-body))))
        (if widget-char-p
            (when seg-start
              (push (buffer-substring-no-properties seg-start pos) segments)
              (setq seg-start nil))
          (unless seg-start
            (setq seg-start pos))))
      (setq pos (1+ pos)))
    (when seg-start
      (push (buffer-substring-no-properties seg-start end) segments))
    (string-trim (apply #'concat (nreverse segments)))))

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
             (start (marker-position (plist-get marker :start)))
             (end (marker-position (plist-get marker :end)))
             (content (if (eq type 'assistant)
                          (llm-chat--extract-assistant-content start end)
                        (string-trim (buffer-substring-no-properties start end))))
             (role (if (eq type 'user) :user :assistant)))
        (push `((:role . ,role)
                (:content . ,content))
              history)))
    (nreverse history)))

(defun llm-chat-commit-changes ()
  "Read current buffer state and update the chat history."
  (interactive)
  (let* ((platform llm-chat--active-platform)
         (process (llm-api--platform-process platform)))
    (when (process-live-p process)
      (user-error "Cannot commit changes while generation is in progress"))
    (let ((history (llm-chat--extract-messages)))
      (setf (llm-api--platform-history platform) history)
      (llm-chat--branch-save-current platform)
      (message "Chat history updated successfully"))))

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
    (llm-chat--branch-save-current platform)
    (llm-chat--clear-message-markers)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (fboundp 'llm-chat-widget-clear-all)
          (llm-chat-widget-clear-all (current-buffer)))
        (erase-buffer)))
    (message "History cleared")))

(defun llm-chat--remove-last-assistant-from-buffer ()
  "Remove the last assistant block from `llm-chat--buffer' and marker list."
  (when (buffer-live-p llm-chat--buffer)
    (with-current-buffer llm-chat--buffer
      (when-let* ((marker-pair (seq-find (lambda (m) (eq (plist-get m :type) 'assistant))
                                          llm-chat--message-markers))
                  (start (marker-position (plist-get marker-pair :start))))
        (let ((delete-start (save-excursion
                              (goto-char start)
                              (if (re-search-backward "^## " nil t)
                                  (point)
                                start))))
          ;; Regenerate always targets the last assistant; trim from its header to EOB.
          (delete-region delete-start (point-max)))
        (set-marker (plist-get marker-pair :start) nil)
        (set-marker (plist-get marker-pair :end) nil)
        (setq llm-chat--message-markers (delq marker-pair llm-chat--message-markers))))))

(defun llm-chat--regenerate (platform)
  (llm-api--remove-last-from-history platform)
  (llm-chat--remove-last-assistant-from-buffer)
  (llm-chat--insert-platform-header platform)
  (with-current-buffer llm-chat--buffer
    (goto-char (point-max))
    (let ((assist-start (point)))
      (llm-chat--add-message-markers assist-start assist-start 'assistant)))
  (llm-chat--msg platform "")
  (message "Regenerating"))

(defun llm-chat--select-model (platform)
  "Select a different model for PLATFORM."
  (let* ((models (llm-api--get-available-models platform))
         (choice (completing-read "Model: " models)))
    (llm-api--set-selected-model platform choice)
    (llm-chat--clear-history platform)
    (llm-chat--update-header-line platform)
    (message "Model changed to %s" choice)))

(defun llm-chat--model-id (model)
  "Return canonical model id/name from MODEL string or plist."
  (if (stringp model)
      model
    (or (plist-get model :model)
        (plist-get model :id)
        (plist-get model :name))))

(defun llm-chat--model-entry-choice-name (entry)
  "Return a user-selectable model name for model ENTRY."
  (if (stringp entry)
      entry
    (or (plist-get entry :name)
        (plist-get entry :model)
        (plist-get entry :id))))

(defun llm-chat--model-entry-matches-id-p (entry model-id)
  "Return non-nil when model ENTRY matches MODEL-ID by id/model/name."
  (when (and entry model-id)
    (let ((entry-id (llm-chat--model-id entry))
          (entry-name (if (consp entry) (plist-get entry :name) entry)))
      (or (and (stringp entry-id) (string= entry-id model-id))
          (and (stringp entry-name) (string= entry-name model-id))))))

(defun llm-chat--refresh-platform-model-metadata (platform)
  "Refresh model metadata for PLATFORM and keep selection valid.
Returns plist (:count N :selected MODEL-ID :changed BOOL)."
  (let* ((before-id (llm-api--get-selected-model platform))
         (_ (llm-api--refresh-model-metadata platform))
         (models (llm-api--platform-available-models platform))
         (count (length models))
         (still-valid (and before-id
                           (seq-find (lambda (m)
                                       (llm-chat--model-entry-matches-id-p m before-id))
                                     models))))
    (unless still-valid
      (when-let* ((first (car models))
                  (choice (llm-chat--model-entry-choice-name first)))
        (llm-api--set-selected-model platform choice)))
    (let ((after-id (llm-api--get-selected-model platform)))
      (list :count count
            :selected after-id
            :changed (not (equal before-id after-id))))))

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

(defvar llm-chat--which-key-registered nil
  "Whether llm-chat which-key replacements are already registered.")

(setq llm-chat--which-key-registered nil)

(defun llm-chat--register-which-key-bindings ()
  "Register human-friendly which-key labels for llm-chat bindings."
  (when (and (not llm-chat--which-key-registered)
             (fboundp 'which-key-add-key-based-replacements))
    (which-key-add-key-based-replacements
      "C-c b" "llm branches"
      "C-c b c" "create branch"
      "C-c b s" "switch branch"
      "C-c b n" "next branch"
      "C-c b p" "previous branch"
      "C-c C-;" "commit edited history"
      "C-c C-l" "show model limits"
      "C-c C-r" "refresh model metadata")
    (setq llm-chat--which-key-registered t)))

(defun llm-chat--keymap (platform buffer)
  "Keyjap for llm-api chat with PLATFORM in BUFFER."
  (llm-chat--register-which-key-bindings)
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
            ;; Explicit widget toggles for terminal/evil key handling consistency
            (evil-local-set-key 'normal (kbd "TAB") #'llm-chat-widget-toggle-at-point)
            (evil-local-set-key 'normal (kbd "<tab>") #'llm-chat-widget-toggle-at-point)
            (evil-local-set-key 'normal (kbd "C-i") #'llm-chat-widget-toggle-at-point)
            (evil-local-set-key 'normal (kbd "q") quit-llm)
            (evil-local-set-key 'normal (kbd "r") regenerate)
            (evil-local-set-key 'normal (kbd ";") commit-changes)
            (evil-local-set-key 'normal (kbd "C-c C-;") commit-changes)
            (evil-local-set-key 'normal (kbd "C-c C-l") #'llm-chat-show-model-limits)
            (evil-local-set-key 'normal (kbd "C-c C-r") #'llm-chat-refresh-model-metadata)
            (evil-local-set-key 'normal (kbd "C-c b c") #'llm-chat-branch-new)
            (evil-local-set-key 'normal (kbd "C-c b s") #'llm-chat-branch-switch)
            (evil-local-set-key 'normal (kbd "C-c b n") #'llm-chat-branch-next)
            (evil-local-set-key 'normal (kbd "C-c b p") #'llm-chat-branch-prev)
            (evil-local-set-key 'normal (kbd "<backspace>") clear-history))
        ;; not evil
        (let ((chat-keymap (copy-keymap (current-local-map))))
          (use-local-map chat-keymap)
          (local-set-key (kbd "RET") send-llm)
          ;; Explicit widget toggles for terminal key handling consistency
          (local-set-key (kbd "TAB") #'llm-chat-widget-toggle-at-point)
          (local-set-key (kbd "<tab>") #'llm-chat-widget-toggle-at-point)
          (local-set-key (kbd "C-i") #'llm-chat-widget-toggle-at-point)
          (local-set-key (kbd "q") quit-llm)
          (local-set-key (kbd "r") regenerate)
          (local-set-key (kbd "C-c C-;") commit-changes)
          (local-set-key (kbd "C-c C-l") #'llm-chat-show-model-limits)
          (local-set-key (kbd "C-c C-r") #'llm-chat-refresh-model-metadata)
          (local-set-key (kbd "C-c b c") #'llm-chat-branch-new)
          (local-set-key (kbd "C-c b s") #'llm-chat-branch-switch)
          (local-set-key (kbd "C-c b n") #'llm-chat-branch-next)
          (local-set-key (kbd "C-c b p") #'llm-chat-branch-prev)
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
  (when (process-live-p (llm-api--platform-process platform))
    (user-error "Generation is still running; wait or kill it before sending a new prompt"))
  (let ((buf-name (or llm-chat-buffer-name "*llm*"))
        (buffer llm-chat--buffer))
    (when (not (buffer-live-p buffer))
      (setq llm-chat--buffer (get-buffer-create buf-name))
      (setq buffer llm-chat--buffer))
    (with-current-buffer llm-chat--buffer
      (let ((window (display-buffer llm-chat--buffer)))
        (when (window-live-p window)
          (select-window window)))
      ;; (goto-char (point-max))
      (unless (eq major-mode llm-chat-buffer-mode)
        (funcall llm-chat-buffer-mode)
        ;; Prevent font-lock from stripping widget text properties
        (setq-local font-lock-extra-managed-props
                    (seq-difference font-lock-extra-managed-props
                                   '(invisible keymap rear-nonsticky))))
      (llm-chat--keymap platform llm-chat--buffer)
      (llm-chat--update-header-line platform)
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
                          (when-let* ((last-marker (car llm-chat--message-markers)))
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
               ;; Widget state
               (tool-widgets (make-hash-table :test 'eql))
               (reasoning-widget-cell (list nil))
               ;; Insert one visual blank line before the next normal text
               ;; chunk after widget blocks (reasoning/tools).
               (widget-gap-pending (list nil))
               ;; Core text insertion
               (insert-text (lambda (text)
                              ;; Remove the annoying leading space
                              (when (= start end)
                                (setq text (s-trim-left text)))
                              ;; If widgets were just emitted/finalized, ensure one
                              ;; blank line before the first visible text chunk.
                              (when (and (car widget-gap-pending)
                                         (string-match-p "\\S-" text))
                                (with-current-buffer (marker-buffer end)
                                  (save-excursion
                                    (goto-char end)
                                    (let ((nl 0)
                                          (pos (point)))
                                      (while (and (> pos (point-min))
                                                  (eq (char-before pos) ?\n))
                                        (setq nl (1+ nl)
                                              pos (1- pos)))
                                      (cond
                                       ((>= nl 2) nil)
                                       ((= nl 1) (insert "\n"))
                                       (t (insert "\n\n"))))))
                                (setcar widget-gap-pending nil))
                              ;; Insert buffer at the end
                              (with-current-buffer (marker-buffer end)
                                (save-excursion
                                  (goto-char end)
                                  (insert text))
                                ;; scrolls the window if visible
                                (when on-insert (funcall on-insert buffer
                                                         :start start
                                                         :end end
                                                         :text text)))))
               ;; Widget callbacks
               (on-tool-start (lambda (idx name args-str)
                                (when (buffer-live-p buffer)
                                  (let ((widget (llm-chat-widget-create-tool
                                                 buffer (marker-position end)
                                                 name args-str)))
                                    (puthash idx widget tool-widgets)
                                    (setcar widget-gap-pending t)))))
               (on-tool-done (lambda (idx name result-str)
                               (when (buffer-live-p buffer)
                                 (let ((widget (gethash idx tool-widgets)))
                                   (when widget
                                     (llm-chat-widget-update-tool
                                      widget
                                      (if (string-prefix-p "[Error:" result-str) :error :success)
                                      result-str)
                                     (setcar widget-gap-pending t))))))
               (on-reasoning (lambda (text)
                               (when (buffer-live-p buffer)
                                 (unless (car reasoning-widget-cell)
                                   (setcar reasoning-widget-cell
                                           (llm-chat-widget-create-reasoning
                                            buffer (marker-position end))))
                                 (llm-chat-widget-append-reasoning
                                  (car reasoning-widget-cell) text))))
               (on-reasoning-finalize (lambda ()
                                        (when (and (buffer-live-p buffer)
                                                   (car reasoning-widget-cell))
                                          (llm-chat-widget-finalize-reasoning
                                           (car reasoning-widget-cell))
                                          ;; Ensure next normal content starts after
                                          ;; a visual blank line below widget blocks.
                                          (setcar widget-gap-pending t)
                                          ;; Reset for next tool-loop iteration
                                          (setcar reasoning-widget-cell nil)))))
          (set-marker start point)
          (set-marker end point)
          (set-marker-insertion-type start nil)
          (set-marker-insertion-type end t)
          (spinner-start llm-chat-spinner-type)
          ;; Explicit history mutation: generation now runs from current history.
          (when (not (string-empty-p prompt))
            (llm-api--add-user-message platform prompt))
          (llm-api--generate-streaming-from-history
           platform
           :on-data insert-text
           :on-finish (lambda ()
                        (llm-api--add-response-to-history platform)
                        (llm-chat--branch-save-current platform)
                        (with-current-buffer buffer
                          (when on-finish
                            (funcall on-finish buffer
                                     :start start
                                     :end end
                                     :text (buffer-substring-no-properties start end)))
                          (spinner-stop)))
           :on-error (lambda (err-msg _partial)
                       ;; Don't add partial response to history
                       ;; Insert error message in buffer
                       (with-current-buffer buffer
                         (save-excursion
                           (goto-char end)
                           (insert (format "\n\n[Error: %s]\n\n" err-msg)))
                         ;; Update assistant end marker
                         (when on-insert
                           (funcall on-insert buffer :start start :end end))
                         (spinner-stop))
                       ;; user message may already be in history; keep branch snapshot in sync
                       (llm-chat--branch-save-current platform)
                       (message "llm-chat error: %s" err-msg))
           :on-tool-start on-tool-start
           :on-tool-done on-tool-done
           :on-reasoning on-reasoning
           :on-reasoning-finalize on-reasoning-finalize))))))

;; user interface

(defun llm-chat-set-platforms (platforms)
  (setq llm-chat--enabled-platforms platforms)
  (let* ((current llm-chat--active-platform)
         (current-valid (cl-loop for (_k v) on platforms by #'cddr
                                 thereis (eq v current)))
         (preferred (plist-get platforms llm-chat-default-platform-key)))
    (setq llm-chat--active-platform
          (cond
           (current-valid current)
           (preferred preferred)
           ((cadr platforms))))
    (when llm-chat--active-platform
      (llm-chat--branch-state llm-chat--active-platform)
      (llm-chat--update-header-line llm-chat--active-platform))))

(defun llm-chat-select-platform ()
  (interactive)
  (let ((platforms (cl-loop for (key nil) on llm-chat--enabled-platforms by #'cddr
                            collect key)))
    (let ((choice (completing-read "> " platforms)))
      (setq llm-chat--active-platform (plist-get llm-chat--enabled-platforms (intern choice)))
      (llm-chat--branch-state llm-chat--active-platform)
      (llm-chat--render-history llm-chat--active-platform))))

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

(defun llm-chat-refresh-model-metadata (&optional refresh-all)
  "Refresh model metadata cache.
Without prefix arg, refresh only the active platform.
With prefix arg REFRESH-ALL, refresh all enabled platforms."
  (interactive "P")
  (if refresh-all
      (let ((rows '()))
        (cl-loop for (_k platform) on llm-chat--enabled-platforms by #'cddr
                 do (condition-case err
                        (let* ((result (llm-chat--refresh-platform-model-metadata platform))
                               (name (llm-api--platform-name platform)))
                          (push (format "%s:%d" name (plist-get result :count)) rows))
                      (error
                       (push (format "%s:ERR" (llm-api--platform-name platform)) rows))))
        (llm-chat--update-header-line llm-chat--active-platform)
        (message "Refreshed model metadata -> %s" (mapconcat #'identity (nreverse rows) ", ")))
    (let ((platform llm-chat--active-platform))
      (unless platform
        (user-error "No active platform"))
      (condition-case err
          (let* ((result (llm-chat--refresh-platform-model-metadata platform))
                 (count (plist-get result :count))
                 (selected (or (plist-get result :selected) "none"))
                 (changed (if (plist-get result :changed) " (selection updated)" "")))
            (llm-chat--update-header-line platform)
            (message "Refreshed %s models:%d selected:%s%s"
                     (llm-api--platform-name platform)
                     count selected changed))
        (error
         (message "Failed refreshing %s metadata: %s"
                  (llm-api--platform-name platform)
                  (error-message-string err)))))))

(defun llm-chat-show-model-limits ()
  "Show known context/output limits for current model."
  (interactive)
  (let* ((platform llm-chat--active-platform)
         (selected-model (llm-api--platform-selected-model platform))
         (model-name (llm-api--get-model-name platform selected-model))
         (caps (llm-api--get-model-capabilities platform selected-model))
         (ctx (plist-get caps :context-window))
         (max-out (plist-get caps :max-output-tokens))
         (src (plist-get caps :source)))
    (if (not caps)
        (message "No model capability metadata known for %s" model-name)
      (message "Model %s | context: %s | max-output: %s | source: %s"
               model-name
               (if (numberp ctx) (llm-chat--format-token-count ctx) "unknown")
               (if (numberp max-out) (llm-chat--format-token-count max-out) "unknown")
               (or src :unknown)))))

(defun llm-chat-branch-new (name)
  "Create and switch to a new chat branch from current history.
When NAME is empty, an automatic branch name is used."
  (interactive "sBranch name (empty=auto): ")
  (llm-chat--branch-save-current llm-chat--active-platform)
  (let ((branch (llm-chat--branch-create llm-chat--active-platform name)))
    (llm-chat--update-header-line llm-chat--active-platform)
    (message "Switched to branch %s (#%d)"
             (plist-get branch :name)
             (plist-get branch :id))))

(defun llm-chat-branch-switch ()
  "Switch to another branch for the active platform."
  (interactive)
  (llm-chat--branch-save-current llm-chat--active-platform)
  (let* ((state (llm-chat--branch-state llm-chat--active-platform))
         (branches (plist-get state :branches))
         (choices '()))
    (maphash (lambda (id branch)
               (push (cons (format "#%d %s (parent:%s, msgs:%d)%s"
                                   id
                                   (plist-get branch :name)
                                   (or (plist-get branch :parent-id) "-")
                                   (length (plist-get branch :history))
                                   (if (= id (plist-get state :current-id)) " *" ""))
                           id)
                     choices))
             branches)
    (let* ((choice (completing-read "Branch: " (mapcar #'car choices) nil t))
           (id (cdr (assoc choice choices)))
           (branch (and id (llm-chat--branch-switch-to llm-chat--active-platform id))))
      (when branch
        (message "Switched to branch %s (#%d)"
                 (plist-get branch :name)
                 (plist-get branch :id))))))

(defun llm-chat-branch-next ()
  "Cycle to next branch by numeric ID."
  (interactive)
  (llm-chat--branch-save-current llm-chat--active-platform)
  (let* ((state (llm-chat--branch-state llm-chat--active-platform))
         (branches (plist-get state :branches))
         (current-id (plist-get state :current-id))
         (ids '()))
    (maphash (lambda (id _branch) (push id ids)) branches)
    (setq ids (sort ids #'<))
    (when ids
      (let* ((idx (or (cl-position current-id ids) 0))
             (next-id (nth (mod (1+ idx) (length ids)) ids))
             (branch (llm-chat--branch-switch-to llm-chat--active-platform next-id)))
        (when branch
          (message "Switched to branch %s (#%d)"
                   (plist-get branch :name)
                   (plist-get branch :id)))))))

(defun llm-chat-branch-prev ()
  "Cycle to previous branch by numeric ID."
  (interactive)
  (llm-chat--branch-save-current llm-chat--active-platform)
  (let* ((state (llm-chat--branch-state llm-chat--active-platform))
         (branches (plist-get state :branches))
         (current-id (plist-get state :current-id))
         (ids '()))
    (maphash (lambda (id _branch) (push id ids)) branches)
    (setq ids (sort ids #'<))
    (when ids
      (let* ((idx (or (cl-position current-id ids) 0))
             (prev-id (nth (mod (1- idx) (length ids)) ids))
             (branch (llm-chat--branch-switch-to llm-chat--active-platform prev-id)))
        (when branch
          (message "Switched to branch %s (#%d)"
                   (plist-get branch :name)
                   (plist-get branch :id)))))))

(defun llm-chat-regenerate-from-edited-history ()
  "Commit current chat buffer edits, then regenerate assistant response."
  (interactive)
  (llm-chat--sync-platform-history-from-chat-buffer llm-chat--active-platform)
  (llm-chat--regenerate llm-chat--active-platform))

(defun llm-chat-branch-new-from-edited-history (name)
  "Commit current chat buffer edits, then create/switch to a new branch."
  (interactive "sBranch name (empty=auto): ")
  (llm-chat--sync-platform-history-from-chat-buffer llm-chat--active-platform)
  (llm-chat-branch-new name))

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

(provide 'llm-chat)
;;; llm-chat.el ends here
