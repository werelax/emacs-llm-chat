;;; llm-chat.el --- Chat UI for llm-api -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Markdown chat buffer on top of llm-api: streaming, tool/reasoning widgets,
;; per-platform branches, context compaction, region rewriting.
;;
;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'spinner)
(require 'llm-api)

;; Load widgets from same directory
(eval-and-compile
  (require 'llm-chat-widgets
           (expand-file-name "llm-chat-widgets"
                             (file-name-directory
                              (or load-file-name
                                  (bound-and-true-p byte-compile-current-file)
                                  buffer-file-name)))))

(defgroup llm-chat nil
  "Chat with LLMs through llm-api."
  :group 'tools)

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

(defcustom llm-chat-compaction-tail-turns 5
  "How many recent user turns to keep verbatim when compacting context."
  :group 'llm-chat
  :type 'integer)

(defcustom llm-chat-compaction-warning-threshold 85
  "Warn when estimated/known context usage percent reaches this threshold."
  :group 'llm-chat
  :type 'integer)

(defcustom llm-chat-compaction-critical-threshold 95
  "Show critical warning when context usage percent reaches this threshold."
  :group 'llm-chat
  :type 'integer)

(defcustom llm-chat-compaction-timeout-seconds 120
  "Timeout in seconds for compaction summary generation."
  :group 'llm-chat
  :type 'integer)

(defcustom llm-chat-system-prompts-directory nil
  "Directory with one system prompt per file, for `llm-chat-select-system-prompt'."
  :group 'llm-chat
  :type '(choice (const :tag "None" nil) directory))

(defcustom llm-chat-change-system-prompt
  "You rewrite text exactly as instructed. Output only the final text, without quotes, fences or commentary."
  "System prompt used by `llm-chat-change' (region rewriting)."
  :group 'llm-chat
  :type 'string)

(defvar llm-chat--buffer nil)
(defvar llm-chat--enabled-platforms '())
(defvar llm-chat--active-platform nil)

(defvar llm-chat--branches-by-platform (make-hash-table :test 'eq)
  "Branch states keyed by platform object.")

(defvar llm-chat--compaction-jobs (make-hash-table :test 'eq)
  "Active compaction jobs keyed by platform object.")

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
                     :history (llm-chat--history-copy (llm-api--platform-history platform))
                     :context-compaction-summary nil
                     :context-compaction-version 0
                     :context-compaction-updated-at nil
                     :context-compaction-last-error nil
                     :context-compaction-last-warning-revision nil)
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

(defun llm-chat--branch-set-current-prop (platform key value)
  "Set current branch KEY to VALUE for PLATFORM."
  (let* ((state (llm-chat--branch-state platform))
         (branches (plist-get state :branches))
         (current-id (plist-get state :current-id))
         (branch (copy-tree (gethash current-id branches))))
    (when branch
      (puthash current-id (plist-put branch key value) branches)
      value)))

(defun llm-chat--branch-get-current-prop (platform key)
  "Get KEY from current PLATFORM branch." 
  (plist-get (llm-chat--branch-current platform) key))

(defun llm-chat--sync-platform-compaction-summary-from-branch (platform)
  "Apply current branch compaction summary to PLATFORM request context."
  (setf (llm-api--platform-context-compaction-summary platform)
        (llm-chat--branch-get-current-prop platform :context-compaction-summary)))

(defun llm-chat--set-current-branch-compaction-summary (platform summary)
  "Set current branch compaction SUMMARY for PLATFORM and sync to platform."
  (let* ((version (or (llm-chat--branch-get-current-prop platform :context-compaction-version) 0))
         (normalized (and (stringp summary) (string-trim summary))))
    (llm-chat--branch-set-current-prop platform :context-compaction-summary normalized)
    (llm-chat--branch-set-current-prop platform :context-compaction-version (1+ version))
    (llm-chat--branch-set-current-prop platform :context-compaction-updated-at (current-time-string))
    (llm-chat--branch-set-current-prop platform :context-compaction-last-error nil)
    (llm-chat--sync-platform-compaction-summary-from-branch platform)))

(defun llm-chat--clear-current-branch-compaction-summary (platform)
  "Clear compaction summary metadata on current PLATFORM branch."
  (llm-chat--branch-set-current-prop platform :context-compaction-summary nil)
  (llm-chat--branch-set-current-prop platform :context-compaction-last-error nil)
  (llm-chat--sync-platform-compaction-summary-from-branch platform))

(defun llm-chat--append-banner (platform text)
  "Append banner TEXT to llm chat buffer for PLATFORM."
  (when (buffer-live-p llm-chat--buffer)
    (with-current-buffer llm-chat--buffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (unless (bolp)
          (insert "\n"))
        (insert (format "> [!warning] %s\n\n" text))))
    (llm-chat--branch-save-current platform)))

(defun llm-chat--context-overflow-error-p (err-msg)
  "Return non-nil when ERR-MSG looks like a context-length overflow." 
  (and (stringp err-msg)
       (let ((msg (downcase err-msg)))
         (or (string-match-p "too many tokens" msg)
             (string-match-p "prompt is too long" msg)
             (string-match-p "context length" msg)
             (string-match-p "token limit" msg)
             (and (string-match-p "context" msg)
                  (or (string-match-p "window" msg)
                      (string-match-p "length" msg)
                      (string-match-p "limit" msg)
                      (string-match-p "exceed" msg)))))))

(defun llm-chat--maybe-show-context-pressure-warning (platform)
  "Append warning banner when PLATFORM context usage is near limit."
  (let* ((selected-model (llm-api--platform-selected-model platform))
         (usage (llm-api--get-context-usage platform selected-model))
         (pct (plist-get usage :percent-used))
         (rev (llm-api--platform-history-revision platform))
         (last-rev (llm-chat--branch-get-current-prop platform :context-compaction-last-warning-revision)))
    (when (and (numberp pct)
               (numberp rev)
               (or (not (numberp last-rev)) (/= last-rev rev))
               (>= pct llm-chat-compaction-warning-threshold))
      (llm-chat--branch-set-current-prop platform :context-compaction-last-warning-revision rev)
      (llm-chat--append-banner
       platform
       (if (>= pct llm-chat-compaction-critical-threshold)
           (format "Context usage is high (~%d%%). Consider running M-x llm-chat-compact-context soon." (truncate pct))
         (format "Context usage is rising (~%d%%). You can run M-x llm-chat-compact-context." (truncate pct)))))))

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
               (usage (llm-api--get-context-usage platform selected-model))
               (ctx (plist-get usage :context-window))
               (used (plist-get usage :used-tokens))
               (pct (plist-get usage :percent-used))
               (ctx-str (cond
                         ((and (numberp ctx) (> ctx 0) (numberp used) (numberp pct))
                          (format " | ~%s/%s (%d%%%%)"
                                  (llm-chat--format-token-count used)
                                  (llm-chat--format-token-count ctx)
                                  (truncate pct)))
                         ((and (numberp ctx) (numberp used))
                          (format " | ~%s/%s"
                                  (llm-chat--format-token-count used)
                                  (llm-chat--format-token-count ctx)))
                         ((numberp ctx)
                          (format " | %s" (llm-chat--format-token-count ctx)))
                         (t "")))
               (prefix (format " %s/" platform-name))
               (branch-str (format " | %s (#%s)" branch-name branch-id))
               (model-str model-name)
               (line "")
               (win (get-buffer-window (current-buffer) t))
               (width (max 20 (if (window-live-p win)
                                  (window-body-width win)
                                (frame-width)))))
          (setq line (concat prefix model-str ctx-str branch-str))
          (when (> (string-width line) width)
            (let ((available (- width
                                (string-width prefix)
                                (string-width ctx-str)
                                (string-width branch-str))))
              (setq model-str
                    (truncate-string-to-width model-str (max 8 available) nil nil "…"))
              (setq line (concat prefix model-str ctx-str branch-str))))
          (when (and (not (string-empty-p ctx-str)) (> (string-width line) width))
            (setq ctx-str "")
            (setq line (concat prefix model-str ctx-str branch-str)))
          (when (> (string-width line) width)
            (let ((available (- width
                                (string-width prefix)
                                (string-width branch-str))))
              (setq model-str
                    (truncate-string-to-width model-str (max 4 available) nil nil "…"))
              (setq line (concat prefix model-str ctx-str branch-str))))
          (setq-local header-line-format line))))))

(defun llm-chat--normalize-whitespace (text)
  "Collapse all whitespace runs in TEXT to single spaces and trim."
  (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " (or text ""))))

(defun llm-chat--history-text-projection (history)
  "Project HISTORY onto what the chat buffer shows.
Returns a list of (ROLE . TEXT) with tool messages dropped and the assistant
texts of one turn merged, whitespace-normalized."
  (let (out)
    (dolist (msg history)
      (let ((role (alist-get :role msg))
            (content (alist-get :content msg)))
        (pcase role
          (:user (push (cons :user (if (stringp content) content "")) out))
          (:assistant
           (when (stringp content)
             (if (eq (caar out) :assistant)
                 (setcdr (car out) (concat (cdar out) " " content))
               (push (cons :assistant content) out)))))))
    (seq-remove (lambda (entry) (and (eq (car entry) :assistant) (string-empty-p (cdr entry))))
                (mapcar (lambda (entry)
                          (cons (car entry) (llm-chat--normalize-whitespace (cdr entry))))
                        (nreverse out)))))

(defun llm-chat--buffer-edited-p (platform)
  "Return non-nil when the chat buffer text differs from PLATFORM history."
  (with-current-buffer llm-chat--buffer
    (not (equal (llm-chat--history-text-projection (llm-chat--extract-messages))
                (llm-chat--history-text-projection (llm-api--platform-history platform))))))

(defun llm-chat--sync-platform-history-from-chat-buffer (platform &optional only-if-edited)
  "Sync PLATFORM history from `llm-chat--buffer' marker extraction.
The buffer only shows user/assistant text, so syncing drops tool calls and
results.  With ONLY-IF-EDITED, sync only when the buffer was edited."
  (when (and platform (buffer-live-p llm-chat--buffer)
             (or (not only-if-edited) (llm-chat--buffer-edited-p platform)))
    (with-current-buffer llm-chat--buffer
      (setf (llm-api--platform-history platform) (llm-chat--extract-messages))
      (llm-api--bump-history-revision platform)
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
      (let ((inhibit-read-only t))
        (llm-chat--prepare-buffer platform)
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
              (insert (llm-chat--assistant-header platform))
              (let ((start (point)))
                (insert content "\n\n")
                (llm-chat--add-message-markers start (point) 'assistant))))))
        (goto-char (point-max))))
    (llm-chat--maybe-show-context-pressure-warning platform)))

(defun llm-chat--branch-create (platform &optional name)
  "Create and switch to a new branch on PLATFORM."
  (let* ((state (llm-chat--branch-state platform))
         (branches (plist-get state :branches))
         (parent-id (plist-get state :current-id))
         (parent-branch (gethash parent-id branches))
         (id (plist-get state :next-id))
         (branch-name (if (and name (not (string-empty-p name)))
                          name
                        (format "branch-%d" id)))
         (branch (list :id id
                       :parent-id parent-id
                       :name branch-name
                       :history (llm-chat--history-copy (llm-api--platform-history platform))
                       :context-compaction-summary (plist-get parent-branch :context-compaction-summary)
                       :context-compaction-version (or (plist-get parent-branch :context-compaction-version) 0)
                       :context-compaction-updated-at (plist-get parent-branch :context-compaction-updated-at)
                       :context-compaction-last-error nil
                       :context-compaction-last-warning-revision nil)))
    (puthash id branch branches)
    (plist-put state :next-id (1+ id))
    (plist-put state :current-id id)
    (llm-chat--sync-platform-compaction-summary-from-branch platform)
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
      (llm-chat--sync-platform-compaction-summary-from-branch platform)
      (llm-api--bump-history-revision platform)
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

(defun llm-chat--history-user-message-indices (history)
  "Return 0-based indices of user messages in HISTORY."
  (let ((idx 0)
        (indices '()))
    (dolist (msg history)
      (when (eq (alist-get :role msg) :user)
        (push idx indices))
      (setq idx (1+ idx)))
    (nreverse indices)))

(defun llm-chat--split-history-for-compaction (history tail-user-turns)
  "Split HISTORY into compactable prefix and protected tail.
Returns plist with keys :compactable, :tail and :split-index." 
  (let* ((user-indices (llm-chat--history-user-message-indices history))
         (user-count (length user-indices))
         (tail-user-turns (max 0 tail-user-turns)))
    (cond
     ;; Protect all user turns -> no-op.
     ((<= user-count tail-user-turns)
      (list :compactable nil :tail history :split-index 0))
     ;; Keep zero user turns -> compact the full history.
     ((= tail-user-turns 0)
      (list :compactable history :tail nil :split-index (length history)))
     (t
      (let* ((split-index (nth (- user-count tail-user-turns) user-indices))
             (compactable (seq-take history split-index))
             (tail (seq-drop history split-index)))
        (list :compactable compactable :tail tail :split-index split-index))))))

(defun llm-chat--message-content-string (value)
  "Convert message VALUE to readable string for compaction transcript."
  (cond
   ((null value) "")
   ((eq value :json-null) "")
   ((stringp value) value)
   (t (condition-case nil
          (json-encode value)
        (error (format "%s" value))))))

(defun llm-chat--serialize-message-for-compaction (msg)
  "Serialize one history MSG into markdown for compaction input."
  (let* ((role (alist-get :role msg))
         (role-name (pcase role
                      (:user "User")
                      (:assistant "Assistant")
                      (:tool "Tool")
                      (_ (format "%s" role))))
         (content (string-trim (llm-chat--message-content-string (alist-get :content msg))))
         (tool-calls (alist-get :tool_calls msg))
         (tool-call-id (alist-get :tool_call_id msg))
         (extras (delq nil
                       (list (when tool-call-id
                               (format "tool_call_id: %s" tool-call-id))
                             (when tool-calls
                               (format "tool_calls: %s"
                                       (llm-chat--message-content-string tool-calls)))))))
    (string-join
     (delq nil
           (list (format "### %s" role-name)
                 (unless (string-empty-p content) content)
                 (when extras (string-join extras "\n"))))
     "\n")))

(defun llm-chat--build-compaction-transcript (messages)
  "Build markdown transcript from compactable MESSAGES." 
  (string-join (mapcar #'llm-chat--serialize-message-for-compaction messages) "\n\n"))

(defun llm-chat--build-compaction-request-prompt (previous-summary transcript)
  "Build prompt for context compaction from PREVIOUS-SUMMARY and TRANSCRIPT."
  (string-join
   (delq nil
         (list
          "You are compacting previous chat history into durable memory for future turns."
          "Produce factual markdown only. Do not invent details."
          "Use exactly these sections:"
          "## Durable context"
          "## Decisions"
          "## Open tasks"
          "## Technical references"
          "## Risks / caveats"
          (when (and (stringp previous-summary) (not (string-empty-p (string-trim previous-summary))))
            (concat "Previous summary:\n\n" previous-summary))
          (concat "Conversation history to compact:\n\n" transcript)
          "Return only the updated summary markdown."))
   "\n\n"))

(defun llm-chat--clone-platform-for-compaction (platform)
  "Create a temporary PLATFORM clone for compaction summarization requests."
  (llm-api--clone-platform
   platform
   :system-prompt "You summarize previous conversation context for continuity."
   :process-buffer-name (format "*llm-compaction-%s*" (llm-api--platform-name platform))
   :tools []
   :tool-executor nil))

(defun llm-chat--compaction-job-running-p (platform)
  "Return non-nil when PLATFORM currently has an active compaction job." 
  (and (gethash platform llm-chat--compaction-jobs) t))

(defun llm-chat--clear-compaction-job (platform)
  "Clear active compaction job bookkeeping for PLATFORM." 
  (when-let* ((job (gethash platform llm-chat--compaction-jobs)))
    (when-let* ((timer (plist-get job :timeout-timer)))
      (cancel-timer timer))
    (remhash platform llm-chat--compaction-jobs)))

(defun llm-chat--start-compaction-job (platform prompt tail tail-user-turns revision-start branch-id-start)
  "Start asynchronous compaction for PLATFORM using PROMPT.
TAIL and TAIL-USER-TURNS are applied on success, guarded by
REVISION-START and BRANCH-ID-START."
  (let* ((worker (llm-chat--clone-platform-for-compaction platform))
         (summary "")
         (timeout-timer nil)
         (cleanup (lambda ()
                    (llm-chat--clear-compaction-job platform))))
    (setq timeout-timer
          (run-at-time llm-chat-compaction-timeout-seconds nil
                       (lambda ()
                         (when (llm-chat--compaction-job-running-p platform)
                           (llm-api--kill-process worker)
                           (funcall cleanup)
                           (when (and (eq platform llm-chat--active-platform)
                                      (= branch-id-start
                                         (or (plist-get (llm-chat--branch-current platform) :id) -1)))
                             (llm-chat--branch-set-current-prop platform :context-compaction-last-error
                                                                (format "Compaction timed out after %ss"
                                                                        llm-chat-compaction-timeout-seconds)))
                           (message "Context compaction timed out after %ss"
                                    llm-chat-compaction-timeout-seconds)))))
    (puthash platform (list :worker worker
                            :timeout-timer timeout-timer
                            :branch-id branch-id-start
                            :revision revision-start)
             llm-chat--compaction-jobs)
    (llm-api--add-user-message worker prompt)
    (llm-api--generate-streaming-from-history
     worker
     :on-data (lambda (text)
                (when (stringp text)
                  (setq summary (concat summary text))))
     :on-finish (lambda ()
                  (llm-api--add-response-to-history worker)
                  (setq summary (string-trim summary))
                  (funcall cleanup)
                  (cond
                   ((string-empty-p summary)
                    (when (and (eq platform llm-chat--active-platform)
                               (= branch-id-start
                                  (or (plist-get (llm-chat--branch-current platform) :id) -1)))
                      (llm-chat--branch-set-current-prop platform :context-compaction-last-error
                                                         "Compaction returned an empty summary"))
                    (message "Context compaction failed: empty summary"))
                   ((/= branch-id-start
                        (or (plist-get (llm-chat--branch-current platform) :id) -1))
                    (message "Compaction finished, but branch changed; discarding result"))
                   ((/= revision-start (llm-api--platform-history-revision platform))
                    (message "Compaction finished, but history changed; discarding result"))
                   (t
                    (llm-chat--set-current-branch-compaction-summary platform summary)
                    (llm-chat--branch-set-current-prop platform :context-compaction-last-warning-revision nil)
                    (setf (llm-api--platform-history platform) (llm-chat--history-copy tail))
                    (llm-api--bump-history-revision platform)
                    (llm-chat--branch-save-current platform)
                    (when (eq platform llm-chat--active-platform)
                      (llm-chat--render-history platform)
                      (llm-chat--append-banner
                       platform
                       (format "Context compacted successfully. Kept %d recent user turns and updated branch memory."
                               tail-user-turns)))
                    (message "Context compacted. Remaining messages: %d" (length tail))))))
     :on-error (lambda (msg _partial)
                 (funcall cleanup)
                 (when (and (eq platform llm-chat--active-platform)
                            (= branch-id-start
                               (or (plist-get (llm-chat--branch-current platform) :id) -1)))
                   (llm-chat--branch-set-current-prop platform :context-compaction-last-error
                                                      (format "Compaction failed: %s" msg)))
                 (message "Context compaction failed: %s" msg))))

(defun llm-chat-commit-changes ()
  "Read current buffer state and update the chat history."
  (interactive)
  (let ((platform llm-chat--active-platform))
    (when (llm-api--busy-p platform)
      (user-error "Cannot commit changes while generation is in progress"))
    (let ((history (llm-chat--extract-messages)))
      (setf (llm-api--platform-history platform) history)
      (llm-api--bump-history-revision platform)
      (llm-chat--branch-save-current platform)
      (message "Chat history updated successfully"))))

(defun llm-chat--msg (platform prompt)
  "Send PROMPT to llm PLATFORM."
  (let ((display-buffer-alist
         `((,(regexp-quote llm-chat-buffer-name)
            (llm-chat--display-buffer)
            (reusable-frames . visible)))))
    (llm-chat--send platform prompt)))

(defun llm-chat--about (platform prompt)
  "Ask PLATFORM about PROMPT regarding the selected region or current buffer."
  (let ((text (if (region-active-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (buffer-substring-no-properties (point-min) (point-max)))))
    (llm-chat--msg platform (format "Regarding this text, %s:\n\n```text\n%s\n```" prompt text))))

(defvar-local llm-chat--change-worker nil
  "Platform clone running a `llm-chat-change' rewrite in this buffer.")

(defun llm-chat--change (platform change-prompt)
  "Rewrite the region (or buffer) with PLATFORM according to CHANGE-PROMPT.
Uses a throwaway clone of PLATFORM: the chat history is not touched, tools
are disabled and reasoning is not inserted.  The original text is put in
the kill ring, and restored if the request fails."
  (when (and llm-chat--change-worker (llm-api--busy-p llm-chat--change-worker))
    (user-error "A rewrite is already running in this buffer"))
  (let* ((buffer (current-buffer))
         (beg (if (region-active-p) (region-beginning) (point-min)))
         (end (if (region-active-p) (region-end) (point-max)))
         (text (buffer-substring-no-properties beg end))
         (worker (llm-api--clone-platform platform
                                          :system-prompt llm-chat-change-system-prompt
                                          :tools []
                                          :tool-executor nil))
         (start-marker (copy-marker beg))
         (end-marker (copy-marker beg t))
         (at-start t))
    (kill-region beg end)
    (setq llm-chat--change-worker worker)
    (spinner-start llm-chat-spinner-type)
    (llm-api--add-user-message
     worker (format "Change the following text, %s:\n\n%s" change-prompt text))
    (llm-api--generate-streaming-from-history
     worker
     :on-data (lambda (chunk)
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (save-excursion
                      (goto-char end-marker)
                      (insert (if at-start (string-trim-left chunk) chunk))
                      (setq at-start (and at-start (string-blank-p chunk)))))))
     :on-finish (lambda ()
                  (when (buffer-live-p buffer)
                    (with-current-buffer buffer
                      ;; Keep the trailing newline the original text had.
                      (when (string-suffix-p "\n" text)
                        (save-excursion
                          (goto-char end-marker)
                          (unless (eq (char-before) ?\n) (insert "\n"))))
                      (spinner-stop)
                      (setq llm-chat--change-worker nil))))
     :on-error (lambda (err-msg _partial)
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (delete-region start-marker end-marker)
                     (save-excursion (goto-char start-marker) (insert text))
                     (spinner-stop)
                     (setq llm-chat--change-worker nil)))
                 (message "llm-chat change failed (text restored): %s" err-msg)))))

(defun llm-chat--clear-history (platform)
  "Clear chat history for PLATFORM."
  (let ((buffer llm-chat--buffer))
    (llm-api--clear-history platform)
    (llm-chat--clear-current-branch-compaction-summary platform)
    (llm-chat--branch-set-current-prop platform :context-compaction-last-warning-revision nil)
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
  "Regenerate the answer to the last user message on PLATFORM.
Drops the last assistant turn (including its tool calls); after a failed
request, simply retries the last user message."
  (when (llm-api--busy-p platform)
    (user-error "Generation is still running; wait or kill it first"))
  (unless (llm-api--truncate-history-after-last-user platform)
    (user-error "Nothing to regenerate"))
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

(defun llm-chat--refresh-platform-model-metadata (platform)
  "Refresh model metadata for PLATFORM (the selection is kept valid).
Returns plist (:count N :selected MODEL-ID :changed BOOL)."
  (let ((before (llm-api--platform-selected-model platform)))
    (llm-api--refresh-model-metadata platform)
    (let ((after (llm-api--platform-selected-model platform)))
      (list :count (length (llm-api--platform-available-models platform))
            :selected after
            :changed (not (equal before after))))))

(defun llm-chat--kill-process (platform)
  "Cancel PLATFORM's generation, including pending tool executions.
Also cancels a `llm-chat-change' rewrite running in the current buffer."
  (let ((killed nil))
    (when (and llm-chat--change-worker (llm-api--busy-p llm-chat--change-worker))
      (llm-api--kill-process llm-chat--change-worker)
      (setq llm-chat--change-worker nil)
      (spinner-stop)
      (setq killed t))
    (when (and platform (llm-api--busy-p platform))
      (llm-api--kill-process platform)
      (when (buffer-live-p llm-chat--buffer)
        (with-current-buffer llm-chat--buffer
          (spinner-stop)))
      (setq killed t))
    (message (if killed "Process killed" "Nothing to kill"))))

;; keys

(defvar llm-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'llm-chat-msg)
    (define-key map (kbd "C-c C-;") #'llm-chat-commit-changes)
    (define-key map (kbd "C-c C-e") #'llm-chat-regenerate-from-edited-history)
    (define-key map (kbd "C-c C-l") #'llm-chat-show-model-limits)
    (define-key map (kbd "C-c C-u") #'llm-chat-refresh-model-metadata)
    (define-key map (kbd "C-c C-m") #'llm-chat-compact-context)
    (define-key map (kbd "C-c C-v") #'llm-chat-show-compaction-summary)
    (define-key map (kbd "C-c C-k") #'llm-chat-clear-compaction-summary)
    (define-key map (kbd "C-c b c") #'llm-chat-branch-new)
    (define-key map (kbd "C-c b e") #'llm-chat-branch-new-from-edited-history)
    (define-key map (kbd "C-c b s") #'llm-chat-branch-switch)
    (define-key map (kbd "C-c b n") #'llm-chat-branch-next)
    (define-key map (kbd "C-c b p") #'llm-chat-branch-prev)
    ;; Explicit widget toggles for terminal key handling consistency
    (define-key map (kbd "TAB") #'llm-chat-widget-toggle-at-point)
    (define-key map (kbd "<tab>") #'llm-chat-widget-toggle-at-point)
    map)
  "Keymap for `llm-chat-mode'.")

(with-eval-after-load 'evil
  (when (fboundp 'evil-define-key*)
    (evil-define-key* 'normal llm-chat-mode-map
      (kbd "RET") #'llm-chat-msg
      ;; Keep vertical movement aligned with visible lines; avoids landing on
      ;; hidden collapsed widget body newlines.
      (kbd "j") #'next-line
      (kbd "k") #'previous-line
      (kbd "TAB") #'llm-chat-widget-toggle-at-point
      (kbd "<tab>") #'llm-chat-widget-toggle-at-point
      (kbd "C-i") #'llm-chat-widget-toggle-at-point
      (kbd "q") #'llm-chat-quit
      (kbd "r") #'llm-chat-regenerate
      (kbd ";") #'llm-chat-commit-changes
      (kbd "<backspace>") #'llm-chat-clear-history)))

(with-eval-after-load 'which-key
  (when (fboundp 'which-key-add-keymap-based-replacements)
    (which-key-add-keymap-based-replacements llm-chat-mode-map
      "C-c b" "llm branches"
      "C-c b c" "create branch"
      "C-c b e" "branch from edited history"
      "C-c b s" "switch branch"
      "C-c b n" "next branch"
      "C-c b p" "previous branch"
      "C-c C-;" "commit edited history"
      "C-c C-e" "regenerate from edited history"
      "C-c C-l" "show model limits"
      "C-c C-u" "refresh model metadata"
      "C-c C-m" "compact context"
      "C-c C-v" "show compact summary"
      "C-c C-k" "clear compact summary")))

(define-minor-mode llm-chat-mode
  "Minor mode for the llm-chat buffer.

\\{llm-chat-mode-map}"
  :lighter " LLM"
  :keymap llm-chat-mode-map
  (when (fboundp 'evil-normalize-keymaps)
    (evil-normalize-keymaps)))

(defun llm-chat--prepare-buffer (platform)
  "Set up the current buffer as the chat buffer for PLATFORM."
  (unless (eq major-mode llm-chat-buffer-mode)
    (funcall llm-chat-buffer-mode)
    ;; Prevent font-lock from stripping widget text properties
    (setq-local font-lock-extra-managed-props
                (seq-difference font-lock-extra-managed-props
                                '(invisible keymap rear-nonsticky))))
  (unless llm-chat-mode
    (llm-chat-mode 1))
  (llm-chat--update-header-line platform))

(defun llm-chat--assistant-header (platform)
  "Return the markdown header for an assistant message of PLATFORM."
  (format "## %s (%s):\n\n"
          llm-chat-assistant-nick
          (or (llm-api--get-model-name platform (llm-api--platform-selected-model platform))
              "-")))

;; private functions

(defun llm-chat--display-buffer (buffer alist)
  "Display BUFFER (with ALIST) reusing a window showing it or *scratch*.
Scrolls to the end to make the answer visible."
  (let ((window (or (display-buffer-reuse-window buffer alist)
                    (when-let* ((scratch-window (get-buffer-window "*scratch*" 'visible)))
                      (window--display-buffer buffer scratch-window 'reuse alist))
                    (let ((display-buffer-alist nil))
                      (display-buffer-pop-up-window buffer alist)))))
    (when window
      (with-selected-window window
        (goto-char (point-max))
        (set-window-start window (point-max))))
    window))

(defun llm-chat--send (platform prompt)
  "Send PROMPT to PLATFORM."
  (unless platform
    (user-error "No active platform"))
  (when (llm-api--busy-p platform)
    (user-error "Generation is still running; wait or kill it before sending a new prompt"))
  ;; Warm the model metadata cache in the background (context window etc.).
  (llm-api--prefetch-models platform (lambda () (llm-chat--update-header-line platform)))
  (let ((buf-name (or llm-chat-buffer-name "*llm*"))
        (buffer llm-chat--buffer))
    (when (not (buffer-live-p buffer))
      (setq llm-chat--buffer (get-buffer-create buf-name))
      (setq buffer llm-chat--buffer))
    (with-current-buffer llm-chat--buffer
      (let ((window (display-buffer llm-chat--buffer)))
        (when (window-live-p window)
          (select-window window)))
      (llm-chat--prepare-buffer platform)
      (when (not (string-empty-p prompt))

        ;; Insert user message header
        (goto-char (point-max))
        (insert (format "## %s:\n\n" llm-chat-user-nick))

        ;; Add markers for user message content
        (let ((start (point)))
          (insert prompt "\n\n")
          (llm-chat--add-message-markers start (point) 'user))

        ;; Insert assistant header
        (insert (llm-chat--assistant-header platform))

        ;; Add markers for assistant message (content will be streamed)
        (goto-char (point-max))
        (let ((assist-start (point)))
          (llm-chat--add-message-markers assist-start assist-start 'assistant))

        (let ((windows (get-buffer-window-list buffer nil t)))
          (dolist (window windows)
            (set-window-point window (point-max))
            (with-selected-window window (recenter 3)))))

      (let* ((on-insert (lambda (buffer &rest _args)
                          ;; Update the end marker of the last assistant message
                          (with-current-buffer buffer
                            (when-let* ((last-marker (car llm-chat--message-markers)))
                              (when (eq (plist-get last-marker :type) 'assistant)
                                (set-marker (plist-get last-marker :end) (point-max)))))))
             (on-finish (lambda (buffer &rest _args)
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
      (goto-char (point-max))
      (insert (llm-chat--assistant-header platform)))))

(defun llm-chat--stream (platform prompt &rest args)
  "Query PLATFORM for PROMPT.
ARGS contains keys for fine control.

:buffer BUFFER -- BUFFER is the buffer (or `buffer-name') to insert the reply
in. Default value is (current-buffer).

:point POINT -- POINT is the point in buffer to insert the reply at.

:on-insert FN, :on-finish FN -- called as (FN BUFFER &rest PLIST)."
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
               (on-tool-done (lambda (idx _name result-str)
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
                          (spinner-stop))
                        (llm-chat--maybe-show-context-pressure-warning platform))
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
                       (when (llm-chat--context-overflow-error-p err-msg)
                         (llm-chat--append-banner
                          platform
                          "Context limit reached. Run M-x llm-chat-compact-context."))
                       (message "llm-chat error: %s" err-msg))
           :on-tool-start on-tool-start
           :on-tool-done on-tool-done
           :on-reasoning on-reasoning
           :on-reasoning-finalize on-reasoning-finalize))))))

;; user interface

(defun llm-chat-set-platforms (platforms)
  "Use PLATFORMS, a plist (:key PLATFORM ...), as the selectable platforms.
Keeps the active platform when still present, else picks
`llm-chat-default-platform-key' or the first one."
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
      (llm-chat--sync-platform-compaction-summary-from-branch llm-chat--active-platform)
      (llm-chat--update-header-line llm-chat--active-platform))))

(defun llm-chat-select-platform ()
  "Switch the active platform and show its conversation."
  (interactive)
  (let* ((keys (cl-loop for (key nil) on llm-chat--enabled-platforms by #'cddr
                        collect (symbol-name key)))
         (choice (completing-read "Platform: " keys nil t))
         (platform (plist-get llm-chat--enabled-platforms (intern choice))))
    (setq llm-chat--active-platform platform)
    (llm-chat--branch-state platform)
    (llm-chat--sync-platform-compaction-summary-from-branch platform)
    (llm-chat--render-history platform)
    (llm-api--prefetch-models platform (lambda () (llm-chat--update-header-line platform)))))

(defun llm-chat-msg (prompt)
  "Send PROMPT to the active platform."
  (interactive "s> ")
  (llm-chat--msg llm-chat--active-platform prompt))

(defun llm-chat-new-chat (prompt)
  "Clear the history and start a new conversation with PROMPT."
  (interactive "s> ")
  (llm-chat-clear-history)
  (llm-chat-msg prompt))

(defun llm-chat-new-chat-with-buffer ()
  "Start a new conversation whose first message is the current buffer."
  (interactive)
  (llm-chat-new-chat (buffer-substring-no-properties (point-min) (point-max))))

(defun llm-chat-regenerate ()
  "Regenerate the last answer."
  (interactive)
  (llm-chat--regenerate llm-chat--active-platform))

(defun llm-chat-about (prompt)
  "Ask the active platform PROMPT about the region (or the whole buffer)."
  (interactive "sAbout this text: ")
  (llm-chat--about llm-chat--active-platform prompt))

(defalias 'llm-chat-interactive-chat-about #'llm-chat-about)

(defun llm-chat-change (change-prompt)
  "Rewrite the region (or the whole buffer) following CHANGE-PROMPT."
  (interactive "sChange: ")
  (llm-chat--change llm-chat--active-platform change-prompt))

(defun llm-chat-clear-history ()
  "Clear the conversation of the active platform."
  (interactive)
  (llm-chat--clear-history llm-chat--active-platform))

(defun llm-chat-kill-process ()
  "Stop the running generation (and rewrite in the current buffer)."
  (interactive)
  (llm-chat--kill-process llm-chat--active-platform))

(defun llm-chat-select-model ()
  "Select the model of the active platform (any model id can be typed)."
  (interactive)
  (llm-chat--select-model llm-chat--active-platform))

(defun llm-chat-quit ()
  "Stop generation, clear the conversation and close the chat buffer."
  (interactive)
  (let ((platform llm-chat--active-platform)
        (buffer llm-chat--buffer))
    (when (buffer-live-p buffer)
      (dolist (window (get-buffer-window-list buffer nil t))
        (quit-window nil window)))
    (when platform
      (when (llm-api--busy-p platform)
        (llm-api--kill-process platform))
      (llm-chat--clear-history platform))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(defun llm-chat-compact-context (&optional tail-user-turns)
  "Manually compact old history into branch-scoped summary memory.
Keeps TAIL-USER-TURNS recent user turns verbatim (default from custom setting)."
  (interactive)
  (let* ((platform llm-chat--active-platform)
         (tail-user-turns (or tail-user-turns llm-chat-compaction-tail-turns)))
    (unless platform
      (user-error "No active platform"))
    (when (llm-api--busy-p platform)
      (user-error "Generation is still running; wait or kill it before compacting"))
    (when (llm-chat--compaction-job-running-p platform)
      (user-error "Context compaction is already running for this platform"))
    ;; Include direct edits from the chat buffer before compacting (only when
    ;; there are any: syncing drops tool calls/results from the history).
    (llm-chat--sync-platform-history-from-chat-buffer platform t)
    (let* ((history (llm-api--platform-history platform))
           (split (llm-chat--split-history-for-compaction history tail-user-turns))
           (compactable (plist-get split :compactable))
           (tail (plist-get split :tail)))
      (if (or (null compactable) (= (length compactable) 0))
          (message "Nothing to compact yet (history shorter than protected tail)")
        (let* ((revision-start (llm-api--platform-history-revision platform))
               (branch-id-start (or (plist-get (llm-chat--branch-current platform) :id) -1))
               (previous-summary (llm-chat--branch-get-current-prop platform :context-compaction-summary))
               (transcript (llm-chat--build-compaction-transcript compactable))
               (prompt (llm-chat--build-compaction-request-prompt previous-summary transcript)))
          (llm-chat--start-compaction-job platform prompt tail tail-user-turns revision-start branch-id-start)
          (message "Compacting context asynchronously (%d messages)..." (length compactable)))))))

(defun llm-chat-show-compaction-summary ()
  "Show current branch compaction summary in a temporary buffer."
  (interactive)
  (let* ((platform llm-chat--active-platform)
         (summary (and platform
                       (llm-chat--branch-get-current-prop platform :context-compaction-summary))))
    (unless platform
      (user-error "No active platform"))
    (if (or (not (stringp summary)) (string-empty-p (string-trim summary)))
        (message "No compaction summary stored for this branch")
      (with-output-to-temp-buffer "*llm-compaction-summary*"
        (princ summary)))))

(defun llm-chat-clear-compaction-summary ()
  "Clear compaction summary for the current branch."
  (interactive)
  (let ((platform llm-chat--active-platform))
    (unless platform
      (user-error "No active platform"))
    (llm-chat--clear-current-branch-compaction-summary platform)
    (llm-chat--branch-set-current-prop platform :context-compaction-last-warning-revision nil)
    (llm-chat--branch-save-current platform)
    (llm-chat--update-header-line platform)
    (message "Cleared branch compaction summary")))

(defun llm-chat-refresh-model-metadata (&optional refresh-all)
  "Refresh model metadata cache.
Without prefix arg, refresh only the active platform.
With prefix arg REFRESH-ALL, refresh all enabled platforms."
  (interactive "P")
  (if refresh-all
      (let ((rows '()))
        (cl-loop for (_k platform) on llm-chat--enabled-platforms by #'cddr
                 do (condition-case nil
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
  "Show known context/output limits and estimated current usage." 
  (interactive)
  (let* ((platform llm-chat--active-platform)
         (selected-model (llm-api--platform-selected-model platform))
         (model-name (llm-api--get-model-name platform selected-model))
         (caps (llm-api--get-model-capabilities platform selected-model))
         (usage (llm-api--get-context-usage platform selected-model))
         (ctx (plist-get caps :context-window))
         (max-out (plist-get caps :max-output-tokens))
         (src (plist-get caps :source))
         (used (plist-get usage :used-tokens))
         (remaining (plist-get usage :remaining-tokens))
         (pct (plist-get usage :percent-used)))
    (if (not caps)
        (message "No model capability metadata known for %s" model-name)
      (message "Model %s | context: %s | used: ~%s (%s%%) | remaining: ~%s | max-output: %s | source: %s"
               model-name
               (if (numberp ctx) (llm-chat--format-token-count ctx) "unknown")
               (if (numberp used) (llm-chat--format-token-count used) "unknown")
               (if (numberp pct) (number-to-string (truncate pct)) "?")
               (if (numberp remaining) (llm-chat--format-token-count remaining) "unknown")
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

(defun llm-chat-set-system-prompt (prompt)
  "Set the system PROMPT of the active platform."
  (interactive (list (read-string "system> " (llm-chat-get-system-prompt))))
  (setf (llm-api--platform-system-prompt llm-chat--active-platform) prompt))

(defun llm-chat-get-system-prompt ()
  "Return the system prompt of the active platform."
  (llm-api--platform-system-prompt llm-chat--active-platform))

(defun llm-chat-show-system-prompt ()
  "Show the system prompt of the active platform."
  (interactive)
  (message "SYSTEM: %s" (llm-chat-get-system-prompt)))

(defun llm-chat-select-system-prompt ()
  "Set the active platform's system prompt from a file.
Files are read from `llm-chat-system-prompts-directory'."
  (interactive)
  (let ((dir llm-chat-system-prompts-directory))
    (unless (and dir (file-directory-p dir))
      (user-error "Set `llm-chat-system-prompts-directory' to a directory of prompts"))
    (let* ((choice (completing-read "System prompt: "
                                    (directory-files dir nil "\\`[^.]") nil t))
           (prompt (with-temp-buffer
                     (insert-file-contents (expand-file-name choice dir))
                     (buffer-string))))
      (llm-chat-set-system-prompt prompt)
      (message "System prompt: %s" choice))))

(defun llm-chat-set-temperature (temperature)
  "Set the sampling TEMPERATURE of the active platform."
  (interactive (list (read-number "Temperature: "
                                  (or (plist-get (llm-api--platform-params llm-chat--active-platform)
                                                 :temperature)
                                      0.7))))
  (let ((platform llm-chat--active-platform))
    (setf (llm-api--platform-params platform)
          (plist-put (copy-sequence (llm-api--platform-params platform))
                     :temperature temperature))))

(provide 'llm-chat)
;;; llm-chat.el ends here
