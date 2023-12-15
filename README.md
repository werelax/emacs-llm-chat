### Example Configuration

```elisp
(load! "../lib/llm-api/llm-api.el")
(load! "../lib/llm-chat/llm-chat.el")

;; ------------------------------
;; llm-api

(setq *togetherai-token* "<token>")
(setq *openrouter-token* "<token>")
(setq *pplx-token* "<token>")

(setq llm-api-enabled-platforms `(:open-router ,(llm--create-open-router-platform *openrouter-token*)
                                  :ollama ,(llm--create-ollama-platform)
                                  :ollama-completion ,(llm--create-ollama-comp-platform)
                                  :pplx ,(llm--create-pplx-platform *pplx-token*)
                                  :together-ai ,(llm--create-together-ai-platform *togetherai-token*)
                                  :openchat-team ,(llm--create-openchat-team-platform)))

(setq *ollama-inference-servers*
      '((:lan-erebos . "http://erebos.local:11434")
        (:local . "http://localhost:11434")))

;; ------------------------------
;; llm-chat

(llm-chat-set-platforms llm-api-enabled-platforms)

;; ------------------------------
;; general ai interface

(defun ai-chat (prompt)
  "Send a message to the AI chat."
  (interactive "s> ")
  (llm-chat-msg prompt))

(defun ai-chat-about ()
  "Ask AI about selected region or current buffer."
  (interactive)
  (llm-chat-interactive-chat-about))

(defun ai-clear-history ()
  "Clear the chat history"
  (interactive)
  (llm-chat-clear-history))

(defun ai-kill-process ()
  "Kill the chat process (interrupt generation)"
  (interactive)
  (llm-chat-kill-process))

(defun ai-change-region (change-prompt)
  "Send prompt and region to the AI and replace the region with the results."
  (interactive "s> ")
  (llm-chat-change change-prompt))

(defun ai-select-model ()
  "Select the model (for the active platform)"
  (interactive)
  (llm-chat-select-model))

(defun ai-new-chat (prompt)
  "Clear history and starts a new thread."
  (interactive "s> ")
  (ai-clear-history)
  (ai-chat prompt))

;; keybindings
(map!
 :in "C-c a" #'ai-chat
 :in "C-c C-a" #'ai-new-chat
 :in "C-c s" #'ai-chat-about
 :in "C-c r" #'ai-change-region
 :in "C-c l" #'ai-select-model
 :in "C-c c" #'ai-clear-history
 :in "C-c k" #'ai-kill-process
 :in "C-c p" #'set-ai-provider
 :in "C-c C-s" #'llm-chat-set-system-prompt
 :in "C-c C-." #'llm-chat-show-system-prompt
 :in "C-c C-p" #'llm-chat-select-platform
 :in "C-c C-t" #'llm-chat-set-temperature)
```
