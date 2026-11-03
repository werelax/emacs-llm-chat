# llm-chat

Markdown chat buffer for [llm-api](https://github.com/werelax/emacs-llm-api):
streaming answers, collapsible tool-call and reasoning widgets, per-platform
branches, context compaction and region rewriting.

## Example configuration (Doom)

```elisp
(load! "../lib/llm-api/llm-api.el")
(load! "../lib/llm-chat/llm-chat.el")

(setq llm-api-enabled-platforms
      `(:openrouter ,(llm--create-open-router-platform "<token>")
        :minimax ,(llm--create-minimax-platform "<token>")
        :kimi ,(llm--create-kimi-platform "<token>")))

(setq llm-chat-default-platform-key :minimax)
(llm-chat-set-platforms llm-api-enabled-platforms)

;; optional: a directory with one system prompt per file
(setq llm-chat-system-prompts-directory "~/prompts")

(map!
 :in "C-c a" #'llm-chat-msg
 :in "C-c C-a" #'llm-chat-new-chat
 :in "C-c s" #'llm-chat-about
 :in "C-c b" #'llm-chat-new-chat-with-buffer
 :in "C-c r" #'llm-chat-change
 :in "C-c C-r" #'llm-chat-regenerate
 :in "C-c l" #'llm-chat-select-model
 :in "C-c c" #'llm-chat-clear-history
 :in "C-c k" #'llm-chat-kill-process
 :in "C-c p" #'llm-chat-select-platform
 :in "C-c C-s" #'llm-chat-set-system-prompt
 :in "C-c C-." #'llm-chat-show-system-prompt
 :in "C-c C-," #'llm-chat-select-system-prompt
 :in "C-c C-t" #'llm-chat-set-temperature)
```

Model lists are fetched from each provider on demand (and in the background
after the first message); `llm-chat-select-model` accepts any model id.

## Chat buffer keys (`llm-chat-mode`)

| Key | Command |
|---|---|
| `RET` (evil normal), `C-c C-c` | send a message |
| `r` (evil normal) | regenerate the last answer |
| `TAB` | toggle the widget at point |
| `;` (evil normal), `C-c C-;` | commit edits made in the buffer to the history |
| `C-c C-e` | commit edits, then regenerate |
| `<backspace>` (evil normal) | clear history |
| `q` (evil normal) | stop, clear and close |
| `C-c C-l` | show model limits and context usage |
| `C-c C-u` | refresh model metadata (`C-u` for all platforms) |
| `C-c C-m` / `C-c C-v` / `C-c C-k` | compact context / show / clear the summary |
| `C-c b c` / `C-c b e` | new branch / new branch from edited history |
| `C-c b s` / `C-c b n` / `C-c b p` | switch / next / previous branch |
