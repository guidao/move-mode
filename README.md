

# (WIP) Move-mode

A emacs major mode for move-lang

## Support Features

- major-mode
- lsp-move

## Installation

0. Install move-analyzer.
1. Clone this repo.
2. (add-to-list 'load-path "/path/to/repo")
3. Configuration.

```elisp
  (require 'move-mode)
  (require 'lsp-move)

  (add-hook 'move-mode-hook 'lsp)
```
