;;;


(require 'lsp-mode)

(add-to-list 'lsp-language-id-configuration '(move-mode . "move"))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection (lambda () "move-analyzer"))
  :major-modes '(move-mode)
  :priority 1
  :library-folders-fn (lambda (_workspace) "~/.move/")
  :server-id 'move-analyzer
  ))


(provide 'lsp-move)
