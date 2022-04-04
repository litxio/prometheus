;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil
  (fill-column . 100)
  (tab-width . 4)
  (indent-tabs-mode . nil)
  )
 (haskell-mode
  (haskell-indentation-layout-offset . 4)
  (haskell-indentation-left-offset . 4)
  (haskell-indentation-starter-offset . 4)
  (haskell-process-type . stack-ghci)
  (haskell-stylish-on-save . nil)
  (lsp-haskell-formatting-provider . "fourmolu")
  (ormolu-process-path . "fourmolu")
  )
 (haskell-cabal-mode
  (haskell-process-type . stack-ghci)
  )
 (yaml-mode
  (tab-width . 2)
  )
 )
