{-# LANGUAGE TemplateHaskell #-}

module PrintF where

import Language.Haskell.TH

data Format = D | S | L String
  deriving Show
