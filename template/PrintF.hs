{-# LANGUAGE TemplateHaskell #-}
module PrintF where

-- NB: printf needs to be in a separate module to the one where
-- you intend to use it.

-- Import some Template Haskell syntax
import Language.Haskell.TH

-- Possible string tokens: %d %s and literal strings
data Format = D | S | L String
    deriving Show

-- a poor man's tokenizer
tokenize :: String -> [Format]
tokenize [] = []
tokenize ('%':'d':rest) = D : tokenize rest
tokenize ('%':'s':rest) = S : tokenize rest
tokenize (s:str) = L (s:p) : tokenize rest -- so we don't get stuck on weird '%'
    where (p, rest) = span (/= '%') str

-- only keep D and S in args
argsMapper :: (Format, Name) -> [PatQ]
argsMapper (L _, n) = []
argsMapper (_, n)   = [varP n]

-- generate argument list for the function
-- concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
args :: [Format] -> [PatQ]
args fmt = concatMap argsMapper (zip fmt names)
    where names = [ mkName $ 'x' : show i | i <- [0..] ]

bodyMapper :: Name -> Format -> ExpQ
bodyMapper _ (L s) = stringE s
bodyMapper n D     = appE [| show |] (varE n)
bodyMapper n S     = varE n

bodyFolder :: ExpQ -> ExpQ -> ExpQ
bodyFolder e e' = infixApp e [| (++) |] e'

-- generate body of the function
body :: [Format] -> ExpQ
body fmt = foldr bodyFolder (last exps) (init exps)
    where exps  = [ bodyMapper n f | (f,n) <- zip fmt names ]
          names = [ mkName $ 'x' : show i | i <- [0..] ]

-- glue the argument list and body together into a lambda
-- this is what gets spliced into the haskell code at the call
-- site of "printf"
printf :: String -> Q Exp
printf format = lamE (args fmt) (body fmt)
    where fmt = tokenize format
