-- https://functional.works-hub.com/blog/Dependent-Types-in-Haskell-2

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Data.Kind
import Data.Either

type Body = [Char]

data Method
  = GET
  | POST
  deriving (Show)

data SMethod m where
  SGET  :: m ~ GET  => SMethod m
  SPOST :: m ~ POST => SMethod m

deriving instance Show (SMethod m)

type family IfGetThenUnitElseMaybeBody (m :: Method) :: Type where
  IfGetThenUnitElseMaybeBody GET = ()
  IfGetThenUnitElseMaybeBody POST = Maybe Body

-- this type should remind you of our ∑ type
-- Σ (x :: Bool) (if x then Int else String)
data Request m =
  Req (SMethod m)
      (IfGetThenUnitElseMaybeBody m)

mkSMethod :: Method -> Either (SMethod GET) (SMethod POST)
mkSMethod m =
    case m of
        GET  -> Left SGET
        POST -> Right SPOST

mkValidRequest :: Method -> Either (Request GET) (Request POST)
mkValidRequest m = do
  let requestBody = (Just "POST BODY" :: Maybe Body)
  let sm = mkSMethod m
  case sm of
    Left  SGET  -> Left $ Req SGET ()
    Right SPOST -> Right $ Req SPOST requestBody

main :: IO ()
main = do
  let x = mkValidRequest GET
      y = isLeft x
      x' = mkValidRequest POST
      y' = isRight x'
  putStrLn $ "hej: " ++ show y ++ ", " ++ show y'
  return ()
