-- https://functional.works-hub.com/blog/Dependent-Types-in-Haskell-2

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

{-
Req (SMethod m) (DependentType m)

  * GET

    Req (SMethod GET) (DependentType GET)
    Req SGET          ()

      (DependentType byts ut mot ())
      (SGET :: SMethod 'GET)

  * POST

    Req (SMethod POST) (DependentType POST)
    Req SPOST          (Maybe Body)

      (DependentType byts ut mot (Maybe Body))
      (SPOST :: SMethod 'POST)
-}

module Main where

import Data.Kind
import Data.Either

type Body = [Char]

data Method = GET | POST
  deriving (Show)

data SMethod m where
  SGET  :: SMethod GET
  SPOST :: SMethod POST

type family DependentType (m :: Method) :: Type where
  DependentType 'GET = ()
  DependentType 'POST = Body

-- this type should remind you of our ∑ type
-- Σ (x :: Bool) (if x then Int else String)
data Request m = Req (SMethod m) (DependentType m)

mkSMethod :: Method -> Either (SMethod GET) (SMethod POST)
mkSMethod m =
    case m of
        GET  -> Left SGET
        POST -> Right SPOST

mkValidRequest :: Method -> Either (Request GET) (Request POST)
mkValidRequest m = do
  let sm = mkSMethod m
  case sm of
    Left  SGET  -> Left $ Req SGET ()
    Right SPOST -> Right $ Req SPOST "POST BODY"

main :: IO ()
main = do
  let x = mkValidRequest GET
      y = isLeft x
      x' = mkValidRequest POST
      y' = isRight x'
  putStrLn $ "hej: " ++ show y ++ ", " ++ show y'
  return ()

apa :: () -> Request 'GET
apa = Req SGET

bepa :: Body -> Request 'POST
bepa = Req SPOST

-- to crash:
--   ghci> let x = Req SGET "get"
