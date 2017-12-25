module hello where

open import Data.List
open import Data.Integer
open import Data.Nat
open import Relation.Binary.PropositionalEquality
open import Level

-- testar
apa : ℕ → ℕ
apa a = a

add : ℕ → ℕ
add x = x Data.Nat.+ 1
