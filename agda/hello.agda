-- http://www.konradvoelkel.com/wp-content/uploads/cheat_sheet.pdf
-- http://www.cse.chalmers.se/edu/year/2017/course/DAT350/LectureNotes.pdf
-- http://agda.readthedocs.io/en/v2.5.2/tools/emacs-mode.html

module hello where

open import Data.List
open import Data.Integer
open import Data.Nat
open import Relation.Binary.PropositionalEquality
open import Level

-- testar
apa : ℕ → ℕ
apa x = x

add : ℕ → ℕ
add x = x Data.Nat.+ 1
