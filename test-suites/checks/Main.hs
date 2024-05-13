{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Prelude.Unicode

import Library

import Control.Monad.Trans.Writer.CPS
import Data.Text (Text)
import Data.Text qualified as Text
import Test.QuickCheck.Instances.Natural ()
import Test.Tasty
import Test.Tasty.QuickCheck

testWriter ∷ TestName → Writer ([TestTree] → [TestTree]) () → TestTree
testWriter name = testGroup name ∘ ($ []) ∘ execWriter

main ∷ IO ()
main = defaultMain do
  testWriter "checks" do
    write do testProperty "least significant reverse and first significant agree" do reverse ∘ bytesOfNaturalLeastSignificantFirst ↔ bytesOfNaturalMostSignificantFirst
    write do splitting "least significant first" naturalFromBytesLeastSignificantFirst bytesOfNaturalLeastSignificantFirst
    write do splitting "most significant first" naturalFromBytesMostSignificantFirst bytesOfNaturalMostSignificantFirst

class ExtensionalEquality α where
  isExtensionallyEqual ∷ α → α → Property

instance {-# OVERLAPPABLE #-} (Eq α, Show α, Arbitrary α) ⇒ ExtensionalEquality α where
  isExtensionallyEqual x y = property $ x === y

instance (ExtensionalEquality β, Show α, Arbitrary α) ⇒ ExtensionalEquality (α → β) where
  isExtensionallyEqual f g = property \x → f x ↔ g x

instance (ExtensionalEquality β, Show α, Arbitrary α) ⇒ ExtensionalEquality (Fun α β) where
  isExtensionallyEqual (applyFun → f) (applyFun → g) = property \x → f x ↔ g x

infix 4 ↔
(↔) ∷ (ExtensionalEquality α) ⇒ α → α → Property
(↔) = isExtensionallyEqual

splitting ∷ (ExtensionalEquality (sliver → sliver)) ⇒ Text → (whole → sliver) → (sliver → whole) → TestTree
splitting name overlay inlay = testProperty (unwords ["split", Text.unpack name]) do overlay ∘ inlay ↔ id

isomorphosis ∷ (ExtensionalEquality (α → α), ExtensionalEquality (β → β)) ⇒ Text → (α → β) → (β → α) → TestTree
isomorphosis name there back = testWriter (unwords ["isomorphosis", Text.unpack name]) do
  write do splitting "α → β → α" there back
  write do splitting "β → α → β" back there
