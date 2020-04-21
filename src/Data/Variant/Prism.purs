module Data.Variant.Prism (variant', variant) where

import Data.Symbol (class IsSymbol)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Prelude
import Data.Lens.Prism (Prism', prism', Prism, prism)
import Data.Variant (SProxy, Variant, default, on, inj)
import Prim.Row (class Cons)
import Unsafe.Coerce (unsafeCoerce)


variant'
  :: forall label r1 r a
   . Cons label a r r1
  => IsSymbol label
  => SProxy label
  -> Prism' (Variant ( | r1)) a
variant' fieldProxy =
  prism'
    (inj fieldProxy)
    (\ v ->
      (default (Nothing)
        # on fieldProxy (\a -> Just a)) v
    )


-- | Construct a (type-changing) lens for a variant case, by providing a
-- | proxy for the `Symbol` which corresponds to the case label.
-- |
-- | The lens is polymorphic in the rest of the row of case labels.
-- |
-- | For example:
-- |
-- | ```purescript
-- | label (SProxy :: SProxy "foo")
-- |   :: forall a b r. Prism (Variant ( foo :: a | r )) (Variant ( foo :: b | r )) a b
-- | ```label'
variant
  :: forall label r1 r2 r a b
   . Cons label a r r1
  => Cons label b r r2
  => IsSymbol label
  => SProxy label
  -> Prism (Variant ( | r1)) (Variant ( | r2)) a b
variant fieldProxy =
  prism
    (inj fieldProxy)
    (\ v ->
      ((on fieldProxy (\a -> Right a)))
        -- Used to use expand here, but it added an unneeded contraint
        (\ v2 -> Left (unsafeCoerce v2)) v)
