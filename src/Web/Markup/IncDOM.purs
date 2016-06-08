module Web.Markup.IncDOM (renderTo, renderToBody) where
--------------------------------------------------------------------------------
import Prelude
import Data.Monoid
import Data.Maybe
import Data.Maybe.First
import Data.Foldable
import Data.Foreign
import Data.Nullable
import Control.Apply
import Control.Monad.Eff
import Web.Markup
import DOM (DOM ())
import DOM.HTML (window)
import DOM.HTML.Document (body)
import DOM.HTML.Window (document)
import DOM.HTML.Types (HTMLElement ())
import Data.Function.Eff

-- | Render `Markup` to a DOM element using virtual-dom.
renderTo
  :: forall eff. HTMLElement
  -> Markup (Eff (dom :: DOM | eff) Unit) -> Eff (dom :: DOM | eff) Unit
renderTo e m = runEffFn1 _withHooks (runEffFn2 _patch e (renderMarkup m))

-- | Render `Markup` to the body of the DOM using virtual-dom.
renderToBody
  :: forall eff. Markup (Eff (dom :: DOM | eff) Unit)
  -> Eff (dom :: DOM | eff) Unit
renderToBody m = window >>= document >>= body >>= toMaybe >>> traverse_ (flip renderTo m)

-- | Converts `Markup` to an incremental DOM render function.
renderMarkup :: forall eff. Markup (Eff eff Unit) -> Render
renderMarkup m = runChained (markup tagf textf m) where
  tagf tn _ ps cs = Chained do
    runEffFn2 _elementOpenStart tn (toNullable (findKey ps))
    traverse_ renderProp ps
    _elementOpenEnd
    runChained cs
    runEffFn1 _elementClose tn
  textf txt = Chained (runEffFn1 _text txt)

-- | Converts `Prop`s to the corresponding incremental DOM render function.
renderProp :: forall eff. Prop (Eff eff Unit) -> Render
renderProp (Attr name val) = runEffFn2 _attr name val
renderProp (Handler name go) = runEffFn2 _handler ("on" ++ name) (mkEffFn1 go)
renderProp _ = pure unit

-- | Finds the first key in a collection of properties, if any.
findKey :: forall e. Array (Prop e) -> Maybe Key
findKey = runFirst <<< foldMap \p -> First $ case p of
  Key k -> Just k
  _ -> Nothing

-- | A function passed to incremental dom that imperatively describes the
-- | construction of a DOM tree.
type Render = Eff (incDOM :: INCDOM) Unit

-- | Effect for the incremental DOM render function.
foreign import data INCDOM :: !

foreign import _elementOpenStart :: EffFn2 (incDOM :: INCDOM) Tag (Nullable Key) Unit
foreign import _elementOpenEnd :: Eff (incDOM :: INCDOM) Unit
foreign import _elementClose :: EffFn1 (incDOM :: INCDOM) Tag Unit
foreign import _attr :: EffFn2 (incDOM :: INCDOM) Attr String Unit
foreign import _handler :: forall eff. EffFn2 (incDOM :: INCDOM) String (EffFn1 eff Foreign Unit) Unit
foreign import _text :: EffFn1 (incDOM :: INCDOM) String Unit
foreign import _patch :: forall eff. EffFn2 (dom :: DOM | eff) HTMLElement Render Unit
foreign import _withHooks :: forall eff. EffFn1 (dom :: DOM | eff) (Eff (dom :: DOM | eff) Unit) Unit

-- | Monoid that chains actions in an applicative.
newtype Chained f = Chained (f Unit)

runChained :: forall f. Chained f -> f Unit
runChained (Chained a) = a

instance chainedSemigroup :: (Apply f) => Semigroup (Chained f) where
  append (Chained a) (Chained b) = Chained (a *> b)

instance chainedMonoid :: (Applicative f) => Monoid (Chained f) where
  mempty = Chained (pure unit)

