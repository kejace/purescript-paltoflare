module Test.Main where

import Prelude

import Data.Array
import Data.Maybe
import Data.Monoid
import Data.Foldable
import Data.Int
import Data.Traversable
import Math (pow, sin, cos, pi, abs)

import Control.Monad.Eff
import DOM

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Signal.DOM
import Signal.Time
import Signal.Channel

import Text.Smolder.HTML as H
import Text.Smolder.Markup as H
import Text.Smolder.HTML.Attributes as A

import Flare
import Flare.Drawing
import Flare.Smolder

import Palto

expr2 :: forall repr. Mult repr
      => Expr repr 
      => Number -> Number -> Number -> repr Number
expr2 x y z = (pint x `pmul` (pint y `padd` pint z))

doc :: forall a e. H.Markup (a -> Eff (console :: CONSOLE | e) Unit)
doc x y z = H.html $ do
              H.head $ do
                H.title $ H.text "OMG HAI LOL"

h :: Number -> Number -> Number -> H.MarkupM Unit
h x y z = (H.div `H.with` (A.style $ "background-color: #f3f") $ H.text $ runStringify $ expr2 x y z) 
       <> (H.div `H.with` (A.style $ "background-color: #abc") $ H.text " = ")
       <> (H.div `H.with` (A.style $ "background-color: #ccc") $ H.text $ show (runEval $ expr2 x y z))

ui0 :: forall a. (UI a) H.Markup
ui0 = h <$> number "x" 5.0 
        <*> number "y" 3.0
        <*> number "z" 2.0

-- Render everything to the DOM
main :: forall a. Eff ( channel :: CHANNEL, dom :: DOM | a) Unit
main = do
  runFlareHTML     "controls0"   "output0"  ui0
