module Test.Main where

import Prelude

import DOM (DOM)
import Control.Monad.Eff (Eff)

import Signal.Channel (CHANNEL)

import Text.Smolder.HTML as H
import Text.Smolder.Markup as H
import Text.Smolder.HTML.Attributes as A

import Flare (UI, number)
import Flare.Smolder (runFlareHTML)

import Palto

expr2 :: forall repr. Mult repr
      => Expr repr 
      => Number -> Number -> Number -> repr Number
expr2 x y z = (pint x `pmul` (pint y `padd` pint z))

h :: forall a. Number -> Number -> Number -> H.MarkupM a Unit
h x y z = (H.div `H.with` (A.style $ "background-color: #f3f") $ H.text $ runStringify $ expr2 x y z) 
       <> (H.div `H.with` (A.style $ "background-color: #abc") $ H.text " = ")
       <> (H.div `H.with` (A.style $ "background-color: #ccc") $ H.text $ show (runEval $ expr2 x y z))

ui0 :: forall a e. UI e (H.MarkupM a Unit)
ui0 = h <$> number "x" 5.0 
        <*> number "y" 3.0
        <*> number "z" 2.0

-- Render everything to the DOM
main :: Eff ( channel :: CHANNEL, dom :: DOM) Unit
main = do
  runFlareHTML     "controls0"   "output0"  ui0
