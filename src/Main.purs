module Main where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Dogs as Dogs

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Dogs.page unit body
