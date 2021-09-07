module Main where

import qualified M4337015
import qualified Gameplay

main :: IO ()
player4337015 = Gameplay.makePlayer "Big V (4337015)" M4337015.shootTheMoonStrategy
main = Gameplay.startCustom1 player4337015

