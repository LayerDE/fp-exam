{-# LANGUAGE RankNTypes #-}
module M0000000 where

import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import qualified Data.Set as Set
import Control.Monad.State.Lazy as State

import Game
import Cards
import Gameplay


shootTheMoonStrategy :: PlayerStrategy
shootTheMoonStrategy =
  PlayerStrategy $ do
  playerState <- State.get
  let trick = playerTrick playerState
      hand = playerHand playerState
      firstCard = leadingCardOfTrick trick
      firstSuit = suit firstCard
      followingCardsOnHand = Set.filter ((== firstSuit) . suit) hand
  if trickEmpty trick 
    then
      return (Set.findMin hand)
    else
      case Set.lookupMin followingCardsOnHand of
        Nothing ->
          return (Set.findMax hand) -- any card is fine, so try to get rid of high hearts
        Just card ->
          return card           -- otherwise use the minimal following card
