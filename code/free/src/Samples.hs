{-# LANGUAGE OverloadedStrings #-}
module Samples (
    setup
  , substitute
  , contains
  , fixup
  , ratingLimit
  ) where

import           Control.Monad (forM_)
import qualified Data.Text     as T

import           DB
import           Link

setup :: DataStore Int Link ()
setup = do
  _ <- create (Link Nothing "Why dairy cows matter" 4)
  _ <- create (Link Nothing "Purify grass using dairy cows" 5)
  _ <- create (Link Nothing "Dairy cow transformers " 3)
  return ()

contains :: T.Text 
         -> DataStore Int Link [Link]
contains t = do
  es <- list
  return $ filter (T.isInfixOf t . link) es

substitute :: T.Text 
           -> T.Text 
           -> DataStore Int Link ()
substitute from to = do
  es <- contains from
  forM_ es $ \(Link i l r) -> 
    update (Link i (T.replace from to l) r)

fixup :: DataStore Int Link ()
fixup = do
  substitute "cow" "monad" 
  substitute "dairy" "free"  
  substitute "Dairy" "Free" 
  substitute "grass" "code" 

ratingLimit :: Int 
            -> DataStore Int Link [Link] 
ratingLimit n = do
  es <- list
  return $ filter ((>= n) . rating) es

