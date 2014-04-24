{-# LANGUAGE OverloadedStrings #-}
module Samples (
    setup
  , contains
  , substitute
  , fixup
  , ratingLimit
  ) where

import           Control.Monad (forM_)
import qualified Data.Text     as T

import           DB
import           Link

setup :: Monad m 
      => DataStore Int Link m ()
setup = do
  _ <- create (Link Nothing "Why dairy cows matter" 4)
  _ <- create (Link Nothing "Purify grass using dairy cows" 5)
  _ <- create (Link Nothing "Dairy cow transformers " 3)
  return ()

contains :: Monad m 
         => T.Text 
         -> DataStore Int Link m [Link]
contains t = do
  es <- list
  return $ filter (T.isInfixOf t . link) es

substitute :: Monad m 
           => T.Text 
           -> T.Text 
           -> DataStore Int Link m ()
substitute from to = do
  es <- contains from
  forM_ es $ \(Link i l r) -> 
    update (Link i (T.replace from to l) r)

fixup :: Monad m
      => DataStore Int Link m ()
fixup = do
  substitute "cow" "monad" 
  substitute "dairy" "free"  
  substitute "Dairy" "Free" 
  substitute "grass" "code" 

ratingLimit :: Monad m 
            => Int 
            -> DataStore Int Link m [Link] 
ratingLimit n = do
  es <- list
  return $ filter ((>= n) . rating) es

