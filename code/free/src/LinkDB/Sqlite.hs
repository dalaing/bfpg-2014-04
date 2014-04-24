{-# LANGUAGE OverloadedStrings #-}
module LinkDB.Sqlite (
    open
  , close
  , runDB
  ) where

import           Control.Monad.Free
import           Data.Maybe             (fromJust)
import           Database.SQLite.Simple hiding (close, open)
import qualified Database.SQLite.Simple (close, open)

import           DB
import           Link

open :: String 
     -> IO Connection
open s = do
  c <- Database.SQLite.Simple.open s
  execute_ c "CREATE TABLE IF NOT EXISTS links (\
             \   id INTEGER PRIMARY KEY \
             \ , link TEXT \
             \ , rating INTEGER \
             \ )"
  return c

close :: Connection 
      -> IO ()
close = Database.SQLite.Simple.close

runDB :: Connection
      -> DataStore Int Link r
      -> IO r

runDB _ (Pure x) =
  return x

runDB c (Free (Create v k)) = do
  execute c "INSERT INTO links VALUES (?, ?, ?)" v
  i <- lastInsertRowId c
  runDB c . k . fromIntegral $ i

runDB c (Free (List k)) = do
  es <- query_ c "SELECT * FROM links"
  runDB c . k $ es

runDB c (Free (Retrieve i k)) = do
  es <- query c "SELECT * FROM links WHERE id = ?" (Only i)
  -- The use of head here is horrifically bad
  -- Stay tuned for the correct way to do it
  runDB c . k . head $ es

runDB c (Free (Update v k)) = do
  -- The use of fromJust here is horrifically bad
  -- Stay tuned for the correct way to do it
  execute c "UPDATE links SET link = ?, rating = ? WHERE id = ?" (link v, rating v, (fromJust . linkId) v)
  runDB c k

runDB c (Free (Delete i k)) = do
  execute c "DELETE FROM links WHERE id = ?" (Only i)
  runDB c k
