{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
module LinkDB.Sqlite (
    open
  , close
  , runDB
  ) where

import           Control.Monad.Error
import           Control.Monad.Reader
import           Control.Monad.Trans.Free

import           Data.Maybe               (listToMaybe)
import           Database.SQLite.Simple   hiding (close, open)
import qualified Database.SQLite.Simple   (close, open)

import           DB
import           Link
import           LinkDBError

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

type DB m r = DataStore Int Link
             (ErrorT LinkDBError
             (ReaderT Connection m))
             r

runDB :: Connection
      -> DB IO r
      -> IO (Either LinkDBError r)
runDB c = flip runReaderT c .
          runErrorT .
          evalDB

evalDB :: (Monad m,
          MonadIO m,
          MonadReader Connection m,
          MonadError LinkDBError m)
      => DataStore Int Link m r
      -> m r
evalDB ds = runFreeT ds >>= evalDB'
  where

    evalDB' (Pure x) =
      return x

    evalDB' (Free (Create v k)) =
      case linkId v of
        Nothing -> do
          c <- ask
          liftIO $ execute c "INSERT INTO links VALUES (?, ?, ?)" v
          i <- liftIO $ lastInsertRowId c
          evalDB . k . fromIntegral $ i
        Just _ -> throwError CreateWithIdSet

    evalDB' (Free (List k)) = do
      c <- ask
      es <- liftIO $ query_ c "SELECT * FROM links"
      evalDB . k $ es

    evalDB' (Free (Retrieve i k)) = do
      c <- ask
      es <- liftIO $ query c "SELECT * FROM links WHERE id = ?" (Only i)
      case listToMaybe es of
        Nothing -> throwError BadRetrieveIndex
        Just x -> evalDB . k $ x

    evalDB' (Free (Update v k)) =
      case linkId v of
        Nothing -> throwError UpdateWithNoId
        Just y -> do
          c <- ask
          liftIO $ execute c "UPDATE links SET link = ?, rating = ? WHERE id = ?" (link v, rating v, y)
          evalDB k

    evalDB' (Free (Delete i k)) = do
      c <- ask
      liftIO $ execute c "DELETE FROM links WHERE id = ?" (Only i)
      evalDB k
