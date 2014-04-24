{-# LANGUAGE FlexibleContexts #-}
module LinkDB.Memory (
    runDB
  ) where

import           Control.Monad.Trans.Free
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Error
import qualified Data.Map           as M
import           Safe               (lastDef)

import           DB
import           Link
import           LinkDBError

type DB m r = DataStore Int Link 
             (ErrorT LinkDBError
             (ReaderT Int 
             (StateT (M.Map Int Link) m))) 
             r

runDB :: Monad m 
      => Int 
      -> M.Map Int Link 
      -> DB m r 
      -> m (Either LinkDBError r, M.Map Int Link)
runDB d m = 
            flip runStateT m .
            flip runReaderT d .
            runErrorT . 
            evalDB 

evalDB :: (Monad m, 
          MonadState (M.Map Int Link) m, 
          MonadReader Int m, 
          MonadError LinkDBError m)
      => DataStore Int Link m r 
      -> m r
evalDB ds = runFreeT ds >>= evalDB'
  where

    evalDB' (Pure x) = 
      return x

    evalDB' (Free (Create (Link i l r) k)) = 
      case i of
        Nothing -> do
          d <- ask
          j <- gets ((+ 1) . lastDef d . M.keys)
          modify (M.insert j (Link (Just j) l r))
          evalDB (k j)
        Just _ -> throwError CreateWithIdSet

    evalDB' (Free (List k)) = do
      es <- gets M.elems
      evalDB (k es)

    evalDB' (Free (Retrieve i k)) = do
      x <- gets (M.lookup i)
      case x of
        Nothing -> throwError BadRetrieveIndex
        Just y -> evalDB (k y)

    evalDB' (Free (Update v k)) = 
      case linkId v of
        Nothing -> throwError UpdateWithNoId
        Just y -> do 
          modify (M.insert y v)
          evalDB k

    evalDB' (Free (Delete i k)) = do
      modify (M.delete i)
      evalDB k
