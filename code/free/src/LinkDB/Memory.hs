module LinkDB.Memory (
    runDB
  ) where

import           Control.Monad.Free
import qualified Data.Map           as M
import           Data.Maybe         (fromJust)
import           Safe               (lastDef)

import           DB
import           Link

runDB :: Int
      -> M.Map Int Link
      -> DataStore Int Link r
      -> (r, M.Map Int Link)

runDB _ m (Pure x) = 
  (x, m)

runDB d m (Free (Create (Link Nothing l r) k)) =
 let
   i = (+ 1) . lastDef d . M.keys $ m
 in
  runDB d (M.insert i (Link (Just i) l r) m) (k i)

runDB d m (Free (Create (Link (Just i) _ _) k)) =
  runDB d m (k i)

runDB d m (Free (List k)) =
  runDB d m (k $ M.elems m)

runDB d m (Free (Retrieve i k)) =
  runDB d m (k $ m M.! i)

runDB d m (Free (Update v@(Link (Just _) _ _) k)) =
  runDB d (M.insert (fromJust . linkId $ v) v m) k

runDB d m (Free (Update (Link Nothing _ _) k)) =
  runDB d m k

runDB d m (Free (Delete i k)) =
  runDB d (M.delete i m) k

