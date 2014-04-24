module DB (
    DataStore
  , DataStoreF(..)
  , create
  , list
  , retrieve
  , update
  , delete
  ) where

import           Control.Monad.Free

data DataStoreF i v f = Create v (i -> f)
              | List ([v] -> f)
              | Retrieve i (v -> f)
              | Update v f
              | Delete i f

instance Functor (DataStoreF i v) where
  fmap f (Create v k) = Create v (f . k)
  fmap f (List k) = List (f . k)
  fmap f (Retrieve i k) = Retrieve i (f . k)
  fmap f (Update v k) = Update v (f k)
  fmap f (Delete i k) = Delete i (f k)

type DataStore i v r = Free (DataStoreF i v) r

create :: v -> DataStore i v i
create v = liftF $ Create v id

list :: DataStore i v [v]
list = liftF $ List id

retrieve :: i -> DataStore i v v
retrieve i = liftF $ Retrieve i id

update :: v -> DataStore i v ()
update v = liftF $ Update v ()

delete :: i -> DataStore i v ()
delete i = liftF $ Delete i ()
