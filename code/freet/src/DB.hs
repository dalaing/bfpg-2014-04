module DB (
    DataStore
  , DataStoreF(..)
  , create
  , list
  , retrieve
  , update
  , delete
  ) where

import           Control.Monad.Trans.Free

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

type DataStore i v m r = FreeT (DataStoreF i v) m r

create :: Monad m
       => v
       -> DataStore i v m i
create v = liftF $ Create v id

list :: Monad m
     => DataStore i v m [v]
list = liftF $ List id

retrieve :: Monad m
         => i
         -> DataStore i v m v
retrieve i = liftF $ Retrieve i id

update :: Monad m
       => v
       -> DataStore i v m ()
update v = liftF $ Update v ()

delete :: Monad m
       => i
       -> DataStore i v m ()
delete i = liftF $ Delete i ()
