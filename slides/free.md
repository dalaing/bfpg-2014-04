% Free Monads are good for you
% Dave Laing

# What

> - Free Monads
> - An abstract syntax tree
 
# Why

> - Abstraction is good
> - Syntax and trees are fun as well

# Where and when

> - EVERYWHERE AND EVERYWHEN

# Coming up, in Free Monads are good for you

# We want some commands

```haskell

data DataStore i v = Create (v -> i)
                   | List (() -> [v])
                   | Retrieve (i -> v)
                   | Update v
                   | Delete i
```

# We need to be able to make them a functor

```haskell

data DataStoreF i v k = Create v (i -> k)
                      | List ([v] -> k)
                      | Retrieve i (v -> k)
                      | Update v k
                      | Delete i k
```

# A functor is born

```haskell

instance Functor (DataStoreF i v) where
  fmap f (Create v k)   = Create v (f . k)
  fmap f (List k)       = List (f . k)
  fmap f (Retrieve i k) = Retrieve i (f . k)
  fmap f (Update v k)   = Update v (f k)
  fmap f (Delete i k)   = Delete i (f k)
```

# A sprinkle of magic

```haskell
import Control.Monad.Free

type DataStore i v r = Free (DataStoreF i v) r
```

# Some helpers

```haskell
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
```
# Data for the store

```haskell
data Link = Link {
             linkId :: Maybe Int
           , link :: T.Text 
           , rating :: Int 
           } deriving (Eq, Show)
```

# Monadic helpers

```haskell
setup :: DataStore Int Link ()
setup = do
  _ <- create (Link Nothing "Why dairy cows matter" 4)
  _ <- create (Link Nothing "Purify grass using dairy cows" 5)
  _ <- create (Link Nothing "Dairy cow transformers " 3)
  return ()

contains :: T.Text -> DataStore Int Link [Link]
contains t = do
  es <- list
  return $ filter (T.isInfixOf t . link) es

substitute :: T.Text -> T.Text -> DataStore Int Link ()
substitute from to = do
  es <- contains from
  forM_ es $ \(Link i l r) -> 
    update (Link i (T.replace from to l) r)
```

# Different ways to interpret the commands

In memory
```haskell
runDB :: Int 
      -> M.Map Int Link 
      -> DataStore Int Link r 
      -> (r, M.Map Int Link)
```

In a database
```haskell
runDB :: Connection 
      -> DataStore Int Link r 
      -> IO r
```

# Preliminaries - Recursive data and recursive types

# Recursive data types and functors

```haskell
data List a = Nil
            | a : List a

instance Functor List where
    fmap f Nil      = Nil
    fmap f (x : xs) = f x : fmap f xs
```

# Abstracting the recursive part

```haskell
data ListF f a = Nil
               | a : f a

instance Functor f => Functor (ListF f) where
    fmap f Nil      = Nil
    fmap f (x : xs) = f x : fmap f xs
```
# Fixing recursive data types

```haskell
type Fix f = f (Fix f)
type List = Fix ListF
type List = ListF (Fix ListF)
type List = ListF (ListF (Fix ListF))
type List = ListF (ListF (ListF (Fix ListF)))
```

# Fixing lists

```haskell
type Fix f = f (Fix f)
type List = Fix ListF
type List = ListF (Fix ListF)

-- expand the constructors
type List a = Nil
            | a : Fix ListF a

-- because List = Fix ListF
type List a = Nil
            | a : List a
```

# Preliminaries - Functors and monads

Say we want to get from `Parser Char` to `Parser Int`

If we had a function 
```haskell
convert :: Char -> Int
```
then we're after
```haskell
convertParser :: Parser Char -> (Char -> Int) -> Parser Int
````
# What if it can fail?

Now lets say we have
```haskell
convert :: Char -> Maybe Int
```
we don't necessarily want
```haskell
Parser Char -> (Char -> Maybe Int) -> Parser (Maybe Int)
```
because a parser should handle that for us

# Dealing with failure

Add a literal parser and a failure parser
```haskell
literalParser   :: a -> Parser a
failParser      :: Parser a
```
then we can do
```haskell
convertWithFailure :: Char -> Parser Int
convertWithFailure c = case convert c of
    Nothing -> failParser
    Just x -> literalParser x
```
which can be stitched into our workflow with
```haskell
convertParser :: Parser Char -> (Char -> Parser Int) -> Parser Int
```

# In the general case

Functor f
```haskell
fmap :: (a -> b) -> f a -> f b
```

Monad m
```haskell
return :: a -> m a
(>>=) :: m a -> (a -> m b) -> m b
```

# An important point

> - compose all the pure functions you like between `>>=`s
> - they're just one function
> - if `>>=` is choosing paths, we have a tree of choices
> - `return` is at the leaves of the tree 
> - alternatively, `>>=` is replacing leaves with new subtrees

# To the freemonadmobile

# Commands for an interpreter

```haskell
data DataStore i v  = Create (v -> i)
                      | List (() -> [v])
                      | Retrieve (i -> v)
                      | Update v
                      | Delete i
```

# What happens next?

> - Every monad is a functor, so we need a functor instance
> - Can't really work over `i` or `v` for the functor

# Continuations for the data type

```haskell
data DataStoreF i v k = Create v (i -> k)
                      | List ([v] -> k)
                      | Retrieve i (v -> k)
                      | Update v k
                      | Delete i k
```

# Coliminaries - Continuations

> - Represents what do we do next
> - Makes sense when we're dealing with sequencing
> - Give away is a type ending with `(a -> r) -> r`
> - Especially if we're constructing an `a`

# The functor instance

```haskell
instance Functor (DataStoreF i v) where
  fmap f (Create v k)   = Create v (f . k)
  fmap f (List k)       = List (f . k)
  fmap f (Retrieve i k) = Retrieve i (f . k)
  fmap f (Update v k)   = Update v (f k)
  fmap f (Delete i k)   = Delete i (f k)
```

# More recursive fun

```haskell
-- from Control.Monad.Free
data Free f r = Free (f (Free f r)) | Pure r

type DataStore i v = Free (DataStoreF i v)
```

# Turn any functor into a monad

```haskell
instance (Functor f) => Monad (Free f) where
    return = Pure
    (Free x) >>= f = Free (fmap (>>= f) x)
    (Pure r) >>= f = f r
```

# Some help with lifting

```haskell
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
```

# Monad machinery and free monads

```haskell
setup :: DataStore Int Link ()
setup = do
  _ <- create (Link Nothing "Why dairy cows matter" 4)
  _ <- create (Link Nothing "Purify grass using dairy cows" 5)
  _ <- create (Link Nothing "Dairy cow transformers " 3)
  return ()

contains :: T.Text -> DataStore Int Link [Link]
contains t = do
  es <- list
  return $ filter (T.isInfixOf t . link) es

substitute :: T.Text -> T.Text -> DataStore Int Link ()
substitute from to = do
  es <- contains from
  forM_ es $ \(Link i l r) -> 
    update (Link i (T.replace from to l) r)
```

# Using free monads

```haskell
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
  return $ filter ((>= n) ∘ rating) es
```

# Running the free monad with a Map

```haskell
sample :: DataStore Int Link [Link]
sample = do
    setup
    fixup
    ratingLimit 4
```

```haskell
fst $ runDB 0 M.empty sample

> [ Link (Just 1) "Why free monads matter" 4,
    Link (Just 2) "Purify code using free monads" 5
  ]
```

# Running the free monad with an SQLite database

```haskell
sample :: DataStore Int Link [Link]
sample = do
    setup
    fixup
    ratingLimit 4
```

```haskell
main = do
    c <- open "test.db"
    runDB c sample
    close c

> [ Link (Just 1) "Why free monads matter" 4,
    Link (Just 2) "Purify code using free monads" 5
  ]
```

# Benefits of free monads

- Separates the definition and construction of an AST from what it means to run it
- Can be used to modularize code along abstraction boundaries
- Also means you have the existing monad machinery at your disposal

# Concrete uses for free monads

- Great for testing 
  - Separate the use of a datastore from the implementation
  - All monads can be express as free monads
    - Becomes relevant when using QuickCheck on monadic code 
- Can also use to setup type-enforced boundaries
  - Like an AST for Console IO, so that you can do your 
    usual operations, but can't work with the network, 
    create directories, etc...

# Coliminaries - Monad transformers

> - Normally your monad is a tree of computations with values as leaves
> - With monad transformers, the leaves of your monad `t` tree are a monad `m` tree
> - `lift` lets you work on the lower tree in the main tree

# This is what it looks like

The typeclass
```haskell
class MonadTrans t where
  lift :: Monad m => m a -> t m a
```

Some examples
```haskell
newtype StateT s m a = 
    StateT {
        runStateT :: s -> m (a, s)
    }
-- runStateT :: StateT s m a -> s -> m (a, s)

newtype MaybeT m a = 
    MaybeT {
        runMaybeT :: m (Maybe a)
    }
-- runMaybeT :: MaybeT m a -> m (Maybe a)
```

# Maybe over State

```haskell
type MOS m r = MaybeT (StateT s m) r
runMOS :: MOS m r -> s -> m (Maybe a, s) 
runMOS = runStateT . runMaybeT
```

> - A combination of state and failure
> - The state transformer runs even if the value fails to come through
> - Because we had a state transformer and we added the option of failure with MaybeT

# State over Maybe

```haskell
type SOM m r = StateT s (MaybeT m) r
runSOM :: SOM m r -> s -> m (Maybe (a, s))
runSOM = runMaybeT . runStateT
```

> - A combination of state and failure
> - The state transformer stops if a failure occurs 
> - Because we had a computation which could fail and we added a state transformer to it 

# From Free to FreeT

- Lets us manage configuration, state, error, etc with standard Haskell idioms

Before
```haskell
data Free f r = Free (f (Free f r)) | Pure r
```

After
```haskell
data FreeF f r a = 
    Free (f a) | Pure r

newtype FreeT f m r 
    = FreeT {
      runFreeT :: m (FreeF f r (FreeT f m r))
    }
```

# Before transformers

```haskell
runDB :: Int 
      -> M.Map Int Link 
      -> DataStore Int Link r 
      -> (r, M.Map Int Link)

runDB _ m (Pure x) = (x, m)

runDB d m (Free (Create (Link Nothing l r) k)) =
 let
   i = (+ 1) . lastDef d . M.keys $ m
   link = Link (Just i) l r
 in
  runDB d (M.insert i link m) (k i)

runDB d m (Free (List k)) =
  runDB d m (k $ M.elems m)
```

# Some boilerplate to get going

```haskell
type DS m r = DataStore Int Link 
                        (ErrorT LinkDBError
                        (ReaderT Int 
                        (StateT (M.Map Int Link) m))) 
                        r

runDB :: Monad m 
      => Int 
      -> M.Map Int Link 
      -> DS m r 
      -> m (Either LinkDBError r)
runDB d m = 
            flip evalStateT m .
            flip runReaderT d .
            runErrorT . 
            evalDB  

```

# More than meets the eye

```haskell
evalDB :: (Monad m, MonadState (M.Map Int Link) m, 
          MonadReader Int m, MonadError LinkDBError m)
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
```

# Publiminaries

- Cofree comonads
- Indexed free
- Yoneda and Codensity

# Conclusion

- Add free monads to your toolbox
- Theoretical stuff is useful and combines in intersting ways

