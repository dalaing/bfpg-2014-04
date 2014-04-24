module LinkDBError (
    LinkDBError(..)
  ) where

import           Control.Monad.Error

data LinkDBError = CreateWithIdSet
                 | BadRetrieveIndex
                 | UpdateWithNoId
                 | Other String
                 deriving (Eq, Show)

instance Error LinkDBError where
  strMsg = Other

