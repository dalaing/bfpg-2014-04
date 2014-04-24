module Link (
    Link(..)
  ) where

import           Control.Applicative
import qualified Data.Text              as T
import           Database.SQLite.Simple

data Link = Link {
             linkId :: Maybe Int
           , link   :: T.Text
           , rating :: Int
           } deriving (Eq, Show)

instance FromRow Link where
  fromRow = Link <$> field <*> field <*> field

instance ToRow Link where
  toRow (Link i l r) = [
      maybe SQLNull (SQLInteger . fromIntegral) i
    , SQLText l
    , SQLInteger (fromIntegral r)
    ]


