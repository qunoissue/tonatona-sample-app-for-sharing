module TonaApp.Bson.Extra
  ( ParseError
  , ParseResult
  , lookupOptional
  ) where

import Tonalude hiding (lookup)

import Control.Monad.Fail (MonadFail(..))
import qualified Data.Bson as Bson
import qualified Data.Text as Text



-- Error


-- |
newtype ParseError
  = ParseError Text
  deriving (Eq, Show)

instance MonadFail (Either ParseError) where
  fail = Left . ParseError . Text.pack


-- |
type ParseResult a = Either ParseError a


-- | optional fieldを取得する。
-- fieldが存在しない場合は `Nothing` を返す。
lookupOptional :: (Bson.Val v, MonadFail m) => Bson.Label -> Bson.Document -> m (Maybe v)
lookupOptional label doc =
  let
    lookup' :: forall v' . Bson.Val v' => Bson.Label -> Bson.Document -> Maybe v'
    lookup' = Bson.lookup
  in
    pure $ lookup' label doc
