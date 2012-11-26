{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Digestive.Aeson
    ( digestJSON ) where

import Control.Applicative
import Data.Aeson (Value(..))
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Text.Digestive

digestJSON :: Monad m => Text -> Form v m a -> Value -> m (View v, Maybe a)
digestJSON t f json = postForm t f (jsonEnv json)
  where jsonEnv :: Monad m => Value -> Env m
        jsonEnv (Object o) [p] = return $ fromMaybe [] (jsonToText <$> Map.lookup p o)
        jsonEnv (Object o) ps = return $ maybe [] concat (jsonEnv <$> Map.lookup p' o <*> pure ps')
          where (p':ps') = filter (not . T.null) ps

        jsonEnv o [] = return $ jsonToText o
        jsonEnv _ _ = return []

        jsonToText (String s) = [TextInput s]
        jsonToText (Bool b) = [TextInput . T.pack $ show b]
        jsonToText Null = []
        jsonToText (Number n) = [TextInput . T.pack $ show n]
        jsonToText (Object _) = []
        jsonToText (Array a) = concatMap jsonToText $ V.toList a
