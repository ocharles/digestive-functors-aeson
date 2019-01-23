{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | Run digestive-functors forms against JSON.
module Text.Digestive.Aeson
    ( digestJSON
    , jsonErrors
    ) where

import Control.Lens
import Control.Monad (join)
import Data.Aeson (ToJSON(toJSON), Value(..), object)
import Data.Aeson.Lens
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import Safe (readMay)
import Text.Digestive
import Text.Digestive.Form.List (unparseIndices)

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Text as T
import qualified Data.Vector as V

--------------------------------------------------------------------------------
{-| Given a JSON document and a form, attempt to use the JSON document to
evaluation the form. If the form fails validation, then 'Nothing' is
returned.

Example:

>     import Data.Aeson (json)
>     import Data.Attoparsec.Lazy (parse, maybeResult)
>     import Text.Digestive.Aeson (digestJSON)
>     ...
>     Just parsedJson <- maybeResult . parse json <$> fetchJsonText
>     digestJSON myForm parsedJson
-}
digestJSON :: Monad m
           => Form v m a
           -- ^ The form to evaluate.
           -> Value
           -- ^ The JSON document to use for validation. If you need to use
           -- only part of this document, you need to transform this value
           -- first. You may find the @aeson-lens@ package useful for this.
           -> m (View v, Maybe a)
digestJSON f json = postForm "" f (const (return (jsonEnv json)))
  where jsonEnv :: Monad m => Value -> Env m
        jsonEnv v p
          | last p == "indices" = case join (Just v ^? pathToLens (init p)) of
              Just (Array a) -> return $ return . TextInput $
                unparseIndices [0 .. (pred $ V.length a)]
              _ -> return [ TextInput "" ]
          | otherwise = return . maybe [] jsonToText $ join (Just v ^? pathToLens p)

        jsonToText (String s)   = [TextInput s]
        jsonToText (Bool True)  = [TextInput "on"]
        jsonToText (Bool False) = [TextInput "off"]
        jsonToText (Number n)   = showPack n
        jsonToText Null         = []
        jsonToText (Object _)   = []
        jsonToText (Array _)    = []

        showPack = return . TextInput . T.pack . show


--------------------------------------------------------------------------------
{-| Takes a 'View' and displays any errors in a hierachical format that matches
the expected input.

Example:

>     > jsonErrors myForm
>     {"person":{"name":"This field is required"}}
-}
jsonErrors :: ToJSON a => View a -> Value
jsonErrors v =
  fromMaybe (object [])
            (foldl encodeError Nothing (viewErrors v))
  where
    encodeError json (path, message) =
      json & pathToLens path . non Null .~ toJSON message


--------------------------------------------------------------------------------
pathToLens :: [T.Text] -> Traversal' (Maybe Value) (Maybe Value)
pathToLens = foldl (.) id . map pathElem . filter (not . T.null)
  where
    pathElem p = maybe (non (object []) . _Object . at p)
                       (\n -> non (Array mempty) . _Array . iso toMap fromMap . at n)
                       (readMay $ T.unpack p)
    toMap = V.ifoldl' (\m i a -> IntMap.insert i a m) IntMap.empty
    fromMap m = V.fromList [ IntMap.findWithDefault Null x m
                           | x <- [0 .. fst (IntMap.findMax m)]
                           ]
