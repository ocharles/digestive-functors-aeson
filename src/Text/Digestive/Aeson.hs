{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Run digestive-functors forms against JSON.
module Text.Digestive.Aeson
    ( digestJSON
    , jsonErrors
    ) where

import           Control.Lens hiding (Index, Path)
import           Data.Aeson (ToJSON(toJSON), Value(..), object)
import           Data.Aeson.Lens
import           Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import           Text.Digestive

import qualified Data.Text as T

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
>     digestJSON "" myForm parsedJson
-}
digestJSON :: Monad m
           => Form v m a
           -- ^ The form to evaluate.
           -> Value
           -- ^ The JSON document to use for validation. If you need to use
           -- only part of this document, you need to transform this value
           -- first. You may find the @aeson-lens@ package useful for this.
           -> m (View v, Maybe a)
digestJSON f json = postForm "" f (jsonEnv json)
  where jsonEnv :: Monad m => Value -> Env m
        jsonEnv v (MetaPath p) =
            return . maybe [] size $ Just v ^. pathToLens p
          where
            size (Array a) = [Container . V.length $ a]
            size _         = []

        jsonEnv v (ActualPath p) =
           return . maybe [] jsonToText $
            Just v ^. pathToLens p

        jsonToText (String s) = [TextInput s]
        jsonToText (Bool b)   = showPack b
        jsonToText (Number n) = showPack n
        jsonToText Null       = []
        jsonToText (Object _) = []
        jsonToText (Array _)  = []

        showPack = return . TextInput . T.pack . show


--------------------------------------------------------------------------------
{-| Takes a 'View' and displays any errors in a hierachical format that matches
the expected input.

Example:

>     > jsonErrors myForm
>     {"person":{"name":"This field is required"}}
-}
jsonErrors :: ToJSON a => View a -> Value
jsonErrors = fromMaybe (error "Constructing error tree failed!") .
    foldl encodeError (Just $ object []) . viewErrors
  where
    encodeError json (path, message) =
      json & pathToLens (pathComponents path) .~ Just (toJSON message)


--------------------------------------------------------------------------------
-- -    encodeError json (path, message) = json & pathToLens path .~ Just (toJSON message)
-- -    pathToLens = foldl (.) id . map pathElem
-- -    pathElem p = maybe (key p) nth (readMay $ T.unpack p)
-- +              writeLBS (encode $ jsonErrors view)
pathToLens :: Functor f
           => [PathElement]
           -> (Maybe Value -> f (Maybe Value))
           -> Maybe Value
           -> f (Maybe Value)
pathToLens = foldl (.) id . map pathElem
  where
    pathElem (Path "") = id
    pathElem (Path p) = key p
    pathElem (Index i) = nth i
