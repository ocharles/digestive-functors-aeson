{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Run digestive-functors forms against JSON.
module Text.Digestive.Aeson
    ( digestJSON
    , jsonErrors
    ) where

import           Control.Lens
import           Data.Aeson (ToJSON(toJSON), Value(..), object)
import           Data.Aeson.Lens
import           Data.Maybe (fromMaybe)
import           Safe
import           Text.Digestive
import           Text.Digestive.Form.List (unparseIndices)

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
        jsonEnv v p
          | head (reverse p) == "indices" = case Just v ^. pathToLens (init p) of
              Just (Array a) -> return $ return . TextInput $
                unparseIndices [0 .. (pred $ V.length a)]
              _ -> return [ TextInput "" ]
          | otherwise = return . maybe [] jsonToText $ Just v ^. pathToLens p

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
      json & pathToLens path .~ Just (toJSON message)


--------------------------------------------------------------------------------
pathToLens :: Functor f
           => [T.Text]
           -> (Maybe Value -> f (Maybe Value))
           -> Maybe Value
           -> f (Maybe Value)
pathToLens [p] = key p
pathToLens ps = let ps' = filter (not . T.null) ps
                in key (head ps') . (foldl (.) id . map pathElem $ tail ps')
  where
    pathElem p = maybe (key p) nth (readMay $ T.unpack p)
