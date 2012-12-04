{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Control.Monad.Identity (runIdentity)
import           Data.Aeson (decode)
import           Data.Text
import           Test.Framework (Test, defaultMain, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit ((@?=))
import           Text.Digestive (Form, (.:), check, text)
import           Text.Digestive.Aeson (digestJSON, jsonErrors)

import qualified Data.Text as T

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testPokemon
        ]


--------------------------------------------------------------------------------
data Pokemon = Pokemon { pokemonName :: Text }
  deriving (Eq, Show)


testPokemon :: Test
testPokemon = testGroup "Pokemon tests" [ testPokemonOk, testPokemonInvalid ]
  where
    pokeForm :: Monad m => Form Text m Pokemon
    pokeForm = Pokemon <$> "name" .: nonEmptyText
      where
        nonEmptyText = check "Name cannot be empty" (not . T.null) $
                         text Nothing

    testPokemonOk = testCase "Submit pokeForm with valid data" $
        Just expected @?= (runIdentity $ snd <$> digestJSON pokeForm json)
      where
        (Just json) = decode "{\"name\":\"Pikachu\"}"
        expected = Pokemon { pokemonName = "Pikachu" }

    testPokemonInvalid =
        let (v, r) = runIdentity $ digestJSON pokeForm json
        in testGroup "Submit pokeForm with invalid data/check error view"
             [ testCase "Failed validation" $ r @?= Nothing
             , testCase "jsonErrors shows correct errors" $ jsonErrors v @?= errors
             ]
      where
        (Just json) = decode "{\"name\":\"\"}"
        (Just errors) = decode "{\"name\":\"Name cannot be empty\"}"
        expected = Pokemon { pokemonName = "Pikachu" }


--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain tests
