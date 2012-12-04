{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Control.Monad.Identity (runIdentity)
import           Data.Aeson (decode)
import           Data.Text
import           Test.Framework (Test, defaultMain, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit ((@?=))
import           Text.Digestive (Form, (.:), check, listOf, text)
import           Text.Digestive.Aeson (digestJSON, jsonErrors)

import qualified Data.Text as T

--------------------------------------------------------------------------------
tests :: [Test]
tests = [ testPokemon
        , testPokedex
        ]


--------------------------------------------------------------------------------
data Pokemon = Pokemon { pokemonName :: Text }
  deriving (Eq, Show)

pokeForm :: Monad m => Form Text m Pokemon
pokeForm = Pokemon <$> "name" .: nonEmptyText
  where
    nonEmptyText = check "Name cannot be empty" (not . T.null) $
                     text Nothing

testPokemon :: Test
testPokemon = testGroup "Pokemon tests" [ testPokemonOk, testPokemonInvalid ]
  where
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


--------------------------------------------------------------------------------
data Pokedex = Pokedex [Pokemon]
  deriving (Eq, Show)


testPokedex :: Test
testPokedex = testGroup "Pokedex tests"
                [ testPokedexSingle
                , testPokedexMany
                , testPokedexFailHead
                , testPokedexFailLater
                ]
  where
    pokedexForm :: Monad m => Form Text m Pokedex
    pokedexForm = Pokedex <$> "pokemon" .: listOf pokeForm

    testPokedexSingle = testCase "Valid pokedex with one pokemon" $
        Just expected @?= (runIdentity $ snd <$> digestJSON pokedexForm json)
      where
        (Just json) = decode "{\"pokemon\":[{\"name\": \"Pikachu\"}]}"
        expected = Pokedex [Pokemon { pokemonName = "Pikachu" }]

    testPokedexMany = testCase "Valid pokedex with many pokemon" $
        Just expected @?= (runIdentity $ snd <$> digestJSON pokedexForm json)
      where
        (Just json) = decode "{\"pokemon\":[{\"name\": \"Pikachu\"}, {\"name\":\"Clefable\"}, {\"name\":\"Gengar\"}]}"
        expected = Pokedex [ Pokemon { pokemonName = "Pikachu" }
                           , Pokemon { pokemonName = "Clefable" }
                           , Pokemon { pokemonName = "Gengar" }
                           ]

    testPokedexFailHead =
        let (v, r) = runIdentity $ digestJSON pokedexForm json
        in testGroup "Submit pokedex with a single invalid item"
             [ testCase "Failed validation" $ r @?= Nothing
             , testCase "jsonErrors shows correct errors" $ jsonErrors v @?= errors
             ]
      where
        (Just json) = decode "{\"pokemon\":[{\"name\":\"\"}]}"
        (Just errors) = decode "{\"pokemon\":[{\"name\":\"Name cannot be empty\"}]}"

    testPokedexFailLater =
        let (v, r) = runIdentity $ digestJSON pokedexForm json
        in testGroup "Submit pokedex with a later invalid item"
             [ testCase "Failed validation" $ r @?= Nothing
             , testCase "jsonErrors shows correct errors" $ jsonErrors v @?= errors
             ]
      where
        (Just json) = decode "{\"pokemon\":[{\"name\": \"Pikachu\"}, {\"name\":\"\"}]}"
        (Just errors) = decode "{\"pokemon\":[null, {\"name\":\"Name cannot be empty\"}]}"


--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain tests
