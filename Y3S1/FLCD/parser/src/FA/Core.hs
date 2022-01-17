module FA.Core(State, FA(..), check, checkPrefix) where

import Data.List.Extra(maximumOn)
import Data.Map.Strict(Map)
import Data.Map.Strict qualified as M
import Data.Set(Set)
import Data.Set qualified as S
import Data.Maybe (catMaybes)

type State = String

data FA =
  FA
  { faStates :: Set State
  , faInitial :: State
  , faFinal :: Set State
  , faAlphabet :: Set Char
  , faTransitions :: Map (State, Char) [State]
  }
  deriving stock Show

-- Returns the longest prefix of the input that is accepted by the FA.
checkPrefix :: FA -> String -> Maybe (String, String)
checkPrefix FA{..} = go [] faInitial
  where
    go :: String -> State -> String -> Maybe (String, String)
    go acc s []
      | S.member s faFinal = Just (reverse acc, [])
      | otherwise = Nothing

    go acc s i@(c : cs)
      | Just ss <- faTransitions M.!? (s, c) =
          case catMaybes $ (go (c : acc) `flip` cs) <$> ss of
            [] -> Nothing
            rs -> Just $ maximumOn (length . fst) rs
      | S.member s faFinal = Just (reverse acc, i)
      | otherwise = Nothing

-- Checks whether the *entire* sequence is accepted by the FA.
check :: FA -> String -> Bool
check fa input =
  case checkPrefix fa input of
    Just (_, "") -> True
    _ -> False
