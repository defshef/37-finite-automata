module Dfa where

import           Data.List (find)

type StateId = Integer
type AcceptStates = [StateId]
type Rule = (StateId, Char, StateId)
type Rulebook = [Rule]
data Dfa = Dfa {state :: StateId,
         acceptStates :: AcceptStates,
             rulebook :: Rulebook}
             deriving (Show)


matchesRule :: StateId -> Rule -> Char -> Bool
matchesRule currentState (from, onChar, to) c = from == currentState && onChar == c

maybeRule :: StateId -> Rulebook -> Char -> Maybe Rule
maybeRule currentState rules c = find (\rule -> matchesRule currentState rule c) rules

acceptChar :: Dfa -> Char -> Maybe Dfa
acceptChar dfa@Dfa {state = currentState, acceptStates = acceptStates, rulebook = rules} c =
  case maybeRule currentState rules c of
    Just (_, _, to) -> Just dfa {state = to}
    Nothing         -> Nothing

acceptString :: Dfa -> String -> Bool
acceptString Dfa {state = currentState, acceptStates = acceptStates} [] = currentState `elem` acceptStates
acceptString dfa (c:cs) =
  case acceptChar dfa c of
    Just newDfa -> acceptString newDfa cs
    Nothing     -> False
