module Fsm where

import           Data.List  (find)
import           Data.Maybe (isJust)

type StateId = Integer
type AcceptStates = [StateId]
type Rule = (StateId, Char, StateId)
type Rulebook = [Rule]
type Dfa = (StateId, AcceptStates, Rulebook)

matchesRule :: StateId -> Rule -> Char -> Bool
matchesRule currentState (from, onChar, to) c = from == currentState && onChar == c

maybeRule :: StateId -> Rulebook -> Char -> Maybe Rule
maybeRule currentState rules c = find (\rule -> matchesRule currentState rule c) rules

acceptChar :: Dfa -> Char -> Maybe Dfa
acceptChar (currentState, acceptStates, rules) c = fmap (\(_, _, nextChar) -> (nextChar, acceptStates, rules)) (maybeRule currentState rules c)

acceptString :: Dfa -> String -> Bool
acceptString (currentState, acceptStates, _) [] = currentState `elem` acceptStates
acceptString dfa (c:cs) =
  case acceptChar dfa c of
    Just newDfa -> acceptString newDfa cs
    Nothing     -> False
