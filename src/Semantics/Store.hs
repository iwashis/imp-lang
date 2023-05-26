module Semantics.Store (emptyStore, VariableName, Value, Store) where

import Data.Map

type VariableName = String
type Value = Int

-- First we define Store which can be thought of as
-- memory of stored values under variables that are
-- used in the languages.
type Store = Map VariableName Value

emptyStore :: Store
emptyStore = empty
