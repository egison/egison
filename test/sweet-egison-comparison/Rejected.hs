{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors -Wno-missing-signatures -Wno-unused-local-binds #-}

-- | Definitions that GHC rejects.  Their type errors are deferred to run
-- time, where Main checks the error messages.
module Rejected (unsupportedCons, chooseMatcher) where

import           Control.Egison

-- | The cons pattern under Something.  Something has no CollectionPattern
-- instance, so the cons pattern cannot be interpreted.
unsupportedCons = matchAll dfs ([1, 2, 3] :: [Int]) Something [[mc| $x : $xs -> x |]]

-- | A choice between two matchers, which have different Haskell types.
chooseMatcher b = if b then List Something else Multiset Something
