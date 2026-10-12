{-# LANGUAGE QuasiQuotes  #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | The unannotated function of the comparison.  GHC infers its type, which
-- InferredType checks.
module Definitions (unconsWith) where

import           Control.Egison

unconsWith m xs = matchAll dfs xs m [[mc| $x : $rest -> (x, rest) |]]
