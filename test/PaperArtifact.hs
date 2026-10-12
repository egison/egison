{- |
Checks of the interpreter results reported in the appendices of the paper on
type inference for Egison pattern matching.

* The four collection matchers of lib/core/collection.egi are inferred
  without their type annotations, with the same types as the annotated
  definitions, and the collection tests pass with and without the
  annotations.
* The exact list and set matcher listings of the paper are inferred without
  type annotations and pass their 18 cases.  The listings are read from the
  paper sources given by the environment variable PAPER_SOURCES; this check
  is skipped when the variable is not set.
* The Egison side of the comparison with Haskell embeddings in the related
  work: the cons query under `something` is rejected, `chooseMatcher` is
  inferred as Bool -> Matcher (Collection Any) [a], and the paper examples of
  test/lib/core/type-pm-examples.egi pass under strict checking.

Run in the Egison source tree:

  PAPER_SOURCES=/absolute/path/to/paper-source cabal test paper-artifact
-}
module Main (main) where

import           Control.Exception              (SomeException, try)
import           Control.Monad                  (filterM, forM_, unless)
import           Control.Monad.State.Strict     (State, evalState, get, put)
import           Data.Char                      (isAlphaNum, toUpper)
import           Data.List                      (isInfixOf, isPrefixOf)
import qualified Data.Map.Strict                as Map
import           System.Directory               (doesFileExist, getHomeDirectory)
import           System.Environment             (lookupEnv)
import           System.FilePath                ((</>))
import           System.FilePath.Glob           (glob)
import           System.Process                 (readProcess)

import           Test.Framework                 (defaultMain)
import           Test.Framework.Providers.HUnit (hUnitTestToTests)
import           Test.HUnit                     hiding (State)

import           Language.Egison
import           Language.Egison.IExpr          (Var (..))
import           Language.Egison.Type.Env       (lookupEnvExact)
import           Language.Egison.Type.Pretty    (prettyTypeScheme)
import           Language.Egison.Type.Types     (CapVar (..), Capability (..),
                                                 Constraint (..), TyVar (..),
                                                 Type (..), TypeScheme (..))

main :: IO ()
main = defaultMain . hUnitTestToTests $ TestList
  [ libraryInferenceTest
  , listingsTest
  , unsupportedConsTest
  , chooseMatcherTest
  , paperExamplesTest
  ]

-- * Running programs

strictOptions :: EgisonOpts
strictOptions = defaultOption { optTypeCheckStrict = True }

-- | The libraries that `egison -t` loads before a test file, in its order.
libraryLoads :: [TopExpr]
libraryLoads = map Load coreLibraries ++ [Load "lib/math/normalize.egi"]

-- | Evaluate top-level expressions in one batch with strict type checking,
-- as `egison --type-check-strict -t` evaluates the libraries and a test
-- file, and return the inferred types of the given names.
runStrict :: EvalM [TopExpr] -> [String] -> IO (Either EgisonError [Maybe TypeScheme])
runStrict program names = fromEvalM strictOptions $ do
  env <- initialEnv
  exprs <- program
  _ <- evalTopExprsNoPrint env exprs
  typeEnv <- getTypeEnv
  return [lookupEnvExact (Var name []) typeEnv | name <- names]

-- | Rename the type and capability variables of a type scheme to v0, v1, ...
-- in the order in which they are printed.  Two schemes agree up to the
-- renaming of variables exactly when their canonical forms are equal, and
-- the canonical form prints like the former check's records.
canonical :: TypeScheme -> TypeScheme
canonical (Forall _ _ constraints ty) =
  evalState (Forall [] [] <$> mapM constraint constraints <*> typ ty) Map.empty
  where
    constraint (Constraint name tys) = Constraint name <$> mapM typ tys
    typ :: Type -> State (Map.Map (Bool, String) String) Type
    typ t = case t of
      TVar (TyVar v)    -> TVar . TyVar <$> rename (False, v)
      TSkolem (TyVar v) -> TSkolem . TyVar <$> rename (False, v)
      TTuple ts         -> TTuple <$> mapM typ ts
      TCollection a     -> TCollection <$> typ a
      TInductive n ts   -> TInductive n <$> mapM typ ts
      TTensor a         -> TTensor <$> typ a
      THash a b         -> THash <$> typ a <*> typ b
      TMatcher c a      -> TMatcher <$> capability c <*> typ a
      TFun a b          -> TFun <$> typ a <*> typ b
      TIO a             -> TIO <$> typ a
      TIORef a          -> TIORef <$> typ a
      TTerm a atoms     -> (`TTerm` atoms) <$> typ a
      TFrac a           -> TFrac <$> typ a
      TPoly a atoms     -> (`TPoly` atoms) <$> typ a
      _                 -> return t
    capability c = case c of
      CapVar (MkCapVar v)    -> CapVar . MkCapVar <$> rename (True, v)
      CapSkolem (MkCapVar v) -> CapSkolem . MkCapVar <$> rename (True, v)
      CapCon dataType cs     -> CapCon dataType <$> mapM capability cs
      CapTuple cs            -> CapTuple <$> mapM capability cs
      CapAny                 -> return CapAny
    rename key = do
      names <- get
      case Map.lookup key names of
        Just name -> return name
        Nothing -> do
          let name = 'v' : show (Map.size names)
          put (Map.insert key name names)
          return name

showCanonical :: TypeScheme -> String
showCanonical = prettyTypeScheme . canonical

-- | The identity of the evaluated interpreter, for the result records.
interpreterRevision :: IO [String]
interpreterRevision = do
  revision <- git ["rev-parse", "HEAD"]
  status <- git ["status", "--porcelain"]
  return [ "interpreter-commit=" ++ trim revision
         , "interpreter-tree=" ++ (if null (trim status) then "clean" else "modified") ]
  where
    git arguments = do
      result <- try (readProcess "git" arguments "") :: IO (Either SomeException String)
      return (either (const "unknown") id result)
    trim = reverse . dropWhile (`elem` " \n") . reverse

-- * The collection matchers of the library without type annotations

collectionLibrary :: FilePath
collectionLibrary = "lib/core/collection.egi"

collectionTests :: FilePath
collectionTests = "test/lib/core/collection.egi"

collectionMatchers :: [String]
collectionMatchers = ["list", "sortedList", "multiset", "set"]

-- | A definition without its type annotation: the parameters keep their
-- names and the body is unchanged, as in `def list m := ...`.
withoutAnnotation :: TopExpr -> TopExpr
withoutAnnotation (DefineWithType typedVar body)
  | typedVarName typedVar `elem` collectionMatchers =
      let name = VarWithIndices (typedVarName typedVar) (typedVarIndices typedVar)
      in case typedVarParams typedVar of
           []     -> Define name body
           params -> Define name (LambdaExpr (map parameter params) body)
  where
    parameter (TPVar v _)         = Arg (APPatVar (VarWithIndices v []))
    parameter (TPInvertedVar v _) = InvertedArg (APPatVar (VarWithIndices v []))
    parameter (TPTuple ps)        = Arg (APTuplePat (map parameter ps))
    parameter (TPWildcard _)      = Arg APWildCard
    parameter (TPUntypedVar v)    = Arg (APPatVar (VarWithIndices v []))
    parameter TPUntypedWildcard   = Arg APWildCard
withoutAnnotation expr = expr

annotatedNames :: [TopExpr] -> [String]
annotatedNames exprs =
  [typedVarName typedVar | DefineWithType typedVar _ <- exprs
                         , typedVarName typedVar `elem` collectionMatchers]

-- | A probe has the type that a use of the matcher receives.
probeName :: String -> String
probeName (c : cs) = "probe" ++ toUpper c : cs
probeName []       = "probe"

probes :: String
probes = unlines ["def " ++ probeName name ++ " := " ++ name | name <- collectionMatchers]

-- | Library files in ~/.egison take precedence over the source tree, so the
-- two runs would not load the same library.
libraryOverrides :: IO [FilePath]
libraryOverrides = do
  home <- getHomeDirectory
  libraries <- glob "lib/**/*.egi"
  filterM doesFileExist [home </> ".egison" </> library | library <- libraries]

libraryInferenceTest :: Test
libraryInferenceTest =
  TestLabel "library matchers infer the annotated types without annotations" . TestCase $ do
    overrides <- libraryOverrides
    unless (null overrides) $
      assertFailure ("user library files prevent loading the source tree: " ++ show overrides)
    tests <- readFile collectionTests
    collection <- fromEvalM strictOptions (loadFile collectionLibrary)
                    >>= either (assertFailure . show) return
    assertEqual "annotated collection matchers" collectionMatchers (annotatedNames collection)
    assertEqual "annotations left after removal" []
      (annotatedNames (map withoutAnnotation collection))
    -- Both runs read the same library file; only the four annotations differ.
    let assertions = length (filter isAssertion (lines tests))
        names = map probeName collectionMatchers
        program transform = do
          testExprs <- loadFile collectionTests
          probeExprs <- readTopExprs probes
          let replace (Load library) | library == collectionLibrary = map transform collection
              replace expr = [expr]
          return (concatMap replace libraryLoads ++ testExprs ++ probeExprs)
    signedTypes <- inferred "with annotations" =<< runStrict (program id) names
    unsignedTypes <- inferred "without annotations" =<< runStrict (program withoutAnnotation) names
    forM_ (zip3 collectionMatchers signedTypes unsignedTypes) $ \(name, withTypes, withoutTypes) ->
      assertEqual ("inferred type of " ++ name)
        (canonical withTypes) (canonical withoutTypes)
    revision <- interpreterRevision
    putStr . unlines $
      revision ++
      [ "library-matchers-with-annotations-removed=" ++ show (length collectionMatchers)
      , "strictly-inferred-without-annotations=" ++ show (length unsignedTypes)
      , "same-inferred-types=" ++ show (length unsignedTypes)
      , "collection-assertions-passed-per-run=" ++ show assertions ] ++
      [name ++ ": " ++ showCanonical scheme | (name, scheme) <- zip collectionMatchers unsignedTypes]
  where
    isAssertion line = any (`isWord` line) ["assert", "assertEqual"]
    isWord word line =
      word `isPrefixOf` line &&
        case drop (length word) line of
          c : _ -> not (isAlphaNum c || c == '_')
          []    -> True
    inferred label result = case result of
      Left err -> assertFailure (label ++ ": " ++ show err)
      Right schemes ->
        maybe (assertFailure (label ++ ": a probe has no inferred type")) return
          (sequence schemes)

-- * The exact listings of the paper

listingFiles :: [FilePath]
listingFiles = ["list-matcher.egi", "set-matcher.egi", "collection-tests.egi"]

-- | The listings define `list` and `set`, which the library also defines
-- with type annotations.  A definition without an annotation is checked
-- against an earlier annotation of the same name, so the check renames the
-- listings' global names to infer them from their bodies alone.
listingNames :: [(String, String)]
listingNames =
  [ (name, "appendix" ++ toUpper c : cs)
  | name@(c : cs) <- ["splitsWith", "headTailsWith", "list", "subset", "subsets", "set"] ]

-- | Replace whole words; a word consists of letters, digits, and underscores.
renameWords :: [(String, String)] -> String -> String
renameWords table = go
  where
    go [] = []
    go text@(c : cs)
      | isWordChar c =
          let (word, rest) = span isWordChar text
          in maybe word id (lookup word table) ++ go rest
      | otherwise = c : go cs
    isWordChar c = isAlphaNum c || c == '_'

listingsTest :: Test
listingsTest =
  TestLabel "paper listings infer without annotations and pass their cases" . TestCase $ do
    sources <- lookupEnv "PAPER_SOURCES"
    case sources of
      Nothing ->
        putStrLn "Skipped: set PAPER_SOURCES to the paper-source directory to check the listings."
      Just directory -> do
        contents <- mapM (readFile . (directory </>)) listingFiles
        let program = renameWords listingNames (concatMap (++ "\n\n") contents)
        result <- runStrict ((libraryLoads ++) <$> readTopExprs program) []
        case result of
          Left err -> assertFailure ("paper listings: " ++ show err)
          Right _ ->
            putStrLn "Paper listings: strict inference and all 18 cases passed."

-- * The Egison side of the comparison with Haskell embeddings

unsupportedConsTest :: Test
unsupportedConsTest =
  TestLabel "the cons query under something is rejected" . TestCase $ do
    result <- runStrict ((libraryLoads ++) <$> readTopExprs
      "def unsupportedCons := matchAll [1,2,3] as something with $x :: $xs -> x\n") []
    case result of
      Left err ->
        assertBool ("unexpected error: " ++ show err)
          ("matcher capabilities do not unify" `isInfixOf` show err)
      Right _ -> assertFailure "the cons query under something was accepted"

chooseMatcherTest :: Test
chooseMatcherTest =
  TestLabel "chooseMatcher is inferred and both choices give their results" . TestCase $ do
    result <- runStrict ((libraryLoads ++) <$> readTopExprs (unlines
      [ "def unconsWith m xs := matchAll xs as m with $x :: $rest -> (x, rest)"
      , "def chooseMatcher b := if b then list something else multiset something"
      , "assertEqual \"choose-list\" (unconsWith (chooseMatcher True) [1,2,3]) [(1,[2,3])]"
      , "assertEqual \"choose-multiset\" (unconsWith (chooseMatcher False) [1,2,3])"
      , "  [(1,[2,3]),(2,[1,3]),(3,[1,2])]" ]))
      ["chooseMatcher"]
    case result of
      Right [Just scheme@(Forall [] [_] [] _)] -> do
        assertEqual "inferred type of chooseMatcher"
          "Bool -> Matcher (Collection Any) [v0]" (showCanonical scheme)
        putStrLn ("chooseMatcher : " ++ prettyTypeScheme scheme)
      Right other -> assertFailure ("unexpected type of chooseMatcher: " ++ show other)
      Left err -> assertFailure ("chooseMatcher: " ++ show err)

paperExamplesTest :: Test
paperExamplesTest =
  TestLabel "the paper examples pass under strict checking" . TestCase $ do
    result <- runStrict (return (libraryLoads ++
      [ LoadFile "test/lib/core/type-pm-examples.egi"
      , LoadFile "test/type-pm-catch-all.egi"
      ])) []
    either (assertFailure . show) (const (return ())) result
