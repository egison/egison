module Language.Egison.Core where
import Language.Egison.Types

eval1 :: Env -> EgisonExpr -> ObjectRef
eval1 env (MatchAllExpr target matcher (pattern, body)) = do
  matchs <- patternMatch MAll env [(MState [] [(MAtom matcher pattern target)])]
  rets <- mapM (\match -> do newEnv <- liftIO $ extendEnv env match
                             objRef <- liftIO $ newIORef (Closure newEnv body)
                             return objRef)
               matchs
  return $ Intermidiate $ ICollection $ map IElement rets


objRefEval1 :: ObjectRef -> Object
objRefEval1 = undefined

patternMatch :: MatchFlag -> Env -> [MatchState] -> IOThrowsError [MatchResult]
patternMatch _ _ [] = return []
patternMatch MOne env ((MState frame []):_) = do
  return [frame]
patternMatch MAll env ((MState frame []):mStates) = do
  ret <- patternMatch MAll env mStates
  return (frame:ret)
patternMatch mFlag env ((MState frame ((MAtom matcher pattern target):mAtoms)):mStates) = do
  pattern1 <- eval1 env pattern
  case pattern1 of
    Intermidiate (IInductiveData _ _) -> do -- same with ValuePat

 