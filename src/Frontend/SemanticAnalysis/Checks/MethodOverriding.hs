{-# LANGUAGE GADTs #-}
module Frontend.SemanticAnalysis.Checks.MethodOverriding where

import qualified Data.Map as M
import Control.Lens
import Control.Monad
import Control.Monad.Except

import Language.BuiltIns
import Frontend.SemanticAnalysis.Monad
import Frontend.Parser.AbsLatte
import Frontend.SemanticAnalysis.Checks.TypeCorrectness
import Frontend.SemanticAnalysis.CheckError


checkMO :: CheckM ()
checkMO = do
  classes <- uses cEnv (M.elems)
  forM_ classes checkMOClass

checkMOClass :: Class -> CheckM ()
checkMOClass Object = return ()
checkMOClass (SubClass _ super _ _ mEnv) =
  forM_ (M.elems mEnv) $ checkMOMethod super

maybeGetMethod :: Ident -> Class -> CheckM (Maybe Function)
maybeGetMethod ident cls = catchError (do
    method <- getMethod ident cls
    return $ Just method
  ) (\_err -> do
    return Nothing
  )

checkMOMethod :: Class -> Function -> CheckM ()
checkMOMethod super method = do
  mmethod <- maybeGetMethod (getIdent method) super
  case mmethod of
    Just superMethod -> ensureMethodsCompatible (getType method) (getType superMethod)
    Nothing          -> return ()

ensureMethodsCompatible :: Type -> Type -> CheckM ()
ensureMethodsCompatible _sub@(FunT ret1 args1) _super@(FunT ret2 args2) = do
  ret1 <=! ret2                                    -- return type of method in subclass should be subtype of the return type of method defined in superclass
  mapM_ (\(a1, a2) -> a2 <=! a1) (zip args1 args2) -- contravariant types of arguments

ensureMethodsCompatible _ _ = throwCheckError (OtherException "TODO incompatible arg types in overriding of methods")
