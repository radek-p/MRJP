module Frontend.SemanticAnalysis.Monad where

import Control.Monad.Except
import Control.Monad.State
import Control.Lens
import qualified Data.Map as M

import Language.BuiltIns
import Frontend.Parser.AbsLatte
import Frontend.SemanticAnalysis.CheckError
import Utility.PrettyPrinting


--------------------------------------
-- Data types                       --
--------------------------------------

type CheckM' s a = StateT s (ExceptT CheckError IO) a
type CheckM  a   = CheckM' CheckState a

data RunMode
  = Normal
  | Debug
  deriving Eq

class HasRunMode a where
  runMode :: Lens' a RunMode

data CheckState
  = CheckState
      Env             -- environment
      Type            -- return type for checked function
      (Env' Variable) -- variables defined in current scope
      RunMode         -- run mode
      (Maybe Class)   -- current class

instance HasRunMode CheckState where
  runMode = lens (\(CheckState _ _ _ rm _) -> rm) (\(CheckState a b c _ e) rm -> CheckState a b c rm e)


--------------------------------------
-- Lenses                           --
--------------------------------------

vEnv' :: Lens' Env (Env' Variable)
fEnv' :: Lens' Env (Env' Function)
cEnv' :: Lens' Env (Env'    Class)
vEnv' = lens (\(ve,  _,  _) -> ve) (\( _, fe, ce) ve -> (ve, fe, ce))
fEnv' = lens (\( _, fe,  _) -> fe) (\(ve,  _, ce) fe -> (ve, fe, ce))
cEnv' = lens (\( _,  _, ce) -> ce) (\(ve, fe,  _) ce -> (ve, fe, ce))

env          :: Lens' CheckState Env
returnType   :: Lens' CheckState Type
currentScope :: Lens' CheckState (Env' Variable)
currentClass :: Lens' CheckState (Maybe Class)
env          = lens (\(CheckState ev _ _ _ _ ) -> ev) (\(CheckState _ rt sc rm cc) ev -> CheckState ev rt sc rm cc)
returnType   = lens (\(CheckState _ rt _ _ _ ) -> rt) (\(CheckState ev _ sc rm cc) rt -> CheckState ev rt sc rm cc)
currentScope = lens (\(CheckState _ _ sc _ _ ) -> sc) (\(CheckState ev rt _ rm cc) sc -> CheckState ev rt sc rm cc)
currentClass = lens (\(CheckState _ _  _ _ cc) -> cc) (\(CheckState ev rt sc rm _) cc -> CheckState ev rt sc rm cc)

vEnv :: Lens' CheckState (Env' Variable)
fEnv :: Lens' CheckState (Env' Function)
cEnv :: Lens' CheckState (Env'    Class)
vEnv = env . vEnv'
fEnv = env . fEnv'
cEnv = env . cEnv'


--------------------------------------
-- Environment access helpers       --
--------------------------------------

getVariable :: Ident -> CheckM Variable
getVariable ident = do
  e <- use vEnv
  case M.lookup ident e of
    Just var -> return var
    Nothing  -> throwTypeError $ VariableNotFound ident

getClass :: Ident -> CheckM Class
getClass ident = do
  e <- use cEnv
  case M.lookup ident e of
    Just cls -> return cls
    Nothing  -> throwTypeError $ ClassNotFound ident

getClassItem :: ((Env' Variable, Env' Function) -> Env' c) -> (Ident -> Class -> TypeError) -> Ident -> Class -> Class -> CheckM c
getClassItem _ err ident orig (Object) =
  throwTypeError $ err ident orig

getClassItem component err ident orig (SubClass _ super fiEnv mEnv) =
  case M.lookup ident (component (fiEnv, mEnv)) of
    Just method -> return method
    Nothing     -> getClassItem component err ident orig super

getField :: Ident -> Class -> CheckM Variable
getField ident cls = getClassItem fst FieldNotFound ident cls cls

getMethod :: Ident -> Class -> CheckM Function
getMethod ident cls = getClassItem snd MethodNotFound ident cls cls

getFunction :: Ident -> CheckM Function
getFunction ident = do
  e <- use fEnv
  case M.lookup ident e of
    Just fun -> return fun
    Nothing  -> throwTypeError $ FunctionNotFound ident


--------------------------------------
-- Error reporting helper functions --
--------------------------------------

throwCheckError :: CEType -> CheckM' a b
throwCheckError et =
  throwError $ CheckError et []

throwTypeError :: TypeError -> CheckM' a b
throwTypeError et =
  throwCheckError $ TypeError et

infixl 0 $$
($$) :: CEContext -> CheckM' a b -> CheckM' a b
x $$ m = m `catchError` (\(CheckError et ctx) -> throwError $ CheckError et (x : ctx))

observeStep :: (Show s, HasRunMode s) => Tree a -> CheckM' s ()
observeStep x = do
  mode <- use runMode
  when (mode == Debug) $
    observeStepInner x
  return ()

instance Show CheckState where
  show st = unlines [
      printBoldWhite "venv:        " ++ show (st ^. vEnv        ),
      printBoldWhite "scope:       " ++ show (st ^. currentScope)
    ]

observeStepInner :: (Show s, HasRunMode s) => Tree a -> CheckM' s ()
observeStepInner x = do
  st <- get
  liftIO $ putStrLn (printBoldWhite "constructor: " ++ (words (show x) !! 0))
  liftIO $ putStrLn (printBoldWhite "tree:        " ++ printTree x)
  liftIO $ putStrLn (show st)
  liftIO $ putStrLn ("#########################################")
  line <- liftIO $ getLine
  when (line == "e") $
    throwCheckError $ OtherException "Interrupt"
  when (line == "r") (runMode .= Normal)
  return ()

-- Features that are not required at this stage
-- were explicitly turned off.
objectsNotSupportedYet :: CheckM' s a
objectsNotSupportedYet = throwCheckError $ FeatureNotSupported "objects"

arraysNotSupportedYet :: CheckM' s a
arraysNotSupportedYet = throwCheckError $ FeatureNotSupported "arrays"