module Frontend.BuiltIns where



import Syntax.LexLatte
import Syntax.ParLatte
import Syntax.SkelLatte
import Syntax.PrintLatte
import Syntax.AbsLatte

objectClassIdent :: Ident
objectClassIdent = Ident "_Object"

-- Built in types
tInt, tBool, tString, tVoid :: Type
[tInt, tBool, tString, tVoid] = map (SimpleT . Ident) ["int", "boolean", "string", "void"]

defaultValue :: Type -> Expr
defaultValue t = case t of
  tInt    -> ELitInt 0
  tBool   -> ELitFalse
  tString -> EString ""
  tVoid   -> error "Assert: Tried to access default value of void type"
  _       -> ELitNull t

