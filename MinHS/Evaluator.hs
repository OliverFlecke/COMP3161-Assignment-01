module MinHS.Evaluator where
import qualified MinHS.Env as E
import MinHS.Syntax
import MinHS.Pretty
import qualified Text.PrettyPrint.ANSI.Leijen as PP

type VEnv = E.Env Value

data Value = I Integer
           | B Bool
           | Nil
           | Cons Integer Value
           -- Others as needed
           deriving (Show)

instance PP.Pretty Value where
  pretty (I i) = numeric $ i
  pretty (B b) = datacon $ show b
  pretty (Nil) = datacon "Nil"
  pretty (Cons x v) = PP.parens (datacon "Cons" PP.<+> numeric x PP.<+> PP.pretty v)
  pretty _ = undefined -- should not ever be used

evaluate :: Program -> Value
evaluate [Bind _ _ _ e] = evalE E.empty e
evaluate bs = evalE E.empty (Let bs (Var "main"))


evalE :: VEnv -> Exp -> Value
-- Simple values
evalE g (Num i) = I i
evalE g (Con "True") = B True
evalE g (Con "False") = B False
-- List
evalE g (Con "Nil") = Nil
evalE g (App (App (Con "Cons") e1) e2) = 
  case evalE g e1 of 
    I i -> Cons i (evalE g e2)
    _   -> error "Only list of integer is supported"

-- Head and tail operator for the list
evalE g (App (Prim Head) e) = 
  case evalE g e of 
    Cons i _  -> I i
    _         -> error "Head is only supported for lists" 
evalE g (App (Prim Tail) e) = 
  case evalE g e of 
    Cons _ tail -> tail
    _           -> error "Tail is only supported for lists"
-- Null operator to check is a list is empty
evalE g (App (Prim Null) e) = 
  case evalE g e of 
    Nil -> B True
    _   -> B False


evalE g e = error "Implement me!" -- For missing cases
