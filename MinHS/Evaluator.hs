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
           | F VEnv [Char] [[Char]] Exp
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




-- Evaluator function
evalE :: VEnv -> Exp -> Value
-- Simple values
evalE g (Num n) = I n
evalE g (Con "True") = B True
evalE g (Con "False") = B False

-- Variable
evalE g (Var s) = 
  case E.lookup g s of 
    Just (F funEnv _ [] funExpr)  -> evalE funEnv funExpr
    Just e                        -> e
    Nothing                       -> error $ "Variable " ++ s ++ " is not in scope"

-- If then else expressions
evalE g (If b e1 e2) = 
  case evalE g b of 
    B True  -> evalE g e1
    B False -> evalE g e2
    _       -> error "The expression could not be evaluated to a boolean"





-- List
evalE g (Con "Nil") = Nil
evalE g (App (App (Con "Cons") e1) e2) = 
  case evalE g e1 of 
    I i -> Cons i (evalE g e2)
    _   -> error "Only list of integer is supported"
-- Head and tail operator for the list
evalE g (App (Prim Head) e) = 
  case evalE g e of 
    Nil             -> error "The list is empty. Head only works on non-empty lists"
    Cons i _        -> I i
    F funEnv f [] e -> 
      case evalE funEnv e of 
        Cons i _  -> I i
        _         -> error "Function did not return a list"
    _               -> error "Head is only supported for lists" 
evalE g (App (Prim Tail) e) = 
  case evalE g e of 
    Nil             -> error "The list is empty. Tail only works on non-empty lists"
    Cons _ tail     -> tail
    F funEnv f [] e ->
      case evalE funEnv e of 
        Cons _ tail -> tail
        _           -> error "Function did not return a list"
    _               -> error "Tail is only supported for lists"
-- Null operator to check is a list is empty
evalE g (App (Prim Null) e) = 
  case evalE g e of 
    Nil -> B True
    _   -> B False



-- Parital functions
evalE g (App (Prim Add) e) = 
  case evalE g e of 
    I n -> F g "" ["x"] (App (App (Prim Add) (Num n)) (Var "x"))
    _   -> error "Addition is only supported for integers"



-- Operators for integers
evalE g (App (Prim op) e) = 
  case evalE g e of 
    I n -> 
      case op of 
        Neg -> I (-n)
        _   -> error "Operator is not yet supported"
    _   -> error "Operators is only supported for integers"

--evalE g (App (Prim Neg) e) = 
--  case evalE g e of 
--    I n -> I (-n)
--    _   -> error "Negation is only supported for integers"
evalE g (App (App (Prim Add) e1) e2) = 
  case (evalE g e1, evalE g e2) of 
    (I x, I y)  -> I (x + y)
    _           -> error "Addition is only supported for integers"  
evalE g (App (App (Prim Sub) e1) e2) = 
  case (evalE g e1, evalE g e2) of 
    (I x, I y)  -> I (x - y) 
    _           -> error "Subtration is only supported for integers"
evalE g (App (App (Prim Quot) e1) e2) = 
  case (evalE g e1, evalE g e2) of 
    (_, I 0)    -> error "Division by zero not allowed!"
    (I x, I y)  -> I (quot x y)
    _           -> error "Division is only supported for integers"
evalE g (App (App (Prim Mul) e1) e2) = 
  case (evalE g e1, evalE g e2) of 
    (I x, I y)  -> I (x * y)
    _           -> error "Multiplcation is only supported for integers"

-- Operators for comparison
evalE g (App (App (Prim op) e1) e2) =
  case (evalE g e1, evalE g e2) of 
    (I x, I y)  ->
        case op of 
          Ge  -> B (x >= y)
          Le  -> B (x <= y)
          Gt  -> B (x > y)
          Lt  -> B (x < y)
          Eq  -> B (x == y)
          Ne  -> B (not (x == y))
          _   -> error "Comparison operator not yet implemented"
    _           -> error "Operators are not supported for integers"

--evalE g (App (App (Prim Ge) e1) e2) =
--  case (evalE g e1, evalE g e2) of 
--    (I x, I y)  -> B (x >= y)
--    _           -> error "Comparison is only supported for integers"
--evalE g (App (App (Prim Le) e1) e2) = 
--  case (evalE g e1, evalE g e2) of 
--    (I x, I y)  -> B (x <= y)
--    _           -> error "Comparison is only supported for integers"
--evalE g (App (App (Prim Gt) e1) e2) = 
--  case (evalE g e1, evalE g e2) of 
--    (I x, I y)  -> B (x > y) 
--    _           -> error "Comparison is only supported for integers"    
--evalE g (App (App (Prim Lt) e1) e2) =
--  case (evalE g e1, evalE g e2) of 
--    (I x, I y)  -> B (x < y) 
--    _           -> error "Comparison is only supported for integers"

-- Equality and inequality
--evalE g (App (App (Prim Eq) e1) e2) = 
--  case (evalE g e1, evalE g e2) of 
--    (I x, I y)  -> B (x == y) 
--    _           -> error "Equality is only supported for integers"
--evalE g (App (App (Prim Ne) e1) e2) =
--  case (evalE g (App (App (Prim Eq) e1) e2)) of
--    B b -> B (not b)



-- Let expression - Variables 
evalE g (Let [Bind s _ [] e1] e2) = 
  let g' = E.add g (s, (evalE g e1))
   in evalE g' e2

-- Let function
evalE g (Letfun (Bind f _ v e)) =
  let g' = E.add g (f, (F g f v e))
  in F g' f v e

-- Apply
evalE g (App (Var s) e) =
  case (E.lookup g s) of 
    Just (F fg n v fe) -> 
      evaluateFunction g (F fg n v fe) e
    Nothing -> error ("Function " ++ s ++ " not in scope")
evalE g (App fun e) = 
  case evalE g fun of 
    F fg n v fe -> 
      evaluateFunction g (F fg n v fe) e

-- For missing cases
evalE g e = error ("Implement me!")

-- Evaluate functions
evaluateFunction :: VEnv -> Value -> Exp -> Value
evaluateFunction g (F fg n v fe) e =
  let fg' = E.add fg (n, F fg n v fe)
  in if v == []
      then evalE fg' fe 
      else 
        let fg'' = E.add fg' (head v, evalE g e)
        in if tail v == []
          then evalE fg'' fe
          else (F fg'' n (tail v) fe)
