module MinHS.Evaluator where
import qualified MinHS.Env as E
import MinHS.Syntax
import MinHS.Pretty
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Control.Exception

type VEnv = E.Env Value

data Value = I Integer
           | B Bool
           | Nil
           | Cons Integer Value
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
    Just (F fg _ [] e)  -> evalE fg e
    Just e              -> e
    Nothing             -> Nil

-- If then else expressions
evalE g (If b e1 e2) = 
  case evalE g b of 
    B True  -> evalE g e1
    B False -> evalE g e2

-- List
evalE g (Con "Nil") = Nil
evalE g (App (App (Con "Cons") e1) e2) = 
  case evalE g e1 of 
    I i -> Cons i (evalE g e2)
    _   -> error "Only list of integer is supported"

-- Head and tail operator for the list
--evalE g (App (Prim Head) e) = 
--  case evalE g e of 
--    Nil             -> error "The list is empty. Head only works on non-empty lists"
--    Cons i _        -> I i
--    F funEnv f [] e -> 
--      case evalE funEnv e of 
--        Cons i _  -> I i
--        _         -> error "Function did not return a list"
--    _               -> error "Head is only supported for lists" 
--evalE g (App (Prim Tail) e) = 
--  case evalE g e of 
--    Nil             -> error "The list is empty. Tail only works on non-empty lists"
--    Cons _ tail     -> tail
--    F funEnv f [] fe ->
--      case evalE funEnv fe of 
--        Cons _ tail -> tail
--        _           -> error "Function did not return a list"
--    _               -> error "Tail is only supported for lists"
-- Null operator to check is a list is empty
evalE g (App (Prim Null) e) = 
  case evalE g e of 
    Nil -> B True
    _   -> B False

-- Head and tail operator for list and Parital functions operators
evalE g (App (Prim op) e) = 
  case evalE g e of 
    Nil           -> 
      case op of 
        Head  -> error "List is empty. Operators only works on non-empty list" 
        Tail  -> error "List is empty. Operators only works on non-empty list" 
        _     -> Nil
    -- Only for list operators
    Cons h t      -> 
      case op of 
        Head  -> I h
        Tail  -> t
        x     -> error "Operator is not supported for list"
    I n       -> case op of 
      Neg   -> I (-n)
      x     -> F g "" ["$"] (App (App (Prim x) (Var "$")) (Num n))
    F fg n [] fe  -> 
      case evalE fg fe of
        Cons h t  -> 
          case op of 
            Head  -> I h 
            Tail  -> t
            x     -> error "Operator is not supported for list"
    _   -> error "Operators is only supported for this type"

-- Operators for integers
evalE g (App (App (Prim op) e1) e2) =
  case (evalE g e1, evalE g e2) of 
    (Nil, _)    -> Nil
    (_, Nil)    -> Nil
    (I x, I y)  -> 
      case op of 
        Add   -> I (x + y)
        Sub   -> I (x - y)
        Mul   -> I (x * y)
        Quot  -> if y == 0 
                  then error "Division by zero is not allowed"
                  else I (quot x y)
        -- Operators for comparison
        Ge  -> B (x >= y)
        Le  -> B (x <= y)
        Gt  -> B (x > y)
        Lt  -> B (x < y)
        Eq  -> B (x == y)
        Ne  -> B (not (x == y))
    _           -> error "Operators is only supported for integers"

-- Let expression - Variables 
evalE g (Let [] e) = evalE g e
evalE g (Let ((Bind s _ [] e1):xs) e2) = 
  let g' = E.add g (s, (evalE g e1))
   in evalE g' (Let xs e2)
evalE g (Let ((Bind f _ v e1):xs) e2) = 
  let g' = E.add g (f, (F g f v e1))
   in evalE g' (Let xs e2)

-- Let function
evalE g (Letfun (Bind f _ v e)) =
  let g' = E.add g (f, (F g f v e))
  in F g' f v e

-- Letrec
evalE g (Letrec [] e) = evalE g e
evalE g (Letrec ((Bind s t [] e1):xs) e2) = 
  case evalE g e1 of 
    Nil -> evalE g (Letrec (xs ++ [Bind s t [] e1]) e2)
    x   -> let g' = E.add g (s, x)
            in evalE g' (Letrec xs e2)

-- Apply
evalE g (App (Var s) e) =
  case (E.lookup g s) of 
    Just (F fg n v fe)  -> evaluateFunction g (F fg n v fe) e
    Nothing             -> error ("Function " ++ s ++ " not in scope")
evalE g (App fun e) = 
  case evalE g fun of 
    F fg n v fe   -> evaluateFunction g (F fg n v fe) e

-- For missing cases - Shouldn't be any
evalE g e = error ("Implement me!")

-- Evaluate functions
evaluateFunction :: VEnv -> Value -> Exp -> Value
evaluateFunction g (F fg n [] fe) e = 
  let fg' = E.add fg (n, F fg n [] fe) 
  in case evalE fg' fe of 
    F fg'' n' v' fe' -> evaluateFunction g (F fg'' n' v' fe') e
    x                -> x  
evaluateFunction g (F fg n v fe) e =
  let fg' = E.add fg (n, F fg n v fe)
      fg'' = E.add fg' (head v, evalE g e)
  in if tail v == []
      then evalE fg'' fe
      else (F fg'' n (tail v) fe)
