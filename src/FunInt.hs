module FunInt where
import Data.List
import Data.Maybe
-- these types must not be edited, or they will break the tests
type Program = Expr
type Identifier = String
data Expr = Number Int
          | Boolean Bool
          | Id Identifier
          | Op Identifier
          | Let Identifier Expr Expr
          | If Expr Expr Expr
          | Fun [Identifier] Expr
          | App Expr [Expr]
          deriving Show

-- this type /can/ be modified without affecting the tests
type Environment = [(Identifier, Expr)]

----------------------------------------------------------------------
-- Given...

lookUp :: (Eq a, Show a, Show b) => a -> [(a, b)] -> b
lookUp k t = case lookup k t of
  Nothing -> error ("error: failed to find " ++ show k ++ " in " ++ show t)
  Just binding -> binding

----------------------------------------------------------------------
-- Part I

applyOp :: Identifier -> Int -> Int -> Expr
applyOp "==" a b = Boolean (a == b)
applyOp ">" a b = Boolean (a > b)
applyOp "+" a b = Number (a + b)
applyOp "*" a b = Number (a * b)

apply :: Expr -> [Expr] -> Environment -> Expr
-- Pre: The application is well-formed wrt arity
-- and is correctly typed.
apply (Op op) [Number x1, Number x2] env = applyOp op x1 x2
apply (Fun ids expr) args env = eval expr ((zip ids args) ++ env)

-- The first four rules correspond to redexes; the catch-all
-- corresponds to normal forms...
eval :: Expr -> Environment -> Expr
-- Pre: the expression is well-formed wrt arity and is
-- correctly typed.
eval expr env = case expr of
  Id x -> lookUp x env
  If p q r -> case eval p env of 
    Boolean True -> eval q env
    Boolean False -> eval r env
  Let v e e' -> eval e' ((v, e):env)
  App x y -> apply (eval x env) (evalList y) env
  _ -> expr
  where
    evalList :: [Expr] -> [Expr]
    evalList [] = []
    evalList (x:xs) = eval x env : evalList xs 


--
-- Given.
--
runProgram :: Program -> Expr
runProgram p = eval p emptyEnv

isWellFormed :: Expr -> Bool
isWellFormed expr = case freeVars expr of
  [] -> True
  _ -> False
  where
    freeVars :: Expr -> [Identifier]
    freeVars expr = case expr of
      Number _ -> []
      Boolean _ -> []
      Op _ -> []
      Id x -> [x]
      Let x e e' -> ((freeVars e) `union` (freeVars e')) \\ [x]
      Fun as es -> freeVars es \\ as
      App f as -> freeVars f `union` freeVarsList as
      If e p q -> freeVars e `union` freeVars p `union` freeVars q
      where
        freeVarsList :: [Expr] -> [Identifier]
        freeVarsList [] = []
        freeVarsList (x:xs) = freeVars x `union` freeVarsList xs

----------------------------------------------------------------------
-- Part II

maybeApply :: Expr -> [Expr] -> Environment -> Maybe Expr
-- Pre: The application is well-formed wrt arity.
maybeApply (Op op) [Number x1, Number x2] env = Just (applyOp op x1 x2)
maybeApply (Op op) _ env = Nothing
maybeApply (Fun ids expr) args env = Just (eval expr ((zip ids args) ++ env))

maybeEval :: Expr -> Environment -> Maybe Expr
-- Pre: the expression is well-formed wrt arity
maybeEval expr env = case expr of
  Id x -> lookup x env
  Let v e e' -> maybeEval e' ((v, e):env)
  App x y -> case maybeEval x env of
    Just x' -> maybeApply x' (catMaybes (maybeEvalList y)) env
    Nothing -> Nothing
  If p q r -> case maybeEval p env of 
    Just (Boolean True) -> maybeEval q env
    Just (Boolean False) -> maybeEval r env
    Nothing -> Nothing
  _ -> Just expr
  where
    maybeEvalList :: [Expr] -> [Maybe Expr]
    maybeEvalList (x:xs) = maybeEval x env : maybeEvalList xs

message1, message2 :: String
message1 = "Type error"
message2 = "Program not well-formed (arity check)"

maybeRunProgram :: Expr -> IO()
maybeRunProgram = undefined

----------------------------------------------------------------------
-- Tests referred to in the spec.

emptyEnv :: Environment
emptyEnv = []

env :: Environment
env = [("x", Number 1), ("b", Boolean True), ("y", Number 5)]

minOf2 :: Expr
minOf2 = Fun ["a", "b"] (If (App (Op ">") [Id "a", Id "b"]) (Id "b") (Id "a"))

factOf6 :: Expr
factOf6 =
  Let "fact"
    (Fun ["x"] (If (App (Op "==") [Id "x", Number 0])
                   (Number 1)
                   (App (Op "*") [Id "x",
                                  App (Id "fact") [App (Op "+") [Id "x",
                                                                 Number (-1)]]])
               )
    )
    (App (Id "fact") [Number 6])

app1 :: Expr
app1 = apply (Op "+") [Number 7, Number 4] emptyEnv

app2 :: Expr
app2 = apply (Op ">") [Number 8, Number 2] emptyEnv

app3 :: Expr
app3 =  apply minOf2 [Number 6, Number 0] emptyEnv

eval1 :: Expr
eval1 = eval (Number 8) env

eval2 :: Expr
eval2 = eval (Id "x") env

eval3 :: Expr
eval3 = eval (Op "+") env

eval4 :: Expr
eval4 = eval (App (Op "+") [Id "x", Id "y"]) env

eval5 :: Expr
eval5 = eval factOf6 []

invalid1 :: Expr
invalid1 = Fun ["x"] (App (Op "+") [Id "x", Id "y"])

invalid2 :: Expr
invalid2 = If (App (Op "==") [Number 1, Number 0]) (Id "x") (Number 4)

typeError1 :: Expr
typeError1 = App (Op ">") [Number 1, Op "+"]
