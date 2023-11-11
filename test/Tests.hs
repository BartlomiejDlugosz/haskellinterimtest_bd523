{-# LANGUAGE StandaloneDeriving #-}
import IC.TestSuite
import FunInt

main :: IO ()
main = runTests tests

tests :: [TestGroup]
tests =
  [ testGroup "applyOp" applyOpTests
  , testGroup "apply" applyTests
  , testGroup "eval" evalTests
  , testGroup "isWellFormed" isWellFormedTests
  -- PART 2
  , testGroup "maybeEval" maybeEvalTests
  ]

applyOpTests :: [TestCase]
applyOpTests = [ applyOp "+" 7 4 --> Number 11
               , applyOp "==" 2 2 --> Boolean True
               ]

applyTests :: [TestCase]
applyTests = [ apply (Op "+") [Number 7, Number 4] emptyEnv --> Number 11
             , apply (Op ">") [Number 8, Number 2] emptyEnv --> Boolean True
             , apply minOf2 [Number 6, Number 0] emptyEnv --> Number 0
             ]

evalTests :: [TestCase]
evalTests = [ eval (Number 8) env --> Number 8
            , eval (Id "x") env --> Number 1
            , eval (App (Op "+") [Id "x", Id "y"]) env --> Number 6
            , eval (Op "+") env --> Op "+"
            , eval factOf6 emptyEnv --> Number 720
            ]

isWellFormedTests :: [TestCase]
isWellFormedTests = [ isWellFormed invalid1 --> False
                    , isWellFormed invalid2 --> False
                    ]

maybeEvalTests :: [TestCase]
maybeEvalTests = [ maybeEval factOf6 emptyEnv --> Just (Number 720)
                 , maybeEval typeError1 emptyEnv --> Nothing
                 ]

-- DO NOT REMOVE OR REPLACE THIS LINE
deriving instance Eq Expr
