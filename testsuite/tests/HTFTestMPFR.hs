{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import Control.Exception
import Distribution.TestSuite
import Data.Approximate.MPFRLowLevel

assertIn a b x = assertBoolVerbose 
     ("Result " ++ show x ++ " in interval [" ++ show a ++ "," ++ show b ++ "]")
      (x > a && x < b)


test_1 =
  let a = fromRationalA Up 70 1.3 :: Rounded
      b = nextAbove a in  
  assertBoolVerbose "a != nextAbove a" (notEqual a b)
       
test_2 =
  let a = fromRationalA Up 70 1.3 :: Rounded
      b = nextAbove a in  
  assertBoolVerbose "nextAbove a != a" (notEqual b a)


main = htfMain htf_thisModulesTests

