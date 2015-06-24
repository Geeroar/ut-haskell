module TestUtil where

import Distribution.TestSuite

buildTest :: Result -> String -> TestInstance
buildTest result label = TestInstance
  { run = return $ Finished result
  , name = label
  , tags = []
  , options = []
  , setOption = \_ _ -> Right $ buildTest result label
  }
