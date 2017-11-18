module Main where

import           BasketCase.Account       (load)
-- import BasketCase.Team (loadTeams)

main :: IO ()
main = do
  account <- load "/Users/aadroher/dev/playground/basket-case/harambee_dev"
  putStrLn (show account)
  
