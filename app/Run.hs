{-# LANGUAGE TypeApplications #-}

import Control.Monad.Logger
import Data.List (foldl')

weeklyBudget :: Int
weeklyBudget = 100

spend :: Int -> Entity LineItem -> Int
spend n e = n - (lineItemAmount $ entityVal e)

runApp :: AppT ()
runApp = do
  lineItems <- runDB $ do
    runMigration migrateAll
    insert_ $ LineItem "Pizza" 11
    insert_ $ LineItem "Burger" 12
    selectList [] []
  let remainingBudget = foldl' spend weeklyBudget lineItems
  liftIO . putStrLn $ "Remaining Budget: " <> show remainingBudget

main :: IO ()
main = do
  env <- runStderrLoggingT $ Env <$> createSqlitePool ":memory:" 10
  runAppT runApp env