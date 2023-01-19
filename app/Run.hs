module Run where

import Data.List (foldl')
import Control.Monad.Logger
import Control.Monad.Reader
import Database.Persist.Sqlite
import Models
import Transformer.ReaderT (AppT, Env (Env), runAppT, runDB)

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

run :: IO ()
run = do
  env <- runStderrLoggingT $ Env <$> createSqlitePool ":memory:" 10
  runAppT runApp env