module Run where

import Control.Monad.Logger
import Control.Monad.Reader
import Database.Persist.Sqlite
import Models
import Transformer.ReaderT (AppT, Env (Env), runAppT, runDB)
import Data.Maybe (fromMaybe)
import Database.Esqueleto.Experimental

weeklyBudget :: Int
weeklyBudget = 100

spend :: Int -> Entity LineItem -> Int
spend n e = n - (lineItemAmount $ entityVal e)

getLineItemTotal :: (Num a, MonadIO m, PersistField a) => SqlPersistT m a
getLineItemTotal = selectSum $ do
  items <- from $ table @LineItem
  pure $ sum_ $ items ^. LineItemAmount
 where
  selectSum = fmap (maybe 0 (fromMaybe 0 . unValue)) . selectOne
  -- Note: in Haskell >= 9, due to simplified subsumption, you will need to do replace selectOne with (\q -> selectOne q)

runApp :: AppT ()
runApp = do
  total <- runDB $ do
    insert_ $ LineItem "Pizza" 11
    insert_ $ LineItem "Burger" 12
    getLineItemTotal
  let remainingBudget = weeklyBudget - total
  liftIO .  putStrLn $ "Remaining Budget: " <> show remainingBudget

run :: IO ()
run = do
  env <- runStderrLoggingT $ Env <$> createSqlitePool ":memory:" 10
  runAppT runApp env