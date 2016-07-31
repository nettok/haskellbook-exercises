{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Todo where

import Control.Monad.IO.Class
import Data.Aeson (ToJSON, FromJSON, encode, decode)
import Data.Fixed
import Data.Maybe
import Data.Time
import GHC.Generics

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL

import qualified Data.Map.Strict as Map
import qualified Database.Redis as R

data Todo = Todo
  { uid :: Maybe Integer
  , text :: String
  , createdTime :: UTCTime
  , completedTime :: Maybe UTCTime
  } deriving (Eq, Show, Generic)

instance ToJSON Todo.Todo
instance FromJSON Todo.Todo

createTodo :: Integer -> String -> UTCTime -> Todo
createTodo todoId txt created = Todo { uid = Just todoId, text = txt, createdTime = created, completedTime = Nothing }

completeTodo :: Todo -> UTCTime -> Todo
completeTodo todo completed = todo { completedTime = Just completed }

uncompleteTodo :: Todo -> Todo
uncompleteTodo todo = todo { completedTime = Nothing }

samples :: Map.Map Integer Todo
samples = Map.fromList
  [ (0, createTodo 0 "elemento 0" (dateTime 2016 07 24 13 13 13))
  , (1, createTodo 1 "elemento 1" (dateTime 2016 07 24 13 14 15))
  ]

findAll :: R.Connection -> String -> IO (Either String [Todo])
findAll conn namespace = R.runRedis conn $ do
    eitherTodos <- R.hvals $ BC.pack namespace
    case eitherTodos of
      Left reply  -> (return . Left) $ show reply
      Right todos -> return $ maybe (Left "Todo.findAll: decoding failed") Right decodedTodos
        where decodedTodos :: Maybe [Todo]
              decodedTodos = sequence $ fmap deserialize todos

find :: R.Connection -> String -> Integer -> IO (Either String (Maybe Todo))
find conn namespace todoId = R.runRedis conn $ find' namespace todoId

find' :: String -> Integer -> R.Redis (Either String (Maybe Todo))
find' namespace todoId = do
  eitherTodo <- R.hget (BC.pack namespace) (BC.pack $ show todoId)
  case eitherTodo of
    Left reply -> (return . Left) $ show reply
    Right maybeTodo -> case maybeTodo of
      Nothing -> (return . Right) Nothing
      Just todo -> return $ maybe (Left "Todo:find: decoding failed") (Right . Just) decodedTodo
        where decodedTodo :: Maybe Todo
              decodedTodo = deserialize todo

deserialize :: BC.ByteString -> Maybe Todo
deserialize = (decode::(BL.ByteString -> Maybe Todo)) . BL.fromStrict

save :: R.Connection -> String -> Todo -> IO (Either String Bool)
save conn namespace todo = R.runRedis conn $ save' namespace todo

save' :: String -> Todo -> R.Redis (Either String Bool)
save' namespace todo =
  let key = show $ fromJust $ uid todo
      value = encode todo
  in do
    eitherBool <- R.hset (BC.pack namespace) (BC.pack key) (BL.toStrict value)
    case eitherBool of
      Left reply -> (return . Left) $ show reply
      Right bool -> (return . Right) bool

delete :: R.Connection -> String -> Integer -> IO (Either String Bool)
delete conn namespace todoId = R.runRedis conn $ do
  eitherCount <- R.hdel (BC.pack namespace) [BC.pack $ show todoId]
  case eitherCount of
    Left reply  -> (return . Left) $ show reply
    Right count | count == 1 -> (return . Right) True
    Right count | count == 0 -> (return . Right) False
    Right count -> fail $ "this should never delete " ++ show count ++ " records"

complete :: R.Connection -> String -> Integer -> IO (Either String (Maybe Todo))
complete conn namespace todoId = updateWith conn namespace todoId (\todo -> fmap (completeTodo todo) getCurrentTime)

uncomplete :: R.Connection -> String -> Integer -> IO (Either String (Maybe Todo))
uncomplete conn namespace todoId = updateWith conn namespace todoId (return . uncompleteTodo)

updateWith :: R.Connection -> String -> Integer -> (Todo -> IO Todo) -> IO (Either String (Maybe Todo))
updateWith conn namespace todoId updateF = R.runRedis conn $ do
  eitherTodo <- find' namespace todoId
  case eitherTodo of
    err@(Left _) -> return err
    Right maybeTodo ->
      case maybeTodo of
        Nothing -> (return . Right) Nothing
        Just todo -> do
          completedTodo <- liftIO $ updateF todo
          saveResult <- save' namespace completedTodo
          case saveResult of
            saveErr@(Left _) -> return $ fmap (const Nothing) saveErr
            Right _ -> (return . Right) $ Just completedTodo

nextUid :: R.Connection -> IO (Either String Integer)
nextUid conn = R.runRedis conn $ do
  eitherUid <- R.hincrby "_seqs" "todo" 1
  case eitherUid of
    Left reply   -> (return . Left) $ show reply
    Right todoId -> (return . Right) todoId

dateTime :: Integer -> Int -> Int -> Int -> Int -> Integer -> UTCTime
dateTime year month day hours minutes seconds = UTCTime  (fromGregorian year month day) (timeOfDayToTime $ fromJust $ makeTimeOfDayValid hours minutes pico)
  where pico = MkFixed seconds * 10 ^ (12 :: Integer)
