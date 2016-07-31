{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.IO.Class
import Network.Wai.Handler.Warp
import Servant

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL

import qualified Database.Redis as R

import qualified Todo

type TodoAPI = "todos" :>
  (    Get '[JSON] [Todo.Todo]
  :<|> ReqBody '[JSON] Todo.Todo :> PostCreated '[JSON] Todo.Todo
  :<|> Capture "uid" Integer :>
    (    Get '[JSON] Todo.Todo
    :<|> DeleteNoContent '[JSON] NoContent
    :<|> "complete"   :> PutNoContent '[JSON] NoContent
    :<|> "uncomplete" :> PutNoContent '[JSON] NoContent
    )
  )

todoServer :: R.Connection -> Server TodoAPI
todoServer conn = getTodos :<|> createTodo :<|> todoOperations
  where getTodos :: Handler [Todo.Todo]
        getTodos = do
          r <- liftIO $ Todo.findAll conn dbNamespace
          case r of
            Right todos -> return todos
            Left  err -> throwError $ errorInterno err

        createTodo :: Todo.Todo -> Handler Todo.Todo
        createTodo todo = do
          eitherTodoId <- liftIO $ Todo.nextUid conn
          case eitherTodoId of
            Right todoId -> do
              let todoWithUid = todo { Todo.uid = Just todoId }
              r <- liftIO $ Todo.save conn dbNamespace todoWithUid
              case r of
                Right _  -> return todoWithUid
                Left err -> throwError $ errorInterno err
            Left err -> throwError $ errorInterno err

        todoOperations todoId = getTodo :<|> deleteTodo :<|> completeTodo :<|> uncompleteTodo
          where getTodo :: Handler Todo.Todo
                getTodo = do
                  r <- liftIO $ Todo.find conn dbNamespace todoId
                  case r of
                    Right maybeTodo -> case maybeTodo of
                      Just todo -> return todo
                      Nothing -> throwError errorTodoNoExiste
                    Left  err -> throwError $ errorInterno err

                deleteTodo :: Handler NoContent
                deleteTodo = do
                  r <- liftIO $ Todo.delete conn dbNamespace todoId
                  case r of
                    Right True  -> return NoContent
                    Right False -> throwError errorTodoNoExiste
                    Left err -> throwError $ errorInterno err

                completeTodo :: Handler NoContent
                completeTodo = do
                  r <- liftIO $ Todo.complete conn dbNamespace todoId
                  case r of
                    Right maybeTodo -> case maybeTodo of
                      Just _ -> return NoContent
                      Nothing -> throwError errorTodoNoExiste
                    Left  err -> throwError $ errorInterno err

                uncompleteTodo :: Handler NoContent
                uncompleteTodo = do
                  r <- liftIO $ Todo.uncomplete conn dbNamespace todoId
                  case r of
                    Right maybeTodo -> case maybeTodo of
                      Just _ -> return NoContent
                      Nothing -> throwError errorTodoNoExiste
                    Left  err -> throwError $ errorInterno err

errorTodoNoExiste :: ServantErr
errorTodoNoExiste = err404 { errBody = (BL.fromStrict . BC.pack) "El todo no existe" }

errorInterno :: String -> ServantErr
errorInterno body = err500 { errBody = (BL.fromStrict . BC.pack) body }

todoAPI :: Proxy TodoAPI
todoAPI = Proxy

webapp :: R.Connection -> Application
webapp conn = serve todoAPI (todoServer conn)

dbNamespace :: String
dbNamespace = "ns2"

main :: IO ()
main = do
  conn <- R.connect R.defaultConnectInfo
  mapM_ (Todo.save conn dbNamespace) Todo.samples
  run 8081 (webapp conn)
