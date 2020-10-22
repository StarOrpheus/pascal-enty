module InterpreterState ( ExecutionMonad
                        , newInterpreterState
                        , InterpreterState (..)
                        , RuntimeError (..)
                        , getVarValue
                        , assignVar
                        , getFunction
                        , getProcedure
                        , declVar
                        , declVars
                        , declConst
                        , declConsts
                        , declFunc
                        , declFunctions
                        , declProc
                        , declProcedures
                        , assertTypeMatch
                        , PASTEvaluatedVariable (..)
                        , updateVariable
                        ) where
import Prelude hiding (lookup)

import Grammar

import Control.Exception (Exception, throwIO)
import Control.Monad.State (MonadIO (liftIO), MonadState (get, put), StateT)
import Data.Array ((//))
import Data.List (foldl')
import Data.Map (Map, fromList, insert, lookup)

--- Pascal Interpreter state, as part of ExecutionMonad
data InterpreterState = InterpreterState
    { stateVarValues      :: Map String Valueble
    , stateDeclFunctions  :: Map String PASTFunctionalDecl
    , stateDeclProcedures :: Map String PASTFunctionalDecl
    } deriving (Show)

--- Returns new InterpreterState
newInterpreterState :: InterpreterState
newInterpreterState = InterpreterState (fromList []) (fromList []) (fromList [])

--- Interpreter's execution monad
type ExecutionMonad a = StateT InterpreterState IO a

--- Runtime error exception type
newtype RuntimeError = RuntimeError String

instance Show RuntimeError where
    show ~(RuntimeError er) = er

instance Exception RuntimeError

assertTypeMatch :: Valueble
                -> PascalType
                -> ExecutionMonad ()
assertTypeMatch value typ =
    case (value, typ) of
        (ValuebleInteger _, PascalIdentType PascalInteger) -> return ()
        (ValuebleReal _, PascalIdentType PascalReal) -> return ()
        (ValuebleString _, PascalIdentType PascalString) -> return ()
        (ValuebleBool _, PascalIdentType PascalBool) -> return ()
        (ValuebleChar _, PascalIdentType PascalChar) -> return ()
        (ValuebleArray _, PascalArrayType _ _) -> return ()
        _ -> liftIO $ throwIO $ RuntimeError $ "Types not matched: expected " ++ show typ
                       ++ ", got " ++ show value


--- Returns variable value from ExecutionMonad
--- Throws RuntimeError when variable not found
getVarValue :: String
            -> ExecutionMonad Valueble
getVarValue varName = do
    (InterpreterState varValues _ _) <- get
    let res = lookup varName varValues
    case res of
        Nothing  -> liftIO $ throwIO $ RuntimeError $ "Cannot find variable, named " ++ show varName
        Just res -> return res

--- Check types equality and assigns variable within ExecutionMonad
--- Throws RuntimeError when types mismatched
assignVar :: String
          -> Valueble
          -> ExecutionMonad ()
assignVar varName varValue = do
    state@(InterpreterState varValues _ _) <- get
    case (lookup varName varValues, varValue) of
        (Nothing, _) -> liftIO $ throwIO $ RuntimeError $ "Assigning non-existing variable " ++ show varName
                               ++ " := " ++ show varValue
        (Just (ValuebleInteger _), ValuebleInteger _) -> return ()
        (Just (ValuebleReal _), ValuebleReal _) -> return ()
        (Just (ValuebleString _), ValuebleString _) -> return ()
        (Just (ValuebleBool _), ValuebleBool _) -> return ()
        (Just (ValuebleChar _), ValuebleChar _) -> return ()
        (Just (ValuebleArray _), ValuebleArray _) -> return ()
        (Just oldVal, newValue) -> liftIO $ throwIO $ RuntimeError $ "Assinging variable a value of different type: "
                       ++ "Old value=" ++ show oldVal
                       ++ " and new=" ++ show newValue
    let newMap = insert varName varValue varValues
    put state { stateVarValues = newMap }

--- Returns function from ExecutionMonad state
--- Throws RuntimeError when function not found
getFunction :: String
            -> ExecutionMonad PASTFunctionalDecl
getFunction funcName = do
    (InterpreterState _ funcDecls _) <- get
    let res = lookup funcName funcDecls
    case res of
        Nothing  -> liftIO $ throwIO $ RuntimeError $ "No such function, named " ++ show funcName
        Just res -> return res

--- Returns procedure from ExecutionMonad state
--- Throws RuntimeError when function not found
getProcedure :: String
             -> ExecutionMonad PASTFunctionalDecl
getProcedure funName = do
    (InterpreterState _ _ procDecls) <- get
    let res = lookup funName procDecls
    case res of
        Nothing  -> liftIO $ throwIO $ RuntimeError $ "No such function, named " ++ show funName
        Just res -> return res

--- Declares variable with specified initial value within ExecutionMonad
declVar :: PASTDeclVar
        -> Valueble
        -> ExecutionMonad ()
declVar ~(PASTDeclVar varName varType) value = do
    assertTypeMatch value varType
    state@(InterpreterState valuesMap _ _) <- get
    let newMap = insert varName value valuesMap
    put state { stateVarValues = newMap }

--- Declares multiple variables with specified initial values within ExecutionMonad
declVars :: [PASTDeclVar]
         -> [Valueble]
         -> ExecutionMonad ()
declVars vars vals =
    foldl' (\acc (newVar, newVal)
            -> acc >> declVar newVar newVal)
           (return ())
           (zip vars vals)

--- Declares const with specified initial value within ExecutionMonad
declConst :: PASTDeclConst
          -> ExecutionMonad ()
declConst ~(PASTDeclConst name value) = do
    state@(InterpreterState valuesMap _ _) <- get
    let newMap = insert name value valuesMap
    put state { stateVarValues = newMap }

--- Declares multiple consts within ExecutionMonad
declConsts :: [PASTDeclConst]
           -> ExecutionMonad ()
declConsts =
    foldl' (\acc decl -> acc >> declConst decl)
           (return ())

--- Declares function within ExecutionMonad
declFunc :: PASTFunctionalDecl
         -> ExecutionMonad ()
declFunc ~self@(PASTDeclFunction name _ _ _) = do
    state@(InterpreterState _ funDecls _) <- get
    let newMap = insert name self funDecls
    put state { stateDeclFunctions = newMap }

--- Declares multiple functions within ExecutionMonad
declFunctions :: [PASTFunctionalDecl]
              -> ExecutionMonad ()
declFunctions =
    foldl' (\acc decl -> acc >> declFunc decl)
           (return ())

--- Declares procedure within ExecutionMonad
declProc :: PASTFunctionalDecl
         -> ExecutionMonad ()
declProc ~self@(PASTDeclProcedure name _ _) = do
    state@(InterpreterState _ _ procDecls) <- get
    let newMap = insert name self procDecls
    put state { stateDeclProcedures = newMap }

--- Declares multiple procedures within ExecutionMonad
declProcedures :: [PASTFunctionalDecl]
               -> ExecutionMonad ()
declProcedures =
    foldl' (\acc decl -> acc >> declProc decl)
           (return ())

--- Form of PASTVariable with evaluated subscript
data PASTEvaluatedVariable = PASTEvaluatedVariable
    { varName      :: String
    , varSubscript :: [Valueble]
    } deriving (Eq, Show)

--- Updates variable with Valueble within ExecutionMonad
--- If variable is an array - uses subscript operator
updateVariable :: PASTEvaluatedVariable
               -> Valueble
               -> ExecutionMonad ()
updateVariable ~self@(PASTEvaluatedVariable varName xs)
               newValue = do
    value <- getVarValue varName
    case (value, length xs) of
        (ValuebleArray _, 0) -> liftIO $ throwIO $ RuntimeError "Assigning array is prohibited, maybe you forgot operator[]?"
        (ValuebleArray arr, 1) -> do
            let index = head xs
            case index of
                ValuebleInteger ind -> assignVar varName $ ValuebleArray (arr // [(ind, newValue)])
                _ -> liftIO $ throwIO $ RuntimeError $ "Array index must be int " ++ show (head xs) ++ " = " ++ show index
        (ValuebleArray _, _) -> liftIO $ throwIO $ RuntimeError $ "Too many args in operator[]: " ++ show self
        (_, 0) -> assignVar varName newValue
        (_, _) -> liftIO $ throwIO $ RuntimeError $ "Too many args in operator[]: " ++ show self
