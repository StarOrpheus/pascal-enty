{-# LANGUAGE InstanceSigs #-}

module Interpreter ( runProgram
                   ) where


import Prelude hiding (lookup)

import Grammar

import Data.Map ( Map, lookup, insert , member, delete, mapMaybeWithKey, fromList)

import Data.List (foldl')

import Data.Array ( (!), array, (//) )

import Data.Foldable (forM_)

import Control.Monad (when)

import Control.Monad.State

import Control.Exception ( throwIO, Exception )

data InterpreterState = InterpreterState
    { stateVarValues        :: Map String Valueble
    , stateDeclFunctions    :: Map String PASTFunctionalDecl
    , stateDeclProcedures   :: Map String PASTFunctionalDecl
    } deriving (Show)

newInterpreterState :: InterpreterState
newInterpreterState = InterpreterState (fromList []) (fromList []) (fromList [])

type ExecutionMonad a = StateT InterpreterState IO a

newtype RuntimeError = RuntimeError String

instance Show RuntimeError where
    show ~(RuntimeError er) = er

instance Exception RuntimeError

getVarValue :: String
            -> ExecutionMonad Valueble
getVarValue varName = do
    (InterpreterState varValues _ _) <- get
    let res = lookup varName varValues
    case res of
        Nothing     -> liftIO $ throwIO $ RuntimeError $ "Cannot find variable, named " ++ show varName
        Just res    -> return res

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

incVar :: String
       -> ExecutionMonad ()
incVar varName = do
    oldValue <- getVarValue varName
    newValue <- callAdd oldValue (ValuebleInteger 1)
    assignVar varName newValue

decVar :: String
       -> ExecutionMonad ()
decVar varName = do
    oldValue <- getVarValue varName
    newValue <- callSub oldValue (ValuebleInteger 1)
    assignVar varName newValue


getFunction :: String
            -> ExecutionMonad PASTFunctionalDecl
getFunction funcName = do
    (InterpreterState _ funcDecls _) <- get
    let res = lookup funcName funcDecls
    case res of
        Nothing     -> liftIO $ throwIO $ RuntimeError $ "No such function, named " ++ show funcName
        Just res    -> return res

getProcedure :: String
             -> ExecutionMonad PASTFunctionalDecl
getProcedure funName = do
    (InterpreterState _ _ procDecls) <- get
    let res = lookup funName procDecls
    case res of
        Nothing     -> liftIO $ throwIO $ RuntimeError $ "No such function, named " ++ show funName
        Just res    -> return res


cmp :: Valueble -> Valueble -> ExecutionMonad Ordering
cmp lhs' rhs' = case (lhs', rhs') of
    (ValuebleInteger lhs, ValuebleInteger rhs) -> return $ compare lhs rhs
    (ValuebleReal lhs, ValuebleReal rhs) -> return $ compare lhs rhs
    (ValuebleString lhs, ValuebleString rhs) -> return $ compare lhs rhs
    (ValuebleBool lhs, ValuebleBool rhs) -> return $ compare lhs rhs
    (ValuebleChar lhs, ValuebleChar rhs) -> return $ compare lhs rhs
    (ValuebleArray _, ValuebleArray _) -> liftIO $ throwIO $ RuntimeError "Can't compare arrays"
    _ -> liftIO $ throwIO $ RuntimeError $ "Comparing Valuebles of different types: "
                   ++ show lhs'
                   ++ " and "
                   ++ show rhs'

callAdd :: Valueble -> Valueble -> ExecutionMonad Valueble
callAdd lhs' rhs' = case (lhs', rhs') of
    (ValuebleInteger lhs, ValuebleInteger rhs) -> return $ ValuebleInteger (lhs + rhs)
    (ValuebleReal lhs, ValuebleReal rhs) -> return $ ValuebleReal (lhs + rhs)
    _ -> liftIO $ throwIO $ RuntimeError $ "Bad operator+ operands: "
                   ++ show lhs'
                   ++ " and "
                   ++ show rhs'

callSub :: Valueble -> Valueble -> ExecutionMonad Valueble
callSub lhs' rhs' = case (lhs', rhs') of
    (ValuebleInteger lhs, ValuebleInteger rhs) -> return $ ValuebleInteger (lhs - rhs)
    (ValuebleReal lhs, ValuebleReal rhs) -> return $ ValuebleReal (lhs - rhs)
    _ -> liftIO $ throwIO $ RuntimeError $ "Bad operator- operands: "
                   ++ show lhs'
                   ++ " and "
                   ++ show rhs'

callOr :: Valueble -> Valueble -> ExecutionMonad Valueble
callOr lhs' rhs' = case (lhs', rhs') of
    (ValuebleBool lhs, ValuebleBool rhs) -> return $ ValuebleBool (lhs || rhs)
    _ -> liftIO $ throwIO $ RuntimeError $ "Bad operator OR operands: "
                   ++ show lhs'
                   ++ " and "
                   ++ show rhs'

callStar :: Valueble -> Valueble -> ExecutionMonad Valueble
callStar lhs' rhs' = case (lhs', rhs') of
    (ValuebleInteger lhs, ValuebleInteger rhs) -> return $ ValuebleInteger (lhs * rhs)
    (ValuebleReal lhs, ValuebleReal rhs) -> return $ ValuebleReal (lhs * rhs)
    _ -> liftIO $ throwIO $ RuntimeError $ "Bad operator* operands: "
                   ++ show lhs'
                   ++ " and "
                   ++ show rhs'

callSlash :: Valueble -> Valueble -> ExecutionMonad Valueble
callSlash lhs' rhs' = case (lhs', rhs') of
    (ValuebleInteger lhs, ValuebleInteger rhs) -> return $ ValuebleReal (fromIntegral lhs / fromIntegral rhs)
    (ValuebleReal lhs, ValuebleReal rhs) -> return $ ValuebleReal (lhs / rhs)
    _ -> liftIO $ throwIO $ RuntimeError $ "Bad operator/ operands: "
                   ++ show lhs'
                   ++ " and "
                   ++ show rhs'

callMod :: Valueble -> Valueble -> ExecutionMonad Valueble
callMod lhs' rhs' = case (lhs', rhs') of
    (ValuebleInteger lhs, ValuebleInteger rhs) -> return $ ValuebleInteger (lhs `mod` rhs)
    -- (ValuebleReal lhs, ValuebleReal rhs) -> return $ ValuebleInteger (lhs `mod` rhs)
    _ -> liftIO $ throwIO $ RuntimeError $ "Bad operator `mod` operands: "
                   ++ show lhs'
                   ++ " and "
                   ++ show rhs'

callDiv :: Valueble -> Valueble -> ExecutionMonad Valueble
callDiv lhs' rhs' = case (lhs', rhs') of
    (ValuebleInteger lhs, ValuebleInteger rhs) -> return $ ValuebleInteger (lhs `div` rhs)
    _ -> liftIO $ throwIO $ RuntimeError $ "Bad operator `div` operands: "
                    ++ show lhs'
                    ++ " and "
                    ++ show rhs'

callAnd :: Valueble -> Valueble -> ExecutionMonad Valueble
callAnd lhs' rhs' = case (lhs', rhs') of
    (ValuebleBool lhs, ValuebleBool rhs) -> return $ ValuebleBool (lhs && rhs)
    _ -> liftIO $ throwIO $ RuntimeError $ "Bad operator `and` operands: "
                    ++ show lhs'
                    ++ " and "
                    ++ show rhs'

class CallableBinaryOperator a where
    callBinaryOperator :: a
                       -> Valueble
                       -> Valueble
                       -> ExecutionMonad Valueble

instance CallableBinaryOperator RelationalOperator where
    callBinaryOperator :: RelationalOperator
                       -> Valueble
                       -> Valueble
                       -> ExecutionMonad Valueble
    callBinaryOperator op lhs rhs =
        case op of
            OperatorLT -> do
                order <- cmp lhs rhs
                return $ ValuebleBool (order == LT)
            OperatorLE -> do
                order <- cmp lhs rhs
                return $ ValuebleBool (order /= GT)
            OperatorGT -> do
                order <- cmp lhs rhs
                return $ ValuebleBool (order == GT)
            OperatorGE -> do
                order <- cmp lhs rhs
                return $ ValuebleBool (order /= LT)
            OperatorEQ -> do
                order <- cmp lhs rhs
                return $ ValuebleBool (order == EQ)
            OperatorNE -> do
                order <- cmp lhs rhs
                return $ ValuebleBool (order /= EQ)

instance CallableBinaryOperator AdditiveOperator where
    callBinaryOperator :: AdditiveOperator
                       -> Valueble
                       -> Valueble
                       -> ExecutionMonad Valueble
    callBinaryOperator op lhs rhs =
        case op of
            OperatorPLUS -> callAdd lhs rhs
            OperatorMINUS -> callSub lhs rhs
            OperatorOR -> callOr lhs rhs


instance CallableBinaryOperator MultiplicativeOperator where
    callBinaryOperator :: MultiplicativeOperator
                       -> Valueble
                       -> Valueble
                       -> ExecutionMonad Valueble
    callBinaryOperator op lhs rhs =
        case op of
            OperatorSTAR -> callStar lhs rhs
            OperatorSLASH -> callSlash lhs rhs
            OperatorMOD -> callMod lhs rhs
            OperatorDIV -> callDiv lhs rhs
            OperatorAND -> callAnd lhs rhs


class Runnable a where
    --- This function will evaluate function calls, statements and etc.
    --- It *must* undeclare inner variables and functions!
    run :: a -> ExecutionMonad ()

class Evaluatable a where
    evalExpr :: a -> ExecutionMonad Valueble

instance Evaluatable PASTExpression where
    evalExpr (PASTExpression lhs' Nothing) = evalExpr lhs'
    evalExpr (PASTExpression lhs' (Just (op, rhs'))) = do
        lhs <- evalExpr lhs'
        rhs <- evalExpr rhs'
        callBinaryOperator op lhs rhs

instance Evaluatable PASTSimpleExpession where
    evalExpr (PASTSimpleExpession lhs' Nothing) = evalExpr lhs'
    evalExpr (PASTSimpleExpession lhs' (Just (op, rhs'))) = do
        lhs <- evalExpr lhs'
        rhs <- evalExpr rhs'
        callBinaryOperator op lhs rhs

instance Evaluatable PASTTerm where
    evalExpr (PASTTerm lhs' Nothing) = evalExpr lhs'
    evalExpr (PASTTerm lhs' (Just (op, rhs'))) = do
        lhs <- evalExpr lhs'
        rhs <- evalExpr rhs'
        callBinaryOperator op lhs rhs

instance Evaluatable PASTSignedFactor where
    evalExpr (PASTSignedFactor Nothing rhs') = evalExpr rhs'
    evalExpr (PASTSignedFactor (Just sig) rhs') = do
        rhs <- evalExpr rhs'
        case sig of
            SignPLUS -> return rhs
            SignMINUS -> case rhs of
                    ValuebleInteger a -> return (ValuebleInteger (-a))
                    ValuebleReal    a -> return (ValuebleReal (-a))
                    _                 -> liftIO $ throwIO $ RuntimeError $ "Cannot call unary minus on " ++ show rhs'

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

declVar :: PASTDeclVar
        -> Valueble
        -> ExecutionMonad ()
declVar ~(PASTDeclVar varName varType) value = do
    assertTypeMatch value varType
    state@(InterpreterState valuesMap _ _) <- get
    let newMap = insert varName value valuesMap
    put state { stateVarValues = newMap }

declVars :: [PASTDeclVar]
         -> [Valueble]
         -> ExecutionMonad ()
declVars vars vals =
    foldl' (\acc (newVar, newVal)
            -> acc >> declVar newVar newVal)
           (return ())
           (zip vars vals)

declConst :: PASTDeclConst
          -> ExecutionMonad ()
declConst ~(PASTDeclConst name value) = do
    state@(InterpreterState valuesMap _ _) <- get
    let newMap = insert name value valuesMap
    put state { stateVarValues = newMap }

declConsts :: [PASTDeclConst]
           -> ExecutionMonad ()
declConsts =
    foldl' (\acc decl -> acc >> declConst decl)
           (return ())

declFunc :: PASTFunctionalDecl
         -> ExecutionMonad ()
declFunc ~self@(PASTDeclFunction name _ _ _) = do
    state@(InterpreterState _ funDecls _) <- get
    let newMap = insert name self funDecls
    put state { stateDeclFunctions = newMap }

declFunctions :: [PASTFunctionalDecl]
              -> ExecutionMonad ()
declFunctions =
    foldl' (\acc decl -> acc >> declFunc decl)
           (return ())

declProc :: PASTFunctionalDecl
         -> ExecutionMonad ()
declProc ~self@(PASTDeclProcedure name _ _) = do
    state@(InterpreterState _ _ procDecls) <- get
    let newMap = insert name self procDecls
    put state { stateDeclProcedures = newMap }

declProcedures :: [PASTFunctionalDecl]
               -> ExecutionMonad ()
declProcedures =
    foldl' (\acc decl -> acc >> declProc decl)
           (return ())

undeclVar :: String
          -> ExecutionMonad ()
undeclVar varName = do
    state@(InterpreterState valuesMap _ _) <- get
    let newMap = delete varName valuesMap
    put state { stateVarValues = newMap }

undeclVars :: [String]
           -> ExecutionMonad ()
undeclVars =
    foldl' (\acc newName -> acc >> undeclVar newName)
           (return ())

defaultValue :: PascalType
             -> ExecutionMonad Valueble
defaultValue typ =
    case typ of
        PascalArrayType (PascalSubrangeType from to) componentT
            -> return $ ValuebleArray $ array (from, to)  []
        PascalIdentType PascalInteger
            -> return $ ValuebleInteger 0
        PascalIdentType PascalReal
            -> return $ ValuebleReal 0
        PascalIdentType PascalString
            -> return $ ValuebleString ""
        PascalIdentType PascalBool
            -> return $ ValuebleBool False
        PascalIdentType PascalChar
            -> return $ ValuebleChar '\0'
        _ -> liftIO $ throwIO $ RuntimeError $ "Can't generate default value for type "
                       ++ show typ

callFunction :: PASTFunctionalDecl
             -> [Valueble]
             -> ExecutionMonad Valueble
callFunction ~(PASTDeclFunction funName resultType paramDecls block)
             params = do
    let paramLength = length paramDecls
    let actualLen = length params
    if paramLength /= actualLen then
        liftIO $ throwIO $ RuntimeError $ "Function " ++ funName
                   ++ " has " ++ show paramLength ++ " arguments "
                   ++ "but " ++ show actualLen ++ " provided"
    else do
        savedState <- get
        funDefValue <- defaultValue resultType
        let (InterpreterState oldVarValues oldFuncDecls oldProcDecls) = savedState
        let declList = PASTDeclVar funName resultType : paramDecls
        let declValList = funDefValue : params
        declVars declList declValList
        run block
        funcResult <- getVarValue funName
        let staleVars = filter (`member` oldVarValues) (fmap varDeclName declList)
        undeclVars staleVars
        return funcResult

callProcedure :: PASTFunctionalDecl
              -> [Valueble]
              -> ExecutionMonad ()
callProcedure ~(PASTDeclProcedure funName paramDecls block)
              params = do
    let paramLength = length paramDecls
    let actualLen = length params
    if paramLength /= actualLen then
        liftIO $ throwIO $ RuntimeError $ "Function " ++ funName
                   ++ " has " ++ show paramLength ++ " arguments "
                   ++ "but " ++ show actualLen ++ " provided"
    else do
        savedState <- get
        let (InterpreterState oldVarValues oldFuncDecls oldProcDecls) = savedState
        let declList = paramDecls
        let declValList = params
        declVars declList declValList
        run block
        let staleVars = filter (`member` oldVarValues) (fmap varDeclName declList)
        undeclVars staleVars
        return ()

instance Evaluatable PASTFactor where
    evalExpr (PASTFactorVariable var) = evalExpr var
    evalExpr (PASTFactorCompound expr) = evalExpr expr
    evalExpr (PASTFunctionDisignator funcName params') = do
        func <- getFunction funcName
        params <- traverse evalExpr params'
        callFunction func params
    evalExpr (PASTUnsignedConstant c) = return c
    evalExpr (PASTFactorNot fac) = do
        res <- evalExpr fac
        case res of
            ValuebleBool b -> return $ ValuebleBool (not b)
            _ -> liftIO $ throwIO $ RuntimeError $ "Cannot call operator `not` on " ++ show fac ++ " evaluated to " ++ show res

instance Evaluatable PASTVariable where
    evalExpr var = do
        let (PASTVariable varName subs) = var
        varValue <- getVarValue varName
        case (varValue, length subs) of
            (ValuebleInteger i, 0) -> return $ ValuebleInteger i
            (ValuebleReal r, 0) -> return $ ValuebleReal r
            (ValuebleString s, 0) -> return $ ValuebleString s
            (ValuebleBool b, 0) -> return $ ValuebleBool b
            (ValuebleChar c, 0) -> return $ ValuebleChar c
            (ValuebleArray ar, 1) -> do
                index <- evalExpr (head subs)
                case index of
                    ValuebleInteger ind -> return (ar ! ind)
                    _ -> liftIO $ throwIO $ RuntimeError $ "Bad array index type: " ++ show index
            _ -> liftIO $ throwIO $ RuntimeError $ "Bad variable: " ++ show var

runFor :: String
       -> Valueble
       -> ExecutionMonad ()
       -> ExecutionMonad ()
runFor varName tilValue stmnt = do
    current <- getVarValue varName
    if current == tilValue then
        return ()
    else do
        stmnt
        runFor varName tilValue stmnt

updateVariable :: PASTVariable
               -> Valueble
               -> ExecutionMonad ()
updateVariable ~self@(PASTVariable varName xs)
               newValue = do
    value <- getVarValue varName
    case (value, length xs) of
        (ValuebleArray _, 0) -> liftIO $ throwIO $ RuntimeError "Assigning array is prohibited, maybe you forgot operator[]?"
        (ValuebleArray arr, 1) -> do
            index <- evalExpr $ head xs
            case index of
                ValuebleInteger ind -> assignVar varName $ ValuebleArray (arr // [(ind, newValue)])
                _ -> liftIO $ throwIO $ RuntimeError $ "Array index must be int " ++ show (head xs) ++ " = " ++ show index
        (ValuebleArray _, _) -> liftIO $ throwIO $ RuntimeError $ "Too many args in operator[]: " ++ show self
        (_, 0) -> assignVar varName newValue
        (_, _) -> liftIO $ throwIO $ RuntimeError $ "Too many args in operator[]: " ++ show self


instance Runnable PASTStatement where
    run :: PASTStatement -> ExecutionMonad ()
    run (PASTCompoundStatement stmnts) =
        foldl' (\acc newStatement -> acc >> run newStatement)
               (return ())
               stmnts
    run (PASTAssignStatement variable value') = do
        value <- evalExpr value'
        updateVariable variable value
    run (PASTProcedureStatement funName params') = do
        params <- traverse evalExpr params'
        proc <- getProcedure funName
        callProcedure proc params
    run PASTEmptyStatement = return ()
    run (PASTConditionalStatement expr thenCase elseCase) = do
        value' <- evalExpr expr
        value <- case value' of
            ValuebleBool b -> return b
            _ -> liftIO $ throwIO $ RuntimeError $ "IF expression evaluated to non-bool "
                           ++ show expr ++ " = " ++ show value'
        if value then
            run thenCase
        else
            forM_ elseCase run
    run self@(PASTWhileStatement expr stmnt) = do
        value' <- evalExpr expr
        value <- case value' of
            ValuebleBool b -> return b
            _ -> liftIO $ throwIO $ RuntimeError $ "WHILE expression evaluated to non-bool "
                           ++ show expr ++ " = " ++ show value'
        when value $ run stmnt >> run self
    run (PASTForStatement varName range@(PASTForTo from' to') stmnt') = do
        from <- evalExpr from'
        to <- evalExpr to'
        assignVar varName from
        let stmnt = run stmnt' >> incVar varName
        runFor varName to stmnt
    run (PASTForStatement varName range@(PASTForDownto from' to') stmnt') = do
        from <- evalExpr from'
        to <- evalExpr to'
        assignVar varName from
        let stmnt = run stmnt' >> decVar varName
        runFor varName to stmnt

updateState :: InterpreterState
            -> InterpreterState
            -> InterpreterState
updateState ~(InterpreterState oldVarValues oldFunDecls oldProcDecls)
            ~(InterpreterState newVarValues newFunDecls newProcDecls) = do
    let varValues = mapMaybeWithKey (\k _ -> lookup k oldVarValues) newVarValues
    let funcDecls = mapMaybeWithKey (\k _ -> lookup k oldFunDecls) newFunDecls
    let procDecls = mapMaybeWithKey (\k _ -> lookup k oldProcDecls) newProcDecls
    InterpreterState varValues funcDecls procDecls

splitFuncProcDecls :: [PASTFunctionalDecl]
                   -> ([PASTFunctionalDecl], [PASTFunctionalDecl])
splitFuncProcDecls lst = do
    let isFunctional decl = case decl of
            PASTDeclFunction {} -> True
            PASTDeclProcedure {} -> False
    let firstPart = filter isFunctional lst
    let secPart = filter (not . isFunctional) lst
    (firstPart, secPart)

instance Runnable PASTProgramBlock where
    run :: PASTProgramBlock -> ExecutionMonad ()
    run ~self@(PASTProgramBlock constDecls varDecls funDecls' block) = do
        oldState <- get
        defaultValues <- traverse (defaultValue . varDeclType) varDecls
        declConsts constDecls
        declVars varDecls defaultValues
        let (funDecls, procDecls) = splitFuncProcDecls funDecls'
        declFunctions funDecls
        declProcedures procDecls

        run block

        currentState <- get
        put (updateState oldState currentState)

runProgram :: PASTProgram -> IO ()
runProgram prog = do
    let evaluate = runStateT $ run (programBlock prog)
    (result, _) <- evaluate newInterpreterState
    return result