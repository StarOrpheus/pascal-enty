{-# LANGUAGE InstanceSigs #-}

module Interpreter ( runProgram
                   ) where


import Prelude hiding (lookup)

import Grammar
import StdLib

import Control.Exception (throwIO)
import Control.Monad (when)
import Control.Monad.State (MonadIO (liftIO), MonadState (get, put), StateT (runStateT))
import Data.Array (array, (!))
import Data.Char (toLower)
import Data.Foldable (forM_)
import Data.List (elem, foldl')
import Data.Map (intersectionWithKey)
import InterpreterState

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

evalVariable :: PASTVariable
             -> ExecutionMonad PASTEvaluatedVariable
evalVariable ~(PASTVariable varName xs) = do
    subs <- traverse evalExpr xs
    return $ PASTEvaluatedVariable varName subs

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
            OperatorPLUS  -> callAdd lhs rhs
            OperatorMINUS -> callSub lhs rhs
            OperatorOR    -> callOr lhs rhs


instance CallableBinaryOperator MultiplicativeOperator where
    callBinaryOperator :: MultiplicativeOperator
                       -> Valueble
                       -> Valueble
                       -> ExecutionMonad Valueble
    callBinaryOperator op lhs rhs =
        case op of
            OperatorSTAR  -> callStar lhs rhs
            OperatorSLASH -> callSlash lhs rhs
            OperatorMOD   -> callMod lhs rhs
            OperatorDIV   -> callDiv lhs rhs
            OperatorAND   -> callAnd lhs rhs


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

defaultValue :: PascalType
             -> ExecutionMonad Valueble
defaultValue typ =
    case typ of
        PascalArrayType (PascalSubrangeType from to) componentT
            -> do
                defaultComponent <- defaultValue $ PascalIdentType componentT
                return $ ValuebleArray $ array (from, to)  [(i, defaultComponent) | i <- [from .. to]]
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
        let (InterpreterState oldVarValues _ _) = savedState
        let declList = PASTDeclVar funName resultType : paramDecls
        let declValList = funDefValue : params
        declVars declList declValList
        run block
        funcResult <- getVarValue funName
        currentState <- get
        put $ updateState savedState currentState declList
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
        let declList = paramDecls
        let declValList = params
        declVars declList declValList
        run block
        currentState <- get
        put $ updateState savedState currentState declList
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

instance Runnable PASTStatement where
    run :: PASTStatement -> ExecutionMonad ()
    run (PASTCompoundStatement stmnts) =
        foldl' (\acc newStatement -> acc >> run newStatement)
               (return ())
               stmnts
    run (PASTAssignStatement var' value') = do
        value <- evalExpr value'
        var <- evalVariable var'
        updateVariable var value
    run (PASTProcedureStatement funName params') = do
        params <- traverse evalExpr params'
        let maybeStandard = checkStandardProcCall (map toLower funName) params' evalExpr
        case maybeStandard of
            Nothing -> do
                proc <- getProcedure funName
                callProcedure proc params
            Just standardCall ->
                standardCall
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
    run (PASTForStatement varName (PASTForTo from' to') stmnt') = do
        from <- evalExpr from'
        to1 <- evalExpr to'
        to <- callAdd to1 (ValuebleInteger 1)
        assignVar varName from
        let stmnt = run stmnt' >> incVar varName
        runFor varName to stmnt
    run (PASTForStatement varName (PASTForDownto from' to') stmnt') = do
        from <- evalExpr from'
        to1 <- evalExpr to'
        to <- callSub to1 (ValuebleInteger 1)
        assignVar varName from
        let stmnt = run stmnt' >> decVar varName
        runFor varName to stmnt

updateState :: InterpreterState
            -> InterpreterState
            -> [PASTDeclVar] -- -> [PASTDeclConst]
            -> InterpreterState
updateState ~(InterpreterState oldVarValues oldFunDecls oldProcDecls)
            ~(InterpreterState newVarValues _ _)
            varDecls = do

    let varIsLocal varName = varName `elem` (varDeclName <$> varDecls)

    let chooseVar key oldValue newValue =
            if varIsLocal key then oldValue else newValue

    let varValues = intersectionWithKey chooseVar oldVarValues newVarValues
    InterpreterState varValues oldFunDecls oldProcDecls


splitFuncProcDecls :: [PASTFunctionalDecl]
                   -> ([PASTFunctionalDecl], [PASTFunctionalDecl])
splitFuncProcDecls lst = do
    let isFunctional decl = case decl of
            PASTDeclFunction {}  -> True
            PASTDeclProcedure {} -> False
    let firstPart = filter isFunctional lst
    let secPart = filter (not . isFunctional) lst
    (firstPart, secPart)

instance Runnable PASTProgramBlock where
    run :: PASTProgramBlock -> ExecutionMonad ()
    run ~(PASTProgramBlock constDecls varDecls funDecls' block) = do
        oldState <- get
        defaultValues <- traverse (defaultValue . varDeclType) varDecls
        declConsts constDecls
        declVars varDecls defaultValues
        let (funDecls, procDecls) = splitFuncProcDecls funDecls'
        declFunctions funDecls
        declProcedures procDecls

        run block

        currentState <- get
        put (updateState oldState currentState varDecls)

--- Well, simple AST interpreter
runProgram :: PASTProgram -> IO ()
runProgram prog = do
    let evaluate = runStateT $ run (programBlock prog)
    (result, _) <- evaluate newInterpreterState
    return result
