module StdLib ( checkStandardProcCall
              ) where

import Formatter (inplaceRender)
import Grammar
import InterpreterState

import Control.Exception (throwIO)
import Control.Monad.State
import Data.List (foldl')

callWrite :: [PASTExpression]
          -> (PASTExpression -> ExecutionMonad Valueble)
          -> ExecutionMonad ()
callWrite args' evaluator = do
    args <- traverse evaluator args'
    let str = show =<< args
    liftIO $ putStr str

callWriteLn :: [PASTExpression]
            -> (PASTExpression -> ExecutionMonad Valueble)
            -> ExecutionMonad ()
callWriteLn args' evaluator =
    callWrite args' evaluator >> liftIO (putStrLn "")

extractVarSetter :: (PASTExpression -> ExecutionMonad Valueble)
                 -> PASTExpression
                 -> Valueble
                 -> ExecutionMonad ()
extractVarSetter evaluator expr value =
    case expr of
        (PASTExpression
            (PASTSimpleExpession
                (PASTTerm
                    (PASTSignedFactor
                        Nothing
                        (PASTFactorVariable var))
                    Nothing)
                Nothing)
            Nothing) -> do
                let (PASTVariable varName subs') = var
                subs <- traverse evaluator subs'
                updateVariable (PASTEvaluatedVariable varName subs) value
        _ -> liftIO $ throwIO $
                RuntimeError $ "Expected varnames, but " ++ inplaceRender expr ++ " got"

extractVarReader :: Valueble
                 -> ExecutionMonad Valueble
extractVarReader val =
    case val of
        ValuebleInteger _ -> liftIO $ ValuebleInteger <$> readLn
        ValuebleReal _ -> liftIO $ ValuebleReal <$> readLn
        ValuebleString _ -> liftIO $ ValuebleString <$> readLn
        ValuebleChar _ -> liftIO $ ValuebleChar <$> readLn
        ValuebleBool _ -> liftIO $ ValuebleBool <$> readLn
        _ -> liftIO $ throwIO $ RuntimeError $ "Can't read type of " ++ inplaceRender val

extractVarSetters :: (PASTExpression -> ExecutionMonad Valueble)
                  -> [PASTExpression]
                  -> [Valueble -> ExecutionMonad ()]
extractVarSetters evaluator = do
    let setter = extractVarSetter evaluator
    fmap setter

extractVarReaders :: [Valueble]
                  -> [ExecutionMonad Valueble]
extractVarReaders = fmap extractVarReader

callReadLn :: [PASTExpression]
           -> (PASTExpression -> ExecutionMonad Valueble)
           -> ExecutionMonad ()
callReadLn args' evaluator = do
    let varSetters = extractVarSetters evaluator args'
    args <- traverse evaluator args'
    let varReaders = extractVarReaders args
    let readSetActions = zip varReaders varSetters
    foldl'
        (\acc (a, b) -> acc >> (a >>= b))
        (return ())
        readSetActions

callAssert :: PASTExpression
           -> Valueble
           -> ExecutionMonad ()
callAssert expr' expr =
    case expr of
        ValuebleBool b ->
            if b then
                return ()
            else
                liftIO $ throwIO $ RuntimeError $
                    "Assert failed: Expression " ++ inplaceRender expr' ++ " evaluated to false"
        val -> liftIO $ throwIO $
            RuntimeError $
                "Cannot check assertion for non-bool value: expression " ++ inplaceRender expr'
             ++ " evaluated to " ++ inplaceRender val

callAsserts :: [PASTExpression]
            -> (PASTExpression -> ExecutionMonad Valueble)
            -> ExecutionMonad ()
callAsserts args' evaluator = do
    args <- traverse evaluator args'
    foldl'
        (\acc (a, b) -> acc >> callAssert a b)
        (return ())
        (zip args' args)

--- Called on every procedure call.
--- If procedure name is a known procedure from standard library - Just calls it. Returns Nothing instead.
checkStandardProcCall :: String
                      -> [PASTExpression]
                      -> (PASTExpression -> ExecutionMonad Valueble)
                      -> Maybe (ExecutionMonad ())
checkStandardProcCall "writeln" args' evaluator =
    Just $ callWriteLn args' evaluator
checkStandardProcCall "write" args' evaluator =
    Just $ callWrite args' evaluator
-- checkStandardProcCall "read" args' evaluator =
    -- Just $ callRead args' evaluator
checkStandardProcCall "readln" args' evaluator =
    Just $ callReadLn args' evaluator
checkStandardProcCall "assert" args' evaluator =
    Just $ callAsserts args' evaluator
checkStandardProcCall _ _ _ = Nothing
