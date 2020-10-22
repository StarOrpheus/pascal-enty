module Grammar  ( RelationalOperator(..)
                , FactorSignum(..)
                , AdditiveOperator(..)
                , MultiplicativeOperator(..)
                , PASTProgramHeading(..)
                , Valueble(..)
                , PascalTypeIdentifier(..)
                , PascalType(..)
                , PASTDeclConst(..)
                , PASTDeclVar(..)
                , PASTFunctionalDecl(..)
                , PASTFactor(..)
                , PASTSignedFactor(..)
                , PASTTerm(..)
                , PASTSimpleExpession(..)
                , PASTExpression(..)
                , PASTVariable(..)
                , PASTForRange(..)
                , PASTStatement(..)
                , PASTProgramBlock(..)
                , PASTProgram(..)
                ) where

import Data.Array (Array)

--- Sum type of supported relational operators
data RelationalOperator = OperatorLT
                        | OperatorLE
                        | OperatorGT
                        | OperatorGE
                        | OperatorEQ
                        | OperatorNE
                        deriving (Eq, Show)

--- Sum type of supported unary operators
data FactorSignum = SignPLUS
                  | SignMINUS
                  deriving (Eq, Show)

--- Sum type for supported additive operators
data AdditiveOperator = OperatorPLUS
                      | OperatorMINUS
                      | OperatorOR
                      deriving (Eq, Show)

--- Sum type of supported multiplicative operators
data MultiplicativeOperator = OperatorSTAR
                            | OperatorSLASH
                            | OperatorMOD
                            | OperatorDIV
                            | OperatorAND
                            deriving (Eq, Show)

--- Sum type for all supported Pascal valuebles
data Valueble = ValuebleInteger Int
              | ValuebleReal    Double
              | ValuebleString  String
              | ValuebleBool    Bool
              | ValuebleChar    Char
              | ValuebleArray   (Array Int Valueble)
              deriving (Eq, Show)

--- Pascal type representation
data PascalType = PascalSubrangeType Int Int
                | PascalArrayType
                    { arrayIndexType     :: PascalType
                    , arrayComponentType :: PascalTypeIdentifier
                    }
                | PascalIdentType PascalTypeIdentifier
                deriving (Eq, Show)

--- Sum type of all supported Pascal type identifiers
data PascalTypeIdentifier = PascalInteger
                          | PascalReal
                          | PascalString
                          | PascalBool
                          | PascalChar
                          deriving (Eq, Show)

--- Const declaration in AST representation
data PASTDeclConst = PASTDeclConst
    { constName  :: String
    , constValue :: Valueble
    } deriving (Eq, Show)

--- Single variable declaration in AST representation
data PASTDeclVar = PASTDeclVar
    { varDeclName :: String
    , varDeclType :: PascalType
    } deriving (Eq, Show)

--- Pascal program header in AST representation
newtype PASTProgramHeading = PASTProgramHeading
    { programNameIdents :: [String]
    } deriving (Eq, Show)

--- Sum type for functional declaraion in AST representation
data PASTFunctionalDecl = PASTDeclFunction
                            { functionName       :: String
                            , functionResultType :: PascalType
                            , functionParamList  :: [PASTDeclVar]
                            -- , functionParamVarList   :: [PASTDeclVar] -- TODO: not supported
                            , functionBlock      :: PASTProgramBlock
                            }
                        | PASTDeclProcedure
                            { functionName      :: String
                            , functionParamList :: [PASTDeclVar]
                            -- , functionParamVarList   :: [PASTDeclVar]
                            , functionBlock     :: PASTProgramBlock
                            }
                        deriving (Eq, Show)

--- Pascal factor in AST representation
data PASTFactor = PASTFactorVariable PASTVariable
                | PASTFactorCompound PASTExpression
                | PASTFunctionDisignator
                    { functionDisignatorName   :: String
                    , functionDisignatorParams :: [PASTExpression]
                    }
                | PASTUnsignedConstant Valueble
                | PASTFactorNot PASTFactor
                deriving (Eq, Show)

--- Pascal signed factor in AST representation
data PASTSignedFactor = PASTSignedFactor
    { signedFactorSignum :: Maybe FactorSignum
    , signedFactor       :: PASTFactor
    } deriving (Eq, Show)

--- Pascal term in AST representation
data PASTTerm = PASTTerm
    { termFactor  :: PASTSignedFactor
    , termMulPart :: Maybe (MultiplicativeOperator, PASTTerm)
    } deriving (Eq, Show)

--- Pascal simple expression in AST representation
data PASTSimpleExpession = PASTSimpleExpession
    { simpleExprTerm    :: PASTTerm
    , simpleExprAddPArt :: Maybe (AdditiveOperator, PASTSimpleExpession)
    } deriving (Eq, Show)

--- Pascal expression in AST representation
data PASTExpression = PASTExpression
    { expressionSimplePart :: PASTSimpleExpession
    , expressionRelPart    :: Maybe (RelationalOperator, PASTExpression)
    } deriving (Eq, Show)

--- Pascal variable usage in AST representation
data PASTVariable = PASTVariable
    { variableName      :: String
    , variableSubscript :: [PASTExpression]
    } deriving (Eq, Show)

--- Sum type for pascal ranges in for loop
data PASTForRange = PASTForTo PASTExpression PASTExpression
                  | PASTForDownto PASTExpression PASTExpression
                  deriving (Eq, Show)

--- Pascal statement in AST representation
data PASTStatement  = PASTCompoundStatement [PASTStatement]
                    | PASTAssignStatement
                        { assignStatementVar  :: PASTVariable
                        , assignStatementExpr :: PASTExpression
                        }
                    | PASTProcedureStatement
                        { procedureStatementIdent  :: String
                        , procedureStatementParams :: [PASTExpression]
                        }
                    | PASTEmptyStatement    {- :) -}
                    | PASTConditionalStatement
                        { conditionalExpr          :: PASTExpression
                        , conditionalThenStatement :: PASTStatement
                        , conditionalElseStatement :: Maybe PASTStatement
                        }
                    | PASTWhileStatement
                        { whileConditionalExpr :: PASTExpression
                        , whileStatements      :: PASTStatement
                        }
                    | PASTForStatement
                        { forStatementIdent :: String
                        , forStatementRange :: PASTForRange
                        , forStatement      :: PASTStatement
                        }
                    deriving (Eq, Show)

--- Pascal's program block representation
data PASTProgramBlock = PASTProgramBlock
    { blockConstDeclPart  :: [PASTDeclConst]
    , blockVarDeclPart    :: [PASTDeclVar]
    , blockFunctionalPart :: [PASTFunctionalDecl]
    , blockStatement      :: PASTStatement
    } deriving (Eq, Show)

--- Pascal program in AST representation
data PASTProgram = PASTProgram
    { programHeading :: PASTProgramHeading
    , programBlock   :: PASTProgramBlock
    } deriving (Eq, Show)
