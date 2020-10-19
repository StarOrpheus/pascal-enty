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

import Data.Array ( Array )

data RelationalOperator = OperatorLT
                        | OperatorLE
                        | OperatorGT
                        | OperatorGE
                        | OperatorEQ
                        | OperatorNE
                        deriving (Eq, Show)

data FactorSignum = SignPLUS
                  | SignMINUS
                  deriving (Eq, Show)

data AdditiveOperator = OperatorPLUS
                      | OperatorMINUS
                      | OperatorOR
                      deriving (Eq, Show)

data MultiplicativeOperator = OperatorSTAR
                            | OperatorSLASH
                            | OperatorMOD
                            | OperatorDIV
                            | OperatorAND
                            deriving (Eq, Show)

data Valueble = ValuebleInteger Int
              | ValuebleReal    Double
              | ValuebleString  String
              | ValuebleBool    Bool
              | ValuebleChar    Char
              | ValuebleArray   (Array Int Valueble)
              deriving (Eq, Show)

data PascalType = PascalSubrangeType Int Int
                | PascalArrayType
                    { arrayIndexType        :: PascalType
                    , arrayComponentType    :: PascalTypeIdentifier
                    }
                | PascalIdentType PascalTypeIdentifier
                deriving (Eq, Show)

data PascalTypeIdentifier = PascalInteger
                          | PascalReal
                          | PascalString
                          | PascalBool
                          | PascalChar
                          deriving (Eq, Show)

data PASTDeclConst = PASTDeclConst
    { constName       :: String
    , constValue      :: Valueble
    } deriving (Eq, Show)

data PASTDeclVar = PASTDeclVar
    { varDeclName     :: String
    , varDeclType     :: PascalType
    } deriving (Eq, Show)


newtype PASTProgramHeading = PASTProgramHeading
    { programNameIdents :: [String]
    } deriving (Eq, Show)

data PASTFunctionalDecl = PASTDeclFunction
                            { functionName           :: String
                            , functionResultType     :: PascalType
                            , functionParamList      :: [PASTDeclVar]
                            -- , functionParamVarList   :: [PASTDeclVar] -- TODO: not supported
                            , functionBlock          :: PASTProgramBlock
                            }
                        | PASTDeclProcedure
                            { functionName           :: String
                            , functionParamList      :: [PASTDeclVar]
                            -- , functionParamVarList   :: [PASTDeclVar]
                            , functionBlock          :: PASTProgramBlock
                            }
                        deriving (Eq, Show)

data PASTFactor = PASTFactorVariable PASTVariable
                | PASTFactorCompound PASTExpression
                | PASTFunctionDisignator
                    { functionDisignatorName    :: String
                    , functionDisignatorParams  :: [PASTExpression]
                    }
                | PASTUnsignedConstant Valueble
                | PASTFactorNot PASTFactor
                deriving (Eq, Show)

data PASTSignedFactor = PASTSignedFactor
    { signedFactorSignum    :: Maybe FactorSignum
    , signedFactor          :: PASTFactor
    } deriving (Eq, Show)

data PASTTerm = PASTTerm
    { termFactor            :: PASTSignedFactor
    , termMulPart           :: Maybe (MultiplicativeOperator, PASTTerm)
    } deriving (Eq, Show)

data PASTSimpleExpession = PASTSimpleExpession
    { simpleExprTerm        :: PASTTerm
    , simpleExprAddPArt     :: Maybe (AdditiveOperator, PASTSimpleExpession)
    } deriving (Eq, Show)

data PASTExpression = PASTExpression
    { expressionSimplePart  :: PASTSimpleExpession
    , expressionRelPart     :: Maybe (RelationalOperator, PASTExpression)
    } deriving (Eq, Show)

data PASTVariable = PASTVariable
    { variableName          :: String
    , variableSubscript     :: [PASTExpression]
    } deriving (Eq, Show)

data PASTForRange = PASTForTo PASTExpression PASTExpression
                  | PASTForDownto PASTExpression PASTExpression
                  deriving (Eq, Show)

data PASTStatement  = PASTCompoundStatement [PASTStatement]
                    | PASTAssignStatement
                        { assignStatementVar        :: PASTVariable
                        , assignStatementExpr       :: PASTExpression
                        }
                    | PASTProcedureStatement
                        { procedureStatementIdent   :: String
                        , procedureStatementParams  :: [PASTExpression]
                        }
                    | PASTEmptyStatement    {- :) -}
                    | PASTConditionalStatement
                        { conditionalExpr           :: PASTExpression
                        , conditionalThenStatement  :: PASTStatement
                        , conditionalElseStatement  :: Maybe PASTStatement
                        }
                    | PASTWhileStatement
                        { whileConditionalExpr      :: PASTExpression
                        , whileStatements           :: PASTStatement
                        }
                    | PASTForStatement
                        { forStatementIdent         :: String
                        , forStatementRange         :: PASTForRange
                        , forStatement              :: PASTStatement
                        }
                    deriving (Eq, Show)

data PASTProgramBlock = PASTProgramBlock
    { blockConstDeclPart    :: [PASTDeclConst]
    , blockVarDeclPart      :: [PASTDeclVar]
    , blockFunctionalPart   :: [PASTFunctionalDecl]
    , blockStatement        :: PASTStatement
    } deriving (Eq, Show)

data PASTProgram = PASTProgram
    { programHeading        :: PASTProgramHeading
    , programBlock          :: PASTProgramBlock
    } deriving (Eq, Show)