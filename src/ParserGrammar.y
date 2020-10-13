{
module ParserGrammar where

import LexerGrammar

}

%name pascalParse
%tokentype { LexerToken }
%error { parseError }

%token AND                                      { TokenAND }
%token ARRAY                                    { TokenARRAY }
%token BEGIN                                    { TokenBEGIN }
%token BOOLEAN                                  { TokenBOOLEAN }
%token CASE                                     { TokenCASE }
%token CHAR                                     { TokenCHAR }
%token CHR                                      { TokenCHR }
%token CONST                                    { TokenCONST }
%token DIV                                      { TokenDIV }
%token DO                                       { TokenDO }
%token DOWNTO                                   { TokenDOWNTO }
%token ELSE                                     { TokenELSE }
%token END                                      { TokenEND }
%token FILE                                     { TokenFILE }
%token FOR                                      { TokenFOR }
%token FUNCTION                                 { TokenFUNCTION }
%token GOTO                                     { TokenGOTO }
%token IF                                       { TokenIF }
%token IN                                       { TokenIN }
%token INTEGER                                  { TokenINTEGER }
%token LABEL                                    { TokenLABEL }
%token MOD                                      { TokenMOD }
%token NIL                                      { TokenNIL }
%token NOT                                      { TokenNOT }
%token OF                                       { TokenOF }
%token OR                                       { TokenOR }
%token PACKED                                   { TokenPACKED }
%token PROCEDURE                                { TokenPROCEDURE }
%token PROGRAM                                  { TokenPROGRAM }
%token REAL                                     { TokenREAL }
%token RECORD                                   { TokenRECORD }
%token REPEAT                                   { TokenREPEAT }
%token SET                                      { TokenSET }
%token THEN                                     { TokenTHEN }
%token TO                                       { TokenTO }
%token TYPE                                     { TokenTYPE }
%token UNTIL                                    { TokenUNTIL }
%token VAR                                      { TokenVAR }
%token WHILE                                    { TokenWHILE }
%token WITH                                     { TokenWITH }
%token UNIT                                     { TokenUNIT }
%token INTERFACE                                { TokenINTERFACE }
%token USES                                     { TokenUSES }
%token STRING                                   { TokenSTRING }
%token IMPLEMENTATION                           { TokenIMPLEMENTATION }
%token TRUE                                     { TokenTRUE }
%token FALSE                                    { TokenFALSE }
%token PLUS                                     { TokenPLUS }
%token MINUS                                    { TokenMINUS }
%token STAR                                     { TokenSTAR }
%token SLASH                                    { TokenSLASH }
%token ASSIGN                                   { TokenASSIGN }
%token COMMA                                    { TokenCOMMA }
%token SEMI                                     { TokenSEMI }
%token COLON                                    { TokenCOLON }
%token EQ                                       { TokenEQ }
%token NEQ                                      { TokenNEQ }
%token LT                                       { TokenLT }
%token GT                                       { TokenGT }
%token GE                                       { TokenGE }
%token LE                                       { TokenLE }
%token LPAREN                                   { TokenLPAREN }
%token RPAREN                                   { TokenRPAREN }
%token LBRACKET                                 { TokenLBRACKET }
%token RBRACKET                                 { TokenRBRACKET }
%token DOT                                      { TokenDOT }
%token DOTDOT                                   { TokenDOTDOT }
%token Identifier                               { TokenIdentifier $$ }
%token Integer                                  { TokenInteger $$ }
%token String                                   { TokenString $$ }
%token Real                                     { TokenReal $$ }

%%

programHeading 
    : PROGRAM Identifier SEMI                   {()}
    | UNIT Identifier SEMI                      {()}
    | {- Empty heading -}                       {()} 


{
parseError _ = error "Parse error"

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
                            | OperatorSlash
                            | OperatorMOD
                            | OperatorDIV
                            | OperatorAND
                            deriving (Eq, Show)

data PASTProgramHeading = PASTProgramHeading 
    { programNameIdents :: [String]
    } deriving (Eq, Show)

data Valueble = ValuebleInteger Int 
              | ValuebleReal    Double
              | ValuebleString  String 
              | ValuebleBool    Bool
              deriving (Eq, Show)

data PrimitivePascalType = PascalInteger
                         | PascalReal
                         | PascalString
                         deriving (Eq, Show)

data PASTDeclConst = PASTDeclConst 
    { constName       :: String
    , constValue      :: Valueble
    } deriving (Eq, Show)

data PASTDeclVar = PASTDeclVar 
    { varDeclName     :: String
    , varDeclType     :: PrimitivePascalType
    } deriving (Eq, Show)

data PASTFunctionalDecl = PASTDeclFunction  
                            { functionName           :: String
                            , functionResultType     :: PrimitivePascalType
                            , functionParamList      :: [PASTDeclVar]
                            , functionParamVarList   :: [PASTDeclVar]
                            , functionBlock          :: PASTProgramBlock 
                            }
                        | PASTDeclProcedure
                            { functionName           :: String
                            , functionParamList      :: [PASTDeclVar]
                            , functionParamVarList   :: [PASTDeclVar]
                            , functionBlock          :: PASTProgramBlock 
                            }
                        deriving (Eq, Show)

data PASTFactor = PASTFactorVariable
                    { factorVarName             :: String
                    , factorVarArraySlice       :: [PASTExpression]
                    }
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

data PASTStatement  = PASTCompoundStatement [PASTStatement]
                    | PASTAssignStatement 
                        { assignStatementVarName   :: String 
                        , assignStatementExpr      :: PASTExpression
                        }
                    | PASTProcedureStatement 
                        { procedureStatementIdent   :: String 
                        , procedureStatementParams  :: [PASTExpression]
                        }
                    | PASTEmptyStatement    {- :) -}
                    | PASTConditionalStatement
                        { conditionalExpr           :: [PASTExpression]
                        , conditionalThenStatement  :: PASTStatement
                        , conditionalElseStatement  :: Maybe PASTStatement
                        }
                    | PASTWhileStatement
                        { whileConditionalExpr      :: PASTExpression
                        , whileStatements           :: PASTStatement
                        }
                    | PASTForStatement
                        { forStatementIdent         :: String
                        , forStatementRange         :: (PASTExpression, PASTExpression)
                        , forStatement              :: PASTStatement
                        }
                    deriving (Eq, Show)

data PASTProgramBlock = PASTProgramBlock 
    { blockConstDeclPart    :: [PASTDeclConst]
    , blockVarDeclPart      :: [PASTDeclVar]  
    , blockFunctionalPart   :: [PASTFunctionalDecl]
    , blockStatements       :: [PASTStatement]
    } deriving (Eq, Show)

data PASTProgram = PASTProgram 
    { programHeading        :: PASTProgramHeading
    , programBlock          :: PASTProgramBlock
    } deriving (Eq, Show)
}