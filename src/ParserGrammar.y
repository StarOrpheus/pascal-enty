{
module ParserGrammar ( parseExpr
                     , parseTokens
                     ) where

import Grammar
import LexerGrammar
import Data.List

import Control.Monad.Except

}

%name pascalParse
%tokentype { Token }
%error { parseError }
%monad { Except String } { (>>=) } { return }

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
%token LBRACK                                   { TokenLBRACKET }
%token RBRACK                                   { TokenRBRACKET }
%token DOT                                      { TokenDOT }
%token DOTDOT                                   { TokenDOTDOT }
%token Identifier                               { TokenIdentifier $$ }
%token Integer                                  { TokenInteger $$ }
%token String                                   { TokenString $$ }
%token Real                                     { TokenReal $$ }

%%

program
    : programHeading block DOT                                                  { PASTProgram $1 $2 }

programHeading
    : PROGRAM Identifier SEMI                                                   { PASTProgramHeading [$2] }
    | PROGRAM Identifier LPAREN identifierList RPAREN SEMI                      { PASTProgramHeading ($2 : $4) }
    | {- Empty heading -}                                                       { PASTProgramHeading [] }

identifierList
    : Identifier                                                                { [$1] }
    | Identifier COMMA identifierList                                           { $1 : $3 }

block
    : blockDeclPart compoundStatement                                           { let (a, b, c) = $1 in PASTProgramBlock a b c $2}
    ;

blockDeclPart
    : constantDefinitionPart blockDeclPart                                      { combineDecls ($1, [], []) $2 }
    | variableDeclarationPart blockDeclPart                                     { combineDecls ([], $1, []) $2 }
    | procedureAndFunctionDeclarationPart blockDeclPart                         { combineDecls ([], [], [$1]) $2 }
    | {- or no declarations -}                                                  { ([], [], []) }

type
   : subrangeType                                                               { $1 }
   | typeIdentifier                                                             { PascalIdentType $1 }
   | arrayType                                                                  { $1 }

subrangeType
    : Integer DOTDOT Integer                                                    { PascalSubrangeType $1 $3 }

typeIdentifier
    : CHAR                                                                      { PascalChar }
    | BOOLEAN                                                                   { PascalBool }
    | INTEGER                                                                   { PascalInteger }
    | REAL                                                                      { PascalReal }
    | STRING                                                                    { PascalString }

arrayType
   : ARRAY LBRACK subrangeType RBRACK OF typeIdentifier                         { PascalArrayType $3 $6 }

constantDefinitionPart
    : CONST constantDefinitions                                                 { $2 }

constantDefinitions
    : constantDefinition SEMI                                                   { [$1] }
    | constantDefinition SEMI constantDefinitions                               { $1 : $3 }

constant
    : Integer                                                                   { ValuebleInteger $1 }
    | Real                                                                      { ValuebleReal $1 }
    | String                                                                    { ValuebleString $1 }
    | bool                                                                      { $1 }

constantDefinition
    : Identifier EQ constant                                                    { PASTDeclConst $1 $3 }

variableDeclaration
    : identifierList COLON type SEMI                                            { zipVarDecls $1 $3 }

variableDeclarations
    : variableDeclaration                                                       { $1 }
    | variableDeclaration variableDeclarations                                  { $1 ++ $2 }

variableDeclarationPart
    : VAR variableDeclarations                                                  { $2 }

procedureAndFunctionDeclarationPart
    : FUNCTION Identifier COLON type SEMI block SEMI                            { PASTDeclFunction $2 $4 [] $6}
    | FUNCTION Identifier formalParameterList COLON type SEMI block SEMI        { PASTDeclFunction $2 $5 $3 $7 }
    | PROCEDURE Identifier SEMI block SEMI                                      { PASTDeclProcedure $2 [] $4}
    | PROCEDURE Identifier formalParameterList SEMI block SEMI                  { PASTDeclProcedure $2 $3 $5 }

formalParameterList
    : LPAREN formalParameterSections RPAREN                                     { $2 }

formalParameterSections
    : formalParameterSection                                                    { $1 }
    -- | formalParameterSection SEMI formalParameterSection                        { combineVarDecls $1 $3 }

formalParameterSection
   : parameterGroup                                                             { $1 }

parameterGroup
   : identifierList COLON typeIdentifier                                        { zipVarDecls $1 (PascalIdentType $3) }

compoundStatement
    : BEGIN statements END                                                      { PASTCompoundStatement $2 }

statements
    : statement                                                                 { [$1] }
    | statement SEMI statements                                                 { $1 : $3 }
    | {- empty statement -}                                                     { [PASTEmptyStatement] }

statement
    : assignmentStatement                                                       { $1 }
    | procedureStatement                                                        { $1 }
    | emptyStatement                                                            { $1 }
    | compoundStatement                                                         { $1 }
    | conditionalStatement                                                      { $1 }
    | repetetiveStatement                                                       { $1 }

assignmentStatement
    : variable ASSIGN expression                                                { PASTAssignStatement $1 $3 }

variable
    : Identifier                                                                { PASTVariable $1 [] }
    | Identifier LBRACK expression RBRACK                                       { PASTVariable $1 [$3] }

procedureStatement
    : Identifier                                                                { PASTProcedureStatement $1 [] }
    | Identifier LPAREN expressionList RPAREN                                   { PASTProcedureStatement $1 $3 }

emptyStatement
    : {- well, literally empty -}                                               { PASTEmptyStatement }

conditionalStatement
    : IF expression THEN statement                                              { PASTConditionalStatement $2 $4 Nothing }
    | IF expression THEN statement ELSE statement                               { PASTConditionalStatement $2 $4 (Just $6) }

repetetiveStatement
    : whileStatement                                                            { $1 }
    | forStatement                                                              { $1 }

whileStatement
   : WHILE expression DO statement                                              { PASTWhileStatement $2 $4 }

forStatement
   : FOR Identifier ASSIGN forList DO statement                                 { PASTForStatement $2 $4 $6 }

forList
   : expression TO expression                                                   { PASTForTo $1 $3 }
   | expression DOWNTO expression                                               { PASTForDownto $1 $3 }

expressionList
    : expression                                                                { [$1] }
    | expression COMMA expressionList                                           { $1 : $3 }

expression
    : simpleExpression                                                          { PASTExpression $1 Nothing }
    | simpleExpression relationaloperator expression                            { PASTExpression $1 (Just ($2, $3)) }

simpleExpression
    : term                                                                      { PASTSimpleExpession $1 Nothing }
    | term additiveoperator simpleExpression                                    { PASTSimpleExpession $1 (Just ($2, $3)) }

term
    : signedFactor                                                              { PASTTerm $1 Nothing }
    | signedFactor multiplicativeoperator term                                  { PASTTerm $1 (Just ($2, $3)) }

signedFactor
    : factor                                                                    { PASTSignedFactor Nothing $1}
    | factorSignum factor                                                       { PASTSignedFactor (Just $1) $2 }

factor
    : variable                                                                  { PASTFactorVariable $1 }
    | LPAREN expression RPAREN                                                  { PASTFactorCompound $2 }
    | functionDesignator                                                        { $1 }
    | unsignedConstant                                                          { PASTUnsignedConstant $1 }
    | NOT factor                                                                { PASTFactorNot $2 }
    | bool                                                                      { PASTUnsignedConstant $1 }

functionDesignator
    : Identifier LPAREN expressionList RPAREN                                   { PASTFunctionDisignator $1 $3 }

unsignedConstant
    : Integer                                                                   { ValuebleInteger $1 }
    | Real                                                                      { ValuebleReal $1 }
    | String                                                                    { ValuebleString $1 }

relationaloperator
    : EQ                                                                        { OperatorEQ }
    | NEQ                                                                       { OperatorNE }
    | LT                                                                        { OperatorLT }
    | GT                                                                        { OperatorGT }
    | LE                                                                        { OperatorLE }
    | GE                                                                        { OperatorGE }

additiveoperator
    : PLUS                                                                      { OperatorPLUS }
    | MINUS                                                                     { OperatorMINUS }
    | OR                                                                        { OperatorOR }

multiplicativeoperator
   : STAR                                                                       { OperatorSTAR }
   | SLASH                                                                      { OperatorSLASH }
   | DIV                                                                        { OperatorDIV }
   | MOD                                                                        { OperatorMOD }
   | AND                                                                        { OperatorAND }

factorSignum
    : PLUS                                                                      { SignPLUS }
    | MINUS                                                                     { SignMINUS }

bool
    : TRUE                                                                      { ValuebleBool True }
    | FALSE                                                                     { ValuebleBool False }

{
parseError :: [Token] -> Except String a
parseError (l:ls) = throwError $ "Unexpected token: " ++ (show l) ++ " and " ++ (show $ 1 + length ls) ++ " left"
parseError [] = throwError "Unexpected EOF"

parseTokens :: [Token] -> Either String PASTProgram
parseTokens = runExcept . pascalParse

parseExpr :: String -> Either String PASTProgram
parseExpr str = runExcept $ do
    let tokenStream = scanTokens str
    pascalParse tokenStream

combineVarDecls :: ([PASTDeclVar], [PASTDeclVar])
                -> ([PASTDeclVar], [PASTDeclVar])
                -> ([PASTDeclVar], [PASTDeclVar])
combineVarDecls (l1, l2) (r1, r2) = (l1 ++ r1, l2 ++ r2)

combineDecls :: ([PASTDeclConst], [PASTDeclVar], [PASTFunctionalDecl])
             -> ([PASTDeclConst], [PASTDeclVar], [PASTFunctionalDecl])
             -> ([PASTDeclConst], [PASTDeclVar], [PASTFunctionalDecl])
combineDecls (l1, l2, l3) (r1, r2, r3) = (l1 ++ r1, l2 ++ r2, l3 ++ r3)

zipVarDecls :: [String] -> PascalType -> [PASTDeclVar]
zipVarDecls varNames typ = foldl' (\acc newVal -> acc ++ [PASTDeclVar newVal typ]) [] varNames
}
