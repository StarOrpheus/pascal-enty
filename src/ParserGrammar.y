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

%token AND                                      { Token TokenAND _ }
%token ARRAY                                    { Token TokenARRAY _ }
%token BEGIN                                    { Token TokenBEGIN _ }
%token BOOLEAN                                  { Token TokenBOOLEAN _ }
%token CASE                                     { Token TokenCASE _ }
%token CHAR                                     { Token TokenCHAR _ }
%token CHR                                      { Token TokenCHR _ }
%token CONST                                    { Token TokenCONST _ }
%token DIV                                      { Token TokenDIV _ }
%token DO                                       { Token TokenDO _ }
%token DOWNTO                                   { Token TokenDOWNTO _ }
%token ELSE                                     { Token TokenELSE _ }
%token END                                      { Token TokenEND _ }
%token FILE                                     { Token TokenFILE _ }
%token FOR                                      { Token TokenFOR _ }
%token FUNCTION                                 { Token TokenFUNCTION _ }
%token GOTO                                     { Token TokenGOTO _ }
%token IF                                       { Token TokenIF _ }
%token IN                                       { Token TokenIN _ }
%token INTEGER                                  { Token TokenINTEGER _ }
%token LABEL                                    { Token TokenLABEL _ }
%token MOD                                      { Token TokenMOD _ }
%token NIL                                      { Token TokenNIL _ }
%token NOT                                      { Token TokenNOT _ }
%token OF                                       { Token TokenOF _ }
%token OR                                       { Token TokenOR _ }
%token PACKED                                   { Token TokenPACKED _ }
%token PROCEDURE                                { Token TokenPROCEDURE _ }
%token PROGRAM                                  { Token TokenPROGRAM _ }
%token REAL                                     { Token TokenREAL _ }
%token RECORD                                   { Token TokenRECORD _ }
%token REPEAT                                   { Token TokenREPEAT _ }
%token SET                                      { Token TokenSET _ }
%token THEN                                     { Token TokenTHEN _ }
%token TO                                       { Token TokenTO _ }
%token TYPE                                     { Token TokenTYPE _ }
%token UNTIL                                    { Token TokenUNTIL _ }
%token VAR                                      { Token TokenVAR _ }
%token WHILE                                    { Token TokenWHILE _ }
%token WITH                                     { Token TokenWITH _ }
%token UNIT                                     { Token TokenUNIT _ }
%token INTERFACE                                { Token TokenINTERFACE _ }
%token USES                                     { Token TokenUSES _ }
%token STRING                                   { Token TokenSTRING _ }
%token IMPLEMENTATION                           { Token TokenIMPLEMENTATION _ }
%token TRUE                                     { Token TokenTRUE _ }
%token FALSE                                    { Token TokenFALSE _ }
%token PLUS                                     { Token TokenPLUS _ }
%token MINUS                                    { Token TokenMINUS _ }
%token STAR                                     { Token TokenSTAR _ }
%token SLASH                                    { Token TokenSLASH _ }
%token ASSIGN                                   { Token TokenASSIGN _ }
%token COMMA                                    { Token TokenCOMMA _ }
%token SEMI                                     { Token TokenSEMI _ }
%token COLON                                    { Token TokenCOLON _ }
%token EQ                                       { Token TokenEQ _ }
%token NEQ                                      { Token TokenNEQ _ }
%token LT                                       { Token TokenLT _ }
%token GT                                       { Token TokenGT _ }
%token GE                                       { Token TokenGE _ }
%token LE                                       { Token TokenLE _ }
%token LPAREN                                   { Token TokenLPAREN _ }
%token RPAREN                                   { Token TokenRPAREN _ }
%token LBRACK                                   { Token TokenLBRACKET _ }
%token RBRACK                                   { Token TokenRBRACKET _ }
%token DOT                                      { Token TokenDOT _ }
%token DOTDOT                                   { Token TokenDOTDOT _ }
%token Identifier                               { Token (TokenIdentifier $$) _ }
%token Integer                                  { Token (TokenInteger $$) _ }
%token String                                   { Token (TokenString $$) _ }
%token Real                                     { Token (TokenReal $$) _ }

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
    : variableDeclarationPart blockDeclPart                                     { combineDecls ([], $1, []) $2 }
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
parseError (l:ls) = throwError $ "Syntax error: unexpected token " ++ (show l) ++ " and " ++ (show $ length ls) ++ " left"
parseError [] = throwError "Unexpected EOF"

--- Parse Tokens into AST or return String with error message
parseTokens :: [Token] -> Either String PASTProgram
parseTokens = runExcept . pascalParse

--- Parse String into AST or return String with error message
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
