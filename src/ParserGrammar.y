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
