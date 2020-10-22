{
module LexerGrammar ( Token(..)
                    , TokenType(..)
                    , scanTokens
                    ) where

import Grammar

}

%wrapper "posn"

$A          = [aA]
$B          = [bB]
$C          = [cC]
$D          = [dD]
$E          = [eE]
$F          = [fF]
$G          = [gG]
$H          = [hH]
$I          = [iI]
$J          = [jJ]
$K          = [kK]
$L          = [lL]
$M          = [mM]
$N          = [nN]
$O          = [oO]
$P          = [pP]
$Q          = [qQ]
$R          = [rR]
$S          = [sS]
$T          = [tT]
$U          = [uU]
$V          = [vV]
$W          = [wW]
$X          = [xX]
$Y          = [yY]
$Z          = [zZ]

$lowletter  = a-z
$highletter = A-Z
$digit      = 0-9
$alpha      = [a-zA-Z]

tokens :-
    $white+                                         ;
    "{".*"}"                                        ;
    $A $N $D                                        { \pos _ -> Token TokenAND pos }
    $A $R $R $A $Y                                  { \pos _ -> Token TokenARRAY pos }
    $B $E $G $I $N                                  { \pos _ -> Token TokenBEGIN pos }
    $B $O $O $L $E $A $N                            { \pos _ -> Token TokenBOOLEAN pos }
    $C $A $S $E                                     { \pos _ -> Token TokenCASE pos }
    $C $H $A $R                                     { \pos _ -> Token TokenCHAR pos }
    $C $H $R                                        { \pos _ -> Token TokenCHR pos }
    $C $O $N $S $T                                  { \pos _ -> Token TokenCONST pos }
    $D $I $V                                        { \pos _ -> Token TokenDIV pos }
    $D $O                                           { \pos _ -> Token TokenDO pos }
    $D $O $W $N $T $O                               { \pos _ -> Token TokenDOWNTO pos }
    $E $L $S $E                                     { \pos _ -> Token TokenELSE pos }
    $E $N $D                                        { \pos _ -> Token TokenEND pos }
    $F $I $L $E                                     { \pos _ -> Token TokenFILE pos }
    $F $O $R                                        { \pos _ -> Token TokenFOR pos }
    $F $U $N $C $T $I $O $N                         { \pos _ -> Token TokenFUNCTION pos }
    $G $O $T $O                                     { \pos _ -> Token TokenGOTO pos }
    $I $F                                           { \pos _ -> Token TokenIF pos }
    $I $N                                           { \pos _ -> Token TokenIN pos }
    $I $N $T $E $G $E $R                            { \pos _ -> Token TokenINTEGER pos }
    $L $A $B $E $L                                  { \pos _ -> Token TokenLABEL pos }
    $M $O $D                                        { \pos _ -> Token TokenMOD pos }
    $N $I $L                                        { \pos _ -> Token TokenNIL pos }
    $N $O $T                                        { \pos _ -> Token TokenNOT pos }
    $O $F                                           { \pos _ -> Token TokenOF pos }
    $O $R                                           { \pos _ -> Token TokenOR pos }
    $P $A $C $K $E $D                               { \pos _ -> Token TokenPACKED pos }
    $P $R $O $C $E $D $U $R $E                      { \pos _ -> Token TokenPROCEDURE pos }
    $P $R $O $G $R $A $M                            { \pos _ -> Token TokenPROGRAM pos }
    $R $E $A $L                                     { \pos _ -> Token TokenREAL pos }
    $R $E $C $O $R $D                               { \pos _ -> Token TokenRECORD pos }
    $R $E $P $E $A $T                               { \pos _ -> Token TokenREPEAT pos }
    $S $E $T                                        { \pos _ -> Token TokenSET pos }
    $T $H $E $N                                     { \pos _ -> Token TokenTHEN pos }
    $T $O                                           { \pos _ -> Token TokenTO pos }
    $T $Y $P $E                                     { \pos _ -> Token TokenTYPE pos }
    $U $N $T $I $L                                  { \pos _ -> Token TokenUNTIL pos }
    $V $A $R                                        { \pos _ -> Token TokenVAR pos }
    $W $H $I $L $E                                  { \pos _ -> Token TokenWHILE pos }
    $W $I $T $H                                     { \pos _ -> Token TokenWITH pos }
    $U $N $I $T                                     { \pos _ -> Token TokenUNIT pos }
    $I $N $T $E $R $F $A $C $E                      { \pos _ -> Token TokenINTERFACE pos }
    $U $S $E $S                                     { \pos _ -> Token TokenUSES pos }
    $S $T $R $I $N $G                               { \pos _ -> Token TokenSTRING pos }
    $I $M $P $L $E $M $E $N $T $A $T $I $O $N       { \pos _ -> Token TokenIMPLEMENTATION pos }
    $T $R $U $E                                     { \pos _ -> Token TokenTRUE pos }
    $F $A $L $S $E                                  { \pos _ -> Token TokenFALSE pos }
    \+                                              { \pos _ -> Token TokenPLUS pos }
    \-                                              { \pos _ -> Token TokenMINUS pos }
    \*                                              { \pos _ -> Token TokenSTAR pos }
    \/                                              { \pos _ -> Token TokenSLASH pos }
    \: \=                                           { \pos _ -> Token TokenASSIGN pos }
    \,                                              { \pos _ -> Token TokenCOMMA pos }
    \;                                              { \pos _ -> Token TokenSEMI pos }
    \:                                              { \pos _ -> Token TokenCOLON pos }
    \=                                              { \pos _ -> Token TokenEQ pos }
    \< \>                                           { \pos _ -> Token TokenNEQ pos }
    \<                                              { \pos _ -> Token TokenLT pos }
    \>                                              { \pos _ -> Token TokenGT pos }
    \> \=                                           { \pos _ -> Token TokenGE pos }
    \< \=                                           { \pos _ -> Token TokenLE pos }
    \(                                              { \pos _ -> Token TokenLPAREN pos }
    \)                                              { \pos _ -> Token TokenRPAREN pos }
    \[                                              { \pos _ -> Token TokenLBRACKET pos }
    \]                                              { \pos _ -> Token TokenRBRACKET pos }
    \.                                              { \pos _ -> Token TokenDOT pos }
    \. \.                                           { \pos _ -> Token TokenDOTDOT pos }
    $alpha ([a-zA-Z0-9_])*                          { \pos s -> Token (TokenIdentifier s) pos }
    $digit+                                         { \pos s -> Token (TokenInteger (read s)) pos }
    \' (\'\' | ~[\'])* \'                           { \pos s -> Token (TokenString s) pos }
    $digit+ \. $digit+                              { \pos s -> Token (TokenReal (read s)) pos }

{
--- Pascal grammar token type representation
data TokenType =
      TokenAND | TokenARRAY | TokenBEGIN | TokenBOOLEAN
    | TokenCASE | TokenCHAR | TokenCHR | TokenCONST
    | TokenDIV | TokenDO | TokenDOWNTO | TokenELSE
    | TokenEND | TokenFILE | TokenFOR | TokenFUNCTION
    | TokenGOTO | TokenIF | TokenIN | TokenINTEGER
    | TokenLABEL | TokenMOD | TokenNIL | TokenNOT
    | TokenOF | TokenOR | TokenPACKED | TokenPROCEDURE
    | TokenPROGRAM | TokenREAL | TokenRECORD | TokenREPEAT
    | TokenSET | TokenTHEN | TokenTO | TokenTYPE
    | TokenUNTIL | TokenVAR | TokenWHILE | TokenWITH
    | TokenUNIT | TokenINTERFACE | TokenUSES | TokenSTRING
    | TokenIMPLEMENTATION | TokenTRUE | TokenFALSE
    | TokenPLUS | TokenMINUS | TokenSTAR | TokenSLASH
    | TokenASSIGN | TokenCOMMA | TokenSEMI
    | TokenCOLON | TokenEQ | TokenNEQ | TokenLT
    | TokenGT | TokenGE | TokenLE
    | TokenLPAREN | TokenRPAREN | TokenLBRACKET | TokenRBRACKET
    | TokenDOT | TokenDOTDOT
    | TokenIdentifier String
    | TokenInteger Int
    | TokenString String
    | TokenReal Double
    deriving (Eq, Show)

--- Pascal token with source location
data Token = Token
    { tokenType         :: TokenType
    , sourceLocation    :: AlexPosn
    } deriving (Eq)

instance Show Token where
    show (Token tokenType ~(AlexPn offset lineNum colNum)) =
         (show tokenType)
     ++ "(" ++ show lineNum ++ "; " ++ show colNum ++ ")"

--- Parse the given String into Tokens
scanTokens :: String -> [Token]
scanTokens = alexScanTokens
}
