{
module LexerGrammar ( Token(..)
                    , scanTokens
                    ) where

import Grammar

}

%wrapper "basic"

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
    $A $N $D                                        { \_ -> TokenAND }
    $A $R $R $A $Y                                  { \_ -> TokenARRAY }
    $B $E $G $I $N                                  { \_ -> TokenBEGIN }
    $B $O $O $L $E $A $N                            { \_ -> TokenBOOLEAN }
    $C $A $S $E                                     { \_ -> TokenCASE }
    $C $H $A $R                                     { \_ -> TokenCHAR }
    $C $H $R                                        { \_ -> TokenCHR }
    $C $O $N $S $T                                  { \_ -> TokenCONST }
    $D $I $V                                        { \_ -> TokenDIV }
    $D $O                                           { \_ -> TokenDO }
    $D $O $W $N $T $O                               { \_ -> TokenDOWNTO }
    $E $L $S $E                                     { \_ -> TokenELSE }
    $E $N $D                                        { \_ -> TokenEND }
    $F $I $L $E                                     { \_ -> TokenFILE }
    $F $O $R                                        { \_ -> TokenFOR }
    $F $U $N $C $T $I $O $N                         { \_ -> TokenFUNCTION }
    $G $O $T $O                                     { \_ -> TokenGOTO }
    $I $F                                           { \_ -> TokenIF }
    $I $N                                           { \_ -> TokenIN }
    $I $N $T $E $G $E $R                            { \_ -> TokenINTEGER }
    $L $A $B $E $L                                  { \_ -> TokenLABEL }
    $M $O $D                                        { \_ -> TokenMOD }
    $N $I $L                                        { \_ -> TokenNIL }
    $N $O $T                                        { \_ -> TokenNOT }
    $O $F                                           { \_ -> TokenOF }
    $O $R                                           { \_ -> TokenOR }
    $P $A $C $K $E $D                               { \_ -> TokenPACKED }
    $P $R $O $C $E $D $U $R $E                      { \_ -> TokenPROCEDURE }
    $P $R $O $G $R $A $M                            { \_ -> TokenPROGRAM }
    $R $E $A $L                                     { \_ -> TokenREAL }
    $R $E $C $O $R $D                               { \_ -> TokenRECORD }
    $R $E $P $E $A $T                               { \_ -> TokenREPEAT }
    $S $E $T                                        { \_ -> TokenSET }
    $T $H $E $N                                     { \_ -> TokenTHEN }
    $T $O                                           { \_ -> TokenTO }
    $T $Y $P $E                                     { \_ -> TokenTYPE }
    $U $N $T $I $L                                  { \_ -> TokenUNTIL }
    $V $A $R                                        { \_ -> TokenVAR }
    $W $H $I $L $E                                  { \_ -> TokenWHILE }
    $W $I $T $H                                     { \_ -> TokenWITH }
    $U $N $I $T                                     { \_ -> TokenUNIT }
    $I $N $T $E $R $F $A $C $E                      { \_ -> TokenINTERFACE }
    $U $S $E $S                                     { \_ -> TokenUSES }
    $S $T $R $I $N $G                               { \_ -> TokenSTRING }
    $I $M $P $L $E $M $E $N $T $A $T $I $O $N       { \_ -> TokenIMPLEMENTATION }
    $T $R $U $E                                     { \_ -> TokenTRUE }
    $F $A $L $S $E                                  { \_ -> TokenFALSE }
    \+                                              { \_ -> TokenPLUS }
    \-                                              { \_ -> TokenMINUS }
    \*                                              { \_ -> TokenSTAR }
    \/                                              { \_ -> TokenSLASH }
    \: \=                                           { \_ -> TokenASSIGN }
    \,                                              { \_ -> TokenCOMMA }
    \;                                              { \_ -> TokenSEMI }
    \:                                              { \_ -> TokenCOLON }
    \=                                              { \_ -> TokenEQ }
    \< \>                                           { \_ -> TokenNEQ }
    \<                                              { \_ -> TokenLT }
    \>                                              { \_ -> TokenGT }
    \> \=                                           { \_ -> TokenGE }
    \< \=                                           { \_ -> TokenLE }
    \(                                              { \_ -> TokenLPAREN }
    \)                                              { \_ -> TokenRPAREN }
    \[                                              { \_ -> TokenLBRACKET }
    \]                                              { \_ -> TokenRBRACKET }
    \.                                              { \_ -> TokenDOT }
    \. \.                                           { \_ -> TokenDOTDOT }
    $alpha ([a-zA-Z0-9_])*                          { \s -> TokenIdentifier s}
    $digit+                                         { \s -> TokenInteger (read s) }
    \' (\'\' | ~[\'])* \'                           { \s -> TokenString s }
    $digit+ \. $digit+                              {\s -> TokenReal (read s) }

{
data Token =
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

scanTokens :: String -> [Token]
scanTokens = alexScanTokens
}
     