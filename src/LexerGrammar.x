{
module LexerGrammar where
}

%wrapper "basic"

$A          = [aA]
$N          = [nN]
$D          = [dD]

$lowletter  = a-z
$highletter = A-Z
$digit      = 0-9
$alpha      = [a-zA-Z]

tokens :-
    $white+                                     ;
    "{".*"}"                                    ;
    $A $N $D                                    { \_ -> TokenAnd }

{
data GrammarToken = TokenAnd
    deriving (Eq, Show)
}
     