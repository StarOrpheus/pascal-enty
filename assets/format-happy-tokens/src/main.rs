fn main() {
    let tokens =
        "TokenAND | TokenARRAY | TokenBEGIN | TokenBOOLEAN
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
        | TokenReal Double";

    for tokenDescription in tokens.split('|') {
        let tokenDescription = tokenDescription.trim();
        if tokenDescription.is_empty() {
            continue;
        }

        let tokenName = tokenDescription.split(' ').take(1).last().unwrap();
        assert!(tokenName.starts_with("Token"));
        let newTokenName = &tokenName[5..];

        print!("%token {}", newTokenName);
        print!("{}", " ".repeat(48 - (newTokenName.len() + 7)));
        if (tokenDescription.len() > tokenName.len()) {
            println!("{{ {} $$ }}", tokenName);
        } else {
            println!("{{ {} }}", tokenName);
        }
    }
}
