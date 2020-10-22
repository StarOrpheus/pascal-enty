# pascal-enty
Simplified pascal interpreter and pretty printer, written in Haskell. Try example:

`stack run pprint assets/ComplexExample.pas`

`stack run run assets/HelloWorld.pas`

`stack run run assets/ComplexSample.pas` and input array len and the array itself (single number on each line)


## Usage
`stack run RUN_FORMAT ARGS...`

Available run formats:
* **dump** - dumps tokens and *AST*
* **pprint** - prints formatted source code
    * Comments are skipped on the lexer phaze
    * Superfluous semicolons are omitted
* **run** - simply runs program
