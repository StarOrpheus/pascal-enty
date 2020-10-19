# pascal-enty
Simplified pascal interpreter and pretty printer, written in Haskell. Try example:

```stack run pprint assets/ComplexExample.pas```


## Usage
`stack run RUN_FORMAT ARGS...`

Available run formats:
* **dump** - dumps tokens and *AST*
* **pprint** - prints formatted source code
    * Comments are skipped on the lexer phaze
    * Superfluous semicolons are omitted
