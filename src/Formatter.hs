module Formatter ( pascalFormat
                 ) where

import Grammar

import Data.List

class Renderable a where
    render :: a -> [String]

class InplaceRenderable a where
    inplaceRender :: a -> String

instance InplaceRenderable RelationalOperator where
    inplaceRender obj = case obj of
        OperatorLT -> "<"
        OperatorLE -> "<="
        OperatorGT -> ">"
        OperatorGE -> ">="
        OperatorEQ -> "="
        OperatorNE -> "<>"

instance InplaceRenderable FactorSignum where
    inplaceRender obj = case obj of
        SignPLUS -> "+"
        SignMINUS -> "-"

instance InplaceRenderable AdditiveOperator where
    inplaceRender obj = case obj of
        OperatorPLUS -> "+"
        OperatorMINUS -> "-"
        OperatorOR -> "or"

instance InplaceRenderable MultiplicativeOperator where
    inplaceRender obj = case obj of
        OperatorSTAR -> "*"
        OperatorSLASH -> "/"
        OperatorMOD -> "mod"
        OperatorDIV -> "div"
        OperatorAND -> "and"

instance InplaceRenderable Valueble where
    inplaceRender obj = case obj of
        ValuebleInteger a -> show a
        ValuebleReal    a -> show a
        ValuebleString  a -> a
        ValuebleBool    a -> if a then "true" else "false"
        ValuebleChar    a -> show a

instance InplaceRenderable PascalType where
    inplaceRender obj = case obj of
        PascalSubrangeType l r -> show l ++ ".." ++ show r
        PascalArrayType indexT componentT -> "array[" ++ inplaceRender indexT ++ "] of " ++ inplaceRender componentT
        PascalIdentType ident -> inplaceRender ident

instance InplaceRenderable PascalTypeIdentifier where
    inplaceRender obj = case obj of
        PascalInteger -> "integer"
        PascalReal -> "real"
        PascalString -> "string"
        PascalBool -> "bool"
        PascalChar -> "char"

instance InplaceRenderable PASTExpression where
    inplaceRender (PASTExpression simple relPart) = case relPart of
        Nothing -> inplaceRender simple
        Just (op, expr) -> "(" ++ inplaceRender simple ++ " " ++ inplaceRender op ++ " " ++ inplaceRender expr ++ ")"

instance InplaceRenderable PASTSimpleExpession where
    inplaceRender (PASTSimpleExpession simple addPart) = case addPart of
        Nothing -> inplaceRender simple
        Just (op, expr) -> "(" ++ inplaceRender simple ++ " " ++ inplaceRender op ++ " " ++ inplaceRender expr ++ ")"

instance InplaceRenderable PASTTerm where
    inplaceRender (PASTTerm simple addPart) = case addPart of
        Nothing -> inplaceRender simple
        Just (op, expr) -> "(" ++ inplaceRender simple ++ " " ++ inplaceRender op ++ " " ++ inplaceRender expr ++ ")"

instance InplaceRenderable PASTSignedFactor where
    inplaceRender (PASTSignedFactor signum term) = case signum of
        Nothing -> inplaceRender term
        Just op -> "(" ++ inplaceRender op ++ inplaceRender term ++ ")"

joinRenderables :: InplaceRenderable a => [a] -> String -> String
joinRenderables list separator =
    let foldFunc acc newVal =
            let prefix = if null acc then "" else acc ++ separator in
                prefix ++ inplaceRender newVal in
        foldl' foldFunc "" list

instance InplaceRenderable PASTFactor where
    inplaceRender obj = case obj of
        PASTFactorVariable var -> inplaceRender var
        PASTFactorCompound expr -> "(" ++ inplaceRender expr ++ ")"
        PASTFunctionDisignator funcName args -> funcName ++ "(" ++ joinRenderables args ", " ++ ")"
        PASTUnsignedConstant val -> inplaceRender val
        PASTFactorNot factor -> "not " ++ inplaceRender factor

instance InplaceRenderable PASTVariable where
    inplaceRender (PASTVariable varName inds) =
        if null inds then varName else varName ++ "[" ++ joinRenderables inds ", " ++ "]"

instance InplaceRenderable PASTForRange where
    inplaceRender obj = case obj of
        PASTForTo from to -> inplaceRender from ++ " to " ++ inplaceRender to
        PASTForDownto from to -> inplaceRender from ++ " downto " ++ inplaceRender to

instance InplaceRenderable PASTDeclConst where
    inplaceRender (PASTDeclConst name value) = name ++ " = " ++ inplaceRender value

instance InplaceRenderable PASTDeclVar where
    inplaceRender (PASTDeclVar name t) = name ++ ": " ++ inplaceRender t

indent :: [String] -> [String]
indent = foldl' (\ acc newVal -> acc ++ ["    " ++ newVal]) []

instance Renderable PASTProgramHeading where
    render (PASTProgramHeading []) = []
    render (PASTProgramHeading (a : as)) =
        case as of
            [] -> ["program " ++ a ++ ";"]
            [x] -> ["program " ++ a ++ "(" ++ x ++ ");"]
            _ -> error "Unexpected program heading: Too many identifiers"
        ++ [""]

renderStatementSeq :: [PASTStatement] -> [String]
renderStatementSeq [] = []
renderStatementSeq [a] = render a
renderStatementSeq (a : as) = undefined -- TODO

instance Renderable PASTStatement where
    render (PASTCompoundStatement ss) =
        ["begin"] ++ indent (renderStatementSeq ss) ++ ["end"]
    render (PASTAssignStatement var expr) =
        [inplaceRender var ++ " := " ++ inplaceRender expr]

pascalFormat :: PASTProgram -> [String]
pascalFormat program = undefined