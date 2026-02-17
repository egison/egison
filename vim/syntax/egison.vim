" Vim syntax file
" Language:    Egison
" Maintainer:  Satoshi Egi <egisatoshi@gmail.com>
" URL:         https://github.com/egison/egison
" Version:     0.1.0
" License:     MIT

if exists("b:current_syntax")
  finish
endif

" ============================================================
" Comments
" ============================================================
syn match egisonLineComment "--.*$" contains=@Spell
syn region egisonBlockComment start="{-" end="-}" contains=egisonBlockComment,@Spell

" ============================================================
" Strings and characters
" ============================================================
syn region egisonString start=+"+ skip=+\\\\\|\\"+ end=+"+ contains=egisonStringEscape
syn region egisonChar start=+'+ skip=+\\\\\|\\'+ end=+'+  contains=egisonStringEscape
syn match egisonStringEscape "\\." contained

" ============================================================
" Numbers
" ============================================================
syn match egisonFloat "\<[0-9]\+\.[0-9]\+\([eE][+-]\?[0-9]\+\)\?\>"
syn match egisonInteger "\<[0-9]\+\>"

" ============================================================
" Type system keywords
" ============================================================
syn keyword egisonTypeKeyword class instance inductive extends declare

" ============================================================
" Definition keywords
" ============================================================
syn keyword egisonDefKeyword def let in where

" ============================================================
" Control flow
" ============================================================
syn keyword egisonControlKeyword if then else

" ============================================================
" Pattern matching
" ============================================================
syn keyword egisonMatchKeyword match matchDFS matchAll matchAllDFS as with loop forall

" ============================================================
" Matcher definition
" ============================================================
syn keyword egisonMatcherKeyword matcher algebraicDataMatcher

" ============================================================
" Lambda and special forms
" ============================================================
syn keyword egisonLambdaKeyword memoizedLambda cambda capply function

" ============================================================
" Tensor operations
" ============================================================
syn keyword egisonTensorKeyword tensor generateTensor contract tensorMap tensorMap2
syn keyword egisonTensorKeyword transpose flipIndices
syn keyword egisonTensorKeyword subrefs suprefs userRefs

" ============================================================
" IO and sequencing
" ============================================================
syn keyword egisonIOKeyword do seq

" ============================================================
" Module and loading
" ============================================================
syn keyword egisonModuleKeyword load loadFile execute

" ============================================================
" Infix declarations
" ============================================================
syn keyword egisonInfixKeyword infixr infixl infix expression pattern

" ============================================================
" Symbols and special forms
" ============================================================
syn keyword egisonSymbolKeyword withSymbols

" ============================================================
" Special values
" ============================================================
syn keyword egisonSpecialValue undefined something

" ============================================================
" Built-in type names
" ============================================================
syn keyword egisonBuiltinType Integer MathExpr Float Bool Char String
syn keyword egisonBuiltinType IO Matcher Pattern
syn keyword egisonBuiltinType Tensor Vector Matrix DiffForm List

" ============================================================
" Boolean literals
" ============================================================
syn keyword egisonBoolean True False

" ============================================================
" Testing primitives
" ============================================================
syn keyword egisonTestKeyword assert assertEqual

" ============================================================
" Pattern variables: $x, $pat, $
" ============================================================
syn match egisonPatternVar "\$[a-zA-Z_][a-zA-Z0-9_']*"
syn match egisonWildcard "\$\ze[^a-zA-Z0-9_(]"

" ============================================================
" Value patterns: #x, #(expr)
" ============================================================
syn match egisonValuePattern "#[a-zA-Z_][a-zA-Z0-9_']*"
syn match egisonValuePatternParen "#\ze("

" ============================================================
" Type class constraints: {Eq a}, {Eq a, Ord b}
" ============================================================
syn region egisonTypeConstraint start="{[A-Z]" end="}" contains=egisonBuiltinType,egisonUserType

" ============================================================
" User-defined types (uppercase identifiers)
" ============================================================
syn match egisonUserType "\<[A-Z][a-zA-Z0-9_]*\>"

" ============================================================
" Function name after def
" ============================================================
syn match egisonFuncDef "\<def\s\+\zs[a-zA-Z_][a-zA-Z0-9_']*"

" ============================================================
" Type/class name after class/instance/inductive
" ============================================================
syn match egisonTypeDef "\<\(class\|instance\|inductive\)\s\+\zs[A-Z][a-zA-Z0-9_]*"

" ============================================================
" Tensor index notation: v~i, T_j, T~i_j
" ============================================================
syn match egisonTensorIndex "[a-zA-Z0-9_']\zs[_~][a-zA-Z0-9_#]\+"

" ============================================================
" Operators
" ============================================================
syn match egisonOperator ":="
syn match egisonOperator "::"
syn match egisonOperator "++"
syn match egisonOperator "=>"
syn match egisonOperator "->"
syn match egisonOperator "\.\.\."
syn match egisonOperator "&&"
syn match egisonOperator "||"
syn match egisonOperator "=="
syn match egisonOperator "/="
syn match egisonOperator "<="
syn match egisonOperator ">="
syn match egisonOperator "∧"

" ============================================================
" Mathematical symbols (operators only, not variables)
" ============================================================
syn match egisonMathSymbol "[∂∇∫∑∏√±×÷≠≤≥∞∈∉∀∃∧∨¬⊕⊗⊥⊤⊆⊇⊂⊃∪∩]"

" ============================================================
" Highlight groups
" ============================================================
hi def link egisonLineComment      Comment
hi def link egisonBlockComment     Comment
hi def link egisonString           String
hi def link egisonChar             String
hi def link egisonStringEscape     SpecialChar
hi def link egisonFloat            Number
hi def link egisonInteger          Number

hi def link egisonTypeKeyword      Keyword
hi def link egisonDefKeyword       Keyword
hi def link egisonControlKeyword   Conditional
hi def link egisonMatchKeyword     Keyword
hi def link egisonMatcherKeyword   Keyword
hi def link egisonLambdaKeyword    Keyword
hi def link egisonTensorKeyword    Function
hi def link egisonIOKeyword        Keyword
hi def link egisonModuleKeyword    Include
hi def link egisonInfixKeyword     Keyword
hi def link egisonSymbolKeyword    Keyword
hi def link egisonSpecialValue     Constant
hi def link egisonTestKeyword      Debug

hi def link egisonBuiltinType      Type
hi def link egisonUserType         Type
hi def link egisonBoolean          Boolean

hi def link egisonPatternVar       Identifier
hi def link egisonWildcard         Identifier
hi def link egisonValuePattern     Constant
hi def link egisonValuePatternParen Constant

hi def link egisonTypeConstraint   Type

hi def link egisonFuncDef          Function
hi def link egisonTypeDef          Type

hi def link egisonTensorIndex      Special

hi def link egisonOperator         Operator
hi def link egisonMathSymbol       Special

let b:current_syntax = "egison"
