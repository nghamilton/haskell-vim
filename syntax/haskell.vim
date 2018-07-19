" syntax highlighting for haskell
"
" Heavily modified version of the haskell syntax
" highlighter to support haskell.
"
" author: raichoo (raichoo@googlemail.com)

if version < 600
  syn clear
elseif exists("b:current_syntax")
  finish
endif

if get(g:, 'haskell_backpack', 0)
  syn keyword haskellBackpackStructure unit signature
  syn keyword haskellBackpackDependency dependency
endif

syn spell notoplevel
syn match haskellRecordField contained containedin=haskellBlock
  \ "[_a-z][a-zA-Z0-9_']*\(,\s*[_a-z][a-zA-Z0-9_']*\)*\_s\+::\_s"
  \ contains=
  \ haskellIdentifier,
  \ haskellOperators,
  \ haskellSeparator,
  \ haskellParens
syn match haskellTypeSig
  \ "^\s*\(where\s\+\|let\s\+\|default\s\+\)\?[_a-z][a-zA-Z0-9_']*#\?\(,\s*[_a-z][a-zA-Z0-9_']*#\?\)*\_s\+::\_s"
  \ contains=
  \ haskellWhere,
  \ haskellLet,
  \ haskellDefault,
  \ haskellIdentifier,
  \ haskellOperators,
  \ haskellSeparator,
  \ haskellParens
syn keyword haskellWhere where
syn keyword haskellLet let
syn match HaskellDerive "\<deriving\>\(\s\+\<\(anyclass\|instance\|newtype\|stock\)\>\)\?"
syn keyword haskellDeclKeyword module class instance newtype in
syn match haskellDecl "\<\(type\|data\)\>\s\+\(\<family\>\)\?"
syn keyword haskellDefault default
syn keyword haskellImportKeywords import qualified safe as hiding contained
syn keyword haskellForeignKeywords foreign export import ccall safe unsafe interruptible capi prim contained
syn region haskellForeignImport start="\<foreign\>" end="\_s\+::\s" keepend
  \ contains=
  \ haskellString,
  \ haskellOperators,
  \ haskellForeignKeywords,
  \ haskellIdentifier
syn match haskellImport "^\s*\<import\>\s\+\(\<safe\>\s\+\)\?\(\<qualified\>\s\+\)\?.\+\(\s\+\<as\>\s\+.\+\)\?\(\s\+\<hiding\>\)\?"
  \ contains=
  \ haskellParens,
  \ haskellOperators,
  \ haskellImportKeywords,
  \ haskellType,
  \ haskellLineComment,
  \ haskellBlockComment,
  \ haskellString,
  \ haskellPragma
syn keyword haskellKeyword do case of
if get(g:, 'haskell_enable_static_pointers', 0)
  syn keyword haskellStatic static
endif
syn keyword haskellConditional if then else
syn match haskellNumber "\<[0-9]\+\>\|\<0[xX][0-9a-fA-F]\+\>\|\<0[oO][0-7]\+\>\|\<0[bB][10]\+\>"
syn match haskellFloat "\<[0-9]\+\.[0-9]\+\([eE][-+]\=[0-9]\+\)\=\>"
syn match haskellSeparator  "[,;]"
syn region haskellParens matchgroup=haskellDelimiter start="(" end=")" contains=TOP,haskellTypeSig,@Spell
syn region haskellBrackets matchgroup=haskellDelimiter start="\[" end="]" contains=TOP,haskellTypeSig,@Spell
syn region haskellBlock matchgroup=haskellDelimiter start="{" end="}" contains=TOP,@Spell
syn keyword haskellInfix infix infixl infixr
syn keyword haskellBottom undefined error
syn match haskellOperators "[-!#$%&\*\+/<=>\?@\\^|~:.]\+\|\<_\>"
syn match haskellQuote "\<'\+" contained
syn match haskellQuotedType "[A-Z][a-zA-Z0-9_']*\>" contained
syn region haskellQuoted start="\<'\+" end="\>"
  \ contains=
  \ haskellType,
  \ haskellQuote,
  \ haskellQuotedType,
  \ haskellSeparator,
  \ haskellParens,
  \ haskellOperators,
  \ haskellIdentifier
syn match haskellLineComment "---*\([^-!#$%&\*\+./<=>\?@\\^|~].*\)\?$"
  \ contains=
  \ haskellTodo,
  \ @Spell
syn match haskellBacktick "`[A-Za-z_][A-Za-z0-9_\.']*#\?`"
syn region haskellString start=+"+ skip=+\\\\\|\\"+ end=+"+
  \ contains=@Spell
syn match haskellIdentifier "[_a-z][a-zA-z0-9_']*" contained
syn match haskellChar "\<'[^'\\]'\|'\\.'\|'\\u[0-9a-fA-F]\{4}'\>"
syn match haskellType "\<[A-Z][a-zA-Z0-9_']*\>"
syn region haskellBlockComment start="{-" end="-}"
  \ contains=
  \ haskellBlockComment,
  \ haskellTodo,
  \ @Spell
syn region haskellPragma start="{-#" end="#-}"
syn region haskellLiquid start="{-@" end="@-}"
syn match haskellPreProc "^#.*$"
syn keyword haskellTodo TODO FIXME contained
" Treat a shebang line at the start of the file as a comment
syn match haskellShebang "\%^#!.*$"
if !get(g:, 'haskell_disable_TH', 0)
    syn match haskellQuasiQuoted "." containedin=haskellQuasiQuote contained
    syn region haskellQuasiQuote matchgroup=haskellTH start="\[[_a-zA-Z][a-zA-z0-9._']*|" end="|\]"
    syn region haskellTHBlock matchgroup=haskellTH start="\[\(d\|t\|p\)\?|" end="|]" contains=TOP
    syn region haskellTHDoubleBlock matchgroup=haskellTH start="\[||" end="||]" contains=TOP
endif
if get(g:, 'haskell_enable_typeroles', 0)
  syn keyword haskellTypeRoles phantom representational nominal contained
  syn region haskellTypeRoleBlock matchgroup=haskellTypeRoles start="type\s\+role" end="$" keepend
    \ contains=
    \ haskellType,
    \ haskellTypeRoles
endif
if get(g:, 'haskell_enable_quantification', 0)
  syn keyword haskellForall forall
endif
if get(g:, 'haskell_enable_recursivedo', 0)
  syn keyword haskellRecursiveDo mdo rec
endif
if get(g:, 'haskell_enable_arrowsyntax', 0)
  syn keyword haskellArrowSyntax proc
endif
if get(g:, 'haskell_enable_pattern_synonyms', 0)
  syn keyword haskellPatternKeyword pattern
endif

" Shameless re-ripped from wlangstroth/vim-haskell
" C Preprocessor directives. Shamelessly ripped from c.vim and trimmed
" First, see whether to flag directive-like lines or not
if (!exists("hs_allow_hash_operator"))
  syn match   haskellError      display "^\s*\(%:\|#\).*$"
endif
" Accept %: for # (C99)
syn region  haskellPreCondit  start="^\s*\(%:\|#\)\s*\(if\|ifdef\|ifndef\|elif\)\>" skip="\\$" end="$" end="//"me=s-1 contains=haskellComment,haskellCppString,haskellCommentError
syn match   haskellPreCondit  display "^\s*\(%:\|#\)\s*\(else\|endif\)\>"
syn region  haskellCppOut     start="^\s*\(%:\|#\)\s*if\s\+0\+\>" end=".\@=\|$" contains=haskellCppOut2
syn region  haskellCppOut2    contained start="0" end="^\s*\(%:\|#\)\s*\(endif\>\|else\>\|elif\>\)" contains=haskellCppSkip
syn region  haskellCppSkip    contained start="^\s*\(%:\|#\)\s*\(if\>\|ifdef\>\|ifndef\>\)" skip="\\$" end="^\s*\(%:\|#\)\s*endif\>" contains=haskellCppSkip
syn region  haskellIncluded   display contained start=+"+ skip=+\\\\\|\\"+ end=+"+
syn match   haskellIncluded   display contained "<[^>]*>"
syn match   haskellInclude    display "^\s*\(%:\|#\)\s*include\>\s*["<]" contains=haskellIncluded
syn cluster haskellPreProcGroup   contains=haskellPreCondit,haskellIncluded,haskellInclude,haskellDefine,haskellCppOut,haskellCppOut2,haskellCppSkip,haskellCommentStartError
syn region  haskellDefine     matchgroup=haskellPreCondit start="^\s*\(%:\|#\)\s*\(define\|undef\)\>" skip="\\$" end="$"
syn region  haskellPreProc    matchgroup=haskellPreCondit start="^\s*\(%:\|#\)\s*\(pragma\>\|line\>\|warning\>\|warn\>\|error\>\)" skip="\\$" end="$" keepend

syn region  haskellComment    matchgroup=haskellCommentStart start="/\*" end="\*/" contains=haskellCommentStartError,cSpaceError contained
syn match   haskellCommentError   display "\*/" contained
syn match   haskellCommentStartError display "/\*"me=e-1 contained
syn region  haskellCppString  start=+L\="+ skip=+\\\\\|\\"\|\\$+ excludenl end=+"+ end='$' contains=cSpecial contained

syn match hsHlFunctionName "[a-z_]\(\S\&[^,\(\)\[\]]\)*" contained
syn match hsFunctionName "^[a-z_]\(\S\&[^,\(\)\[\]]\)*" contained contains=hsHlFunctionName
syn match hsHlInfixFunctionName "`[a-z_][^`]*`" contained
syn match hsInfixFunctionName "^\S[^=]*`[a-z_][^`]*`"me=e-1 contained contains=hsHlInfixFunctionName,hsType,hsConSym,hsVarSym,hsString,hsCharacter
syn match hsHlInfixOp "\(\W\&\S\&[^`(){}'[\]]\)\+" contained contains=hsString
syn match hsInfixOpFunctionName "^\(\(\w\|[[\]{}]\)\+\|\(\".*\"\)\|\('.*'\)\)\s*[^:]=*\(\W\&\S\&[^='\"`()[\]{}@]\)\+"
      \ contained contains=hsHlInfixOp,hsCharacter
syn match hsOpFunctionName        "(\(\W\&[^(),\"]\)\+)" contained
"syn region hsFunction start="^["'a-z_([{]" end="=\(\s\|\n\|\w\|[([]\)" keepend extend
syn region hsFunction start="^["'a-zA-Z_([{]\(\(.\&[^=]\)\|\(\n\s\)\)*=" end="\(\s\|\n\|\w\|[([]\)"
      \ contains=hsOpFunctionName,hsInfixOpFunctionName,hsInfixFunctionName,hsFunctionName,hsType,hsConSym,hsVarSym,hsString,hsCharacter
syn match hsTypeOp "::"
syn match hsDeclareFunction "^[a-z_(]\S*\(\s\|\n\)*::" contains=hsFunctionName,hsOpFunctionName,hsTypeOp


highlight def link haskellCppString String
highlight def link haskellCommentStart Comment
highlight def link haskellCommentError Error
highlight def link haskellCommentStartError Error
highlight def link haskellError Error
highlight def link haskellInclude Include
highlight def link haskellPreProc PreProc
highlight def link haskellIncluded String
highlight def link haskellPreCondit PreCondit
highlight def link haskellComment Comment
highlight def link haskellCppSkip haskellCppOut
highlight def link haskellCppOut2 haskellCppOut
highlight def link haskellCppOut Comment
highlight def link hsFunction Function
highlight def link hsDeclareFunction Function
highlight def link hsHlFunctionName Function
highlight def link hsHlInfixFunctionName Function
highlight def link hsHlInfixOp Function
highlight def link hsOpFunctionName Function

highlight def link haskellBottom Macro
highlight def link haskellTH Boolean
highlight def link haskellIdentifier Identifier
highlight def link haskellForeignKeywords Structure
highlight def link haskellKeyword Keyword
highlight def link haskellDefault Keyword
highlight def link haskellConditional Conditional
highlight def link haskellNumber Number
highlight def link haskellFloat Float
highlight def link haskellSeparator Delimiter
highlight def link haskellDelimiter Delimiter
highlight def link haskellInfix Keyword
highlight def link haskellOperators Operator
highlight def link haskellQuote Operator
highlight def link haskellShebang Comment
highlight def link haskellLineComment Comment
highlight def link haskellBlockComment Comment
highlight def link haskellPragma SpecialComment
highlight def link haskellLiquid SpecialComment
highlight def link haskellString String
highlight def link haskellChar String
highlight def link haskellBacktick Operator
highlight def link haskellQuasiQuoted String
highlight def link haskellTodo Todo
highlight def link haskellPreProc PreProc
highlight def link haskellAssocType Type
highlight def link haskellQuotedType Type
highlight def link haskellType Type
highlight def link haskellImportKeywords Include
if get(g:, 'haskell_classic_highlighting', 0)
  highlight def link haskellDeclKeyword Keyword
  highlight def link HaskellDerive Keyword
  highlight def link haskellDecl Keyword
  highlight def link haskellWhere Keyword
  highlight def link haskellLet Keyword
else
  highlight def link haskellDeclKeyword Structure
  highlight def link HaskellDerive Structure
  highlight def link haskellDecl Structure
  highlight def link haskellWhere Structure
  highlight def link haskellLet Structure
endif

if get(g:, 'haskell_enable_quantification', 0)
  highlight def link haskellForall Operator
endif
if get(g:, 'haskell_enable_recursivedo', 0)
  highlight def link haskellRecursiveDo Keyword
endif
if get(g:, 'haskell_enable_arrowsyntax', 0)
  highlight def link haskellArrowSyntax Keyword
endif
if get(g:, 'haskell_enable_static_pointers', 0)
  highlight def link haskellStatic Keyword
endif
if get(g:, 'haskell_classic_highlighting', 0)
  if get(g:, 'haskell_enable_pattern_synonyms', 0)
    highlight def link haskellPatternKeyword Keyword
  endif
  if get(g:, 'haskell_enable_typeroles', 0)
    highlight def link haskellTypeRoles Keyword
  endif
else
  if get(g:, 'haskell_enable_pattern_synonyms', 0)
    highlight def link haskellPatternKeyword Structure
  endif
  if get(g:, 'haskell_enable_typeroles', 0)
    highlight def link haskellTypeRoles Structure
  endif
endif

if get(g:, 'haskell_backpack', 0)
  highlight def link haskellBackpackStructure Structure
  highlight def link haskellBackpackDependency Include
endif
let b:current_syntax = "haskell"
