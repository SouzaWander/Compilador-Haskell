{
module Parser where
import Lexer
}

%name parser
%tokentype { Token }
%error  { parseError }

%nonassoc "==" "!=" '<' '>' "<=" ">="

%left '+' '-'
%left '*' '/'
%left '%'


%token
	num		 				{ NUM $$ }
	id		    			{ ID $$ }
	'+'		 				{ PLUS }
	'-'		 				{ MINUS }
	'*'		 				{ MULT }
	'/'		 				{ DIV }
	'%'		 				{ REST }
	'('		 				{ LPAREN }
	')'		 				{ RPAREN }
	'{'		 				{ LCHAV }
	'}'      				{ RCHAV }
	"=="     				{ EQUAL_EQUAL }
	"!="     				{ DIFF }
	'<'	     				{ LESS }
	"<="     				{ L_EQUAL }
	'>'	    				{ GREATER }
	">=" 					{ G_EQUAL }
	'='		 				{ EQUAL }
	';'		 				{ DOTCOMMA }
	','						{ COMMA }
	"++"					{ INCR }
	"--"					{ DECR }
	if 	     				{ IF }
	else     				{ ELSE }
	true					{ TRUE }
	false 					{ FALSE }
	while    				{ WHILE }
	for						{ FOR }
	int 	 				{ INT }
	bool					{ BOOL }
	return   				{ RETURN }
	scan_int  				{ SCAN_INT }
	print_int 				{ PRINT_INT }


%%
Prog : Func_Dec				{ [$1] }
| Func_Dec  Prog { $1 : $2 }

Func_Dec : Type Atom '(' Args ')'	'{' Blo_Dec Blo_Inst '}'   { Declaration $1 $2 $4 $7 $8 }

Args : {- empty -}	{ [] }
		| Simple 				{ [$1] }
		| Simple ',' Args { $1 : $3 }

Blo_Dec : {- empty -}	{ [] }
 		| Simple ';' Blo_Dec{ $1 : $3 }

Inst : if '(' Exp ')' Inst 																						{ If $3 $5 }
	 | if '(' Exp ')' Inst else Inst 																		{ If_Else $3 $5 $7 }
	 | while '(' Exp ')' '{' Blo_Inst '}'																{ While $3 $6 }
	 | for '(' Simple ';' Exp ';' Simple ')' '{' Blo_Inst '}'						{ For $3 $5 $7 $10 }
	 | return Exp ';'																										{ Return $2 }
	 | return ';'																												{ Return_Emp }
	 | print_int '(' Exp ')' ';' 																				{ Print_Int $3 }
	 | Atom '=' Exp ';'									{ AssignExp $1 $3 }
	 | Atom "++"										{ Incr $1 }
	 | Atom "--"										{ Decr $1 }
	 | '{' Blo_Inst '}'							{ Bloco $2}


Blo_Inst :  {- empty -}	{ [] }
 		| Inst Blo_Inst { $1 : $2 }


Simple : Type Atom 										{ AssignType $1 $2 }
	   | Type Atom '=' Exp 								{ AssignTExp $1 $2 $4 }

Exp : '(' Exp ')' 										{ $2 }
		| ArExp											{ Arit $1 }
 		| BoolExp										{ BoolE $1 }
 		| num											{ Num $1 }
		| Atom											{ Id_Exp $1 }
		| Func_C										{ Function $1}
		| true 											{ BTrue }
		| false 										{ BFalse }
		| scan_int '(' ')' 						{ Scan_Int }

BoolExp : Exp "==" Exp 									{ Equal_Equal $1 $3 }
		| Exp "!=" Exp									{ Diff $1 $3 }
		| Exp '<' Exp									{ Less $1 $3 }
		| Exp "<=" Exp									{ L_Equal $1 $3 }
		| Exp '>' Exp									{ Greater $1 $3 }
		| Exp ">=" Exp									{ G_Equal $1 $3 }

ArExp : Exp '+' Exp				{ Plus $1 $3 }
	  | Exp '-' Exp					{ Minus $1 $3 }
	  | Exp '*' Exp					{ Mult $1 $3 }
	  | Exp '/' Exp					{ Div $1 $3 }
	  | Exp '%' Exp					{ Rest $1 $3 }




Func_C : Atom '(' Exps ')'  						{ Function_Call $1 $3 }

Exps : {- empty -}	{ [] }
	| Exp 				{ [$1] }
	| Exp ',' Exps { $1 : $3 }


Atom : id									{ Id $1 }

Type : int  								{ TyInt }
	 | bool									{ TyBool }


{
data Func_Dec = Declaration Type Atom Args Blo_Dec Blo_Inst
			 deriving (Eq, Show)

data Inst = While Exp	Blo_Inst
		  | If Exp Inst
		  | If_Else Exp Inst Inst
		  | For Simple Exp Simple Blo_Inst
		  | Return Exp
		  | Return_Emp
			| Print_Int Exp
			| AssignExp Atom Exp
			| Incr Atom
			| Decr Atom
			| Bloco Blo_Inst
		 	deriving (Eq, Show)


data Simple = AssignType Type Atom
			| AssignTExp Type Atom Exp
		  deriving (Eq, Show)


data Exp = Num Int
   			| Id_Exp	Atom
				| Arit ArExp
				| BoolE BoolExp
				| Function Func_C
				| BTrue
				| BFalse
				| Scan_Int
				deriving (Eq, Show)

data ArExp = Plus Exp Exp
          | Minus Exp Exp
          | Mult Exp Exp
          | Div Exp Exp
		   		| Rest Exp Exp
		  		deriving (Eq, Show)

data BoolExp = Equal_Equal Exp Exp
			 			| Less Exp Exp
            | L_Equal Exp Exp
			 			| Greater Exp Exp
      			| G_Equal Exp Exp
			 			| Diff Exp Exp
						deriving (Eq, Show)


data Func_C = Function_Call Atom Exps
					deriving (Eq, Show)

data Atom = Id String
		 deriving (Eq, Show, Ord)

data Type = TyInt
				| TyBool
				| TyFun Type [Type]
		 		deriving (Eq, Show)

type Args = [Simple]

type Prog = [Func_Dec]

type Blo_Inst = [Inst]

type Blo_Dec = [Simple]

type Exps = [Exp]

parseError :: [Token] -> a
parseError toks = error "parse error"
}
