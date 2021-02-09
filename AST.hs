module AST where

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
