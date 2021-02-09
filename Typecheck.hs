module Typecheck where

import           Parser

import           Data.Map(Map)
import qualified Data.Map as Map

-- ambiente de tipos (tabela de símbolos)
type Id = String
type TypeEnv = Map Id Type

----------------------------------------------------------------------------------
-- Expressions

checkAtom :: TypeEnv -> Atom -> Type
checkAtom env (Id x) = case Map.lookup x env of
    Nothing -> error "undeclared var"
    Just t -> t


checkExp :: TypeEnv -> Exp -> Type
checkExp env (Num n) = TyInt
checkExp env (BTrue) = TyBool
checkExp env (BFalse) = TyBool
checkExp env (Id_Exp x) = checkAtom env x
checkExp env (Arit expr) = checkArExp env expr
checkExp env (BoolE expr) = checkBoolExp env expr
checkExp env (Scan_Int) = TyInt

-- nao sei se isto pinta para expressoes com mais que 2 e
checkArExp :: TypeEnv -> ArExp -> Type
checkArExp env (Plus e1 e2)
  = let t1 = checkExp env e1
        t2 = checkExp env e2
    in if t1==TyInt && t2==TyInt then TyInt
       else error "type error in +"

checkArExp env (Minus e1 e2)
  = let t1 = checkExp env e1
        t2 = checkExp env e2
    in if t1==TyInt && t2==TyInt then TyInt
       else error "type error in -"

checkArExp env (Mult e1 e2)
  = let t1 = checkExp env e1
        t2 = checkExp env e2
    in if t1==TyInt && t2==TyInt then TyInt
       else error "type error in /"

checkArExp env (Rest e1 e2)
  = let t1 = checkExp env e1
        t2 = checkExp env e2
    in if t1==TyInt && t2==TyInt then TyInt
       else error "type error in %"


checkBoolExp :: TypeEnv -> BoolExp -> Type
checkBoolExp env (Less e1 e2)
  = let t1 = checkExp env e1
        t2 = checkExp env e2
    in if t1==TyInt && t2==TyInt then TyBool
       else error "type error in <"

checkBoolExp env (L_Equal e1 e2)
  = let t1 = checkExp env e1
        t2 = checkExp env e2
    in if t1==TyInt && t2==TyInt then TyBool
       else error "type error in <="

checkBoolExp env (Greater e1 e2)
   = let t1 = checkExp env e1
         t2 = checkExp env e2
     in if t1==TyInt && t2==TyInt then TyBool
        else error "type error in >"

checkBoolExp env (G_Equal e1 e2)
    = let t1 = checkExp env e1
          t2 = checkExp env e2
      in if t1==TyInt && t2==TyInt then TyBool
         else error "type error in >="

checkBoolExp env (Diff e1 e2)
   = let t1 = checkExp env e1
         t2 = checkExp env e2
     in if t1==TyInt && t2==TyInt then TyBool
        else error "type error in !="

checkBoolExp env (Equal_Equal e1 e2)
    = let t1 = checkExp env e1
          t2 = checkExp env e2
      in if t1==TyInt && t2==TyInt then TyBool
         else error "type error in =="


--- intructions
checkInst :: TypeEnv -> Inst -> Bool
checkInst env (If cond inst)
  = let t0 = checkExp env cond
    in if t0 == TyBool then checkInst env inst
      else error "type error: condition must be bool"

checkInst env (If_Else cond inst1 inst2)
  = let t0 = checkExp env cond
    in if t0 == TyBool then checkInst env inst1 &&  checkInst env inst2
       else error "type error: condition must be bool"

checkInst env (Bloco bloInst)
  = checkAll env bloInst

checkInst env (Return expr)
  = let t0 = checkExp env expr
    in if t0 == t0 then True
       else error "type error"

checkInst env (Print_Int expr)
  = let t0 = checkExp env expr
    in if t0 == t0 then True
       else error "type error"

checkInst env (While cond bloInst)
  = let t0 = checkExp env cond
    in if t0 == TyBool then checkAll env bloInst
       else error "type error: condition must be bool"



checkInst env (AssignExp (Id var) expr)
  = let t0 = checkAtom env (Id var)
        t1 = checkExp env expr
    in if t0 == t1 then True
       else error "assign error"

----declarações/simpleAssignExp
checkDec :: TypeEnv -> Simple -> TypeEnv
checkDec env (AssignType ty (Id var))
  = Map.insert var ty env

checkDec env (AssignTExp ty (Id var) expr)
  = let env' = Map.insert var ty env
        t0 = checkExp env' expr
        t1 = checkAtom env' (Id var)
    in if t0 == t1 then env'
       else error "assign error"

--- vai retornar o env com a declaração da função para podermos chamarmos as funções em outras funçoes
checkFunc :: TypeEnv -> Func_Dec -> TypeEnv
checkFunc env (Declaration ty (Id var) args blo_dec blo_inst)
    = let env' = Map.insert var (TyFun ty (argTypes args)) env
          env'' =  checkBlocDec env' blo_dec
          temp = checkAll env'' blo_inst
          in if temp == True then env'
             else error "invalid argument types"

checkCall :: TypeEnv -> Func_C -> Type
checkCall env (Function_Call (Id var) args)
  = case Map.lookup var env of
    Just (TyFun ty  targs) -> if targs == (argFCTypes env args) then ty
                              else error "invalid argument types"
    _ -> error "invalid function name"
---lista de tipos
argTypes :: Args -> [Type]
argTypes ((AssignType t _):xs) = t:(argTypes xs)
argTypes ((AssignTExp t _ _):xs) = t:(argTypes xs)
argTypes [] = []

argFCTypes :: TypeEnv -> Exps -> [Type]
argFCTypes env (var:xs)
  = (checkExp env  var):(argFCTypes env xs)
argFCTypes env [] = []

checkArgs :: TypeEnv -> Args -> TypeEnv
checkArgs env [] =  env
checkArgs env (first:rest) = checkArgs (checkDec env first) rest

checkProg :: TypeEnv -> Prog -> Bool
checkProg env (p:ps) = let env1 = (checkFunc env p) in
  env1 `seq ` checkProg env1 ps
checkProg env [] = True

checkCode :: Prog -> Bool
checkCode ps = checkProg Map.empty ps

checkBlocDec :: TypeEnv -> Blo_Dec -> TypeEnv
checkBlocDec env [] =  env
checkBlocDec env (first:rest) = checkBlocDec (checkDec env first) rest

checkAll env [] = True
checkAll env (first:rest) = checkInst env first && checkAll env rest
