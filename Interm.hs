module Interm where

import Parser
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State

type Temp = String
type Label = String

data Instr = MOVE Temp Temp
           | MOVEI Temp Int
           | LABEL Label
           | JUMP Label
           | PLUSI Temp Temp Int
           | PLUS Temp Temp Temp
           | MINUS Temp Temp Temp
           | MINUSI Temp Temp Int
           | MULT Temp Temp Temp
           | MULTI Temp Temp Int
           | DIV Temp Temp Temp
           | DIVI Temp Temp Int
           | REST Temp Temp Temp
           | RESTI Temp Temp Int
           | EQUAL_EQUAL Temp Temp Label Label
           | LESS Temp Temp Label Label
           | L_EQUAL Temp Temp Label Label
           | GREATER Temp Temp Label Label
           | G_EQUAL Temp Temp Label Label
           | DIFF Temp Temp Label Label
           | AND_OP Temp Temp Label Label
           | OR_OP Temp Temp Label Label
           | NOT_OP Temp Label Label
           | FCALL Temp Atom [Temp]
           | FDEC Atom [Temp]
           | RETURN Temp
           | PRINT_INT Temp
           | SCAN_INT Temp
          deriving Show

type Table =  Map Atom Temp

type Count = (Int, Int)

--type ArExp = (Exp, ArOp, Exp)

newTemp :: State Count Temp
newTemp = do (temps,labels) <- get
             put (temps+1,labels)
             return ("t"++show temps)

newLabel :: State Count Label
newLabel = do (temps,labels) <- get
              put (temps,labels+1)
              return ("L"++show labels)

transExp :: Table -> Exp -> Temp -> State Count [Instr]
transExp tabl (Id_Exp x) dest
    = case Map.lookup x tabl of
      Just temp -> return [MOVE dest temp]
      Nothing -> error "invalid variable"

transExp tabl (Num n) dest
    = return [MOVEI dest n]

transExp tabl (Arit arexp) dest = transArExp tabl arexp dest

transExp tabl (Function f) dest = transFunc_C tabl f

transExp tabl (Scan_Int) dest =  return [SCAN_INT dest]

transArExp :: Table -> ArExp -> Temp -> State Count [Instr]
transArExp tabl (Plus exp1 exp2) dest
    = do temp1 <- newTemp
         temp2 <- newTemp
         code1 <- transExp tabl exp1 temp1
         code2 <- transExp tabl exp2 temp2
         return (code1 ++ code2 ++ [PLUS dest temp1 temp2])

transArExp tabl (Minus exp1 exp2) dest
    = do temp1 <- newTemp
         temp2 <- newTemp
         code1 <- transExp tabl exp1 temp1
         code2 <- transExp tabl exp2 temp2
         return (code1 ++ code2 ++ [MINUS dest temp1 temp2])

transArExp tabl (Mult exp1 exp2) dest
    = do temp1 <- newTemp
         temp2 <- newTemp
         code1 <- transExp tabl exp1 temp1
         code2 <- transExp tabl exp2 temp2
         return (code1 ++ code2 ++ [MULT dest temp1 temp2])

transArExp tabl (Div exp1 exp2) dest
    = do temp1 <- newTemp
         temp2 <- newTemp
         code1 <- transExp tabl exp1 temp1
         code2 <- transExp tabl exp2 temp2
         return (code1 ++ code2 ++ [DIV dest temp1 temp2])

transArExp tabl (Rest exp1 exp2) dest
    = do temp1 <- newTemp
         temp2 <- newTemp
         code1 <- transExp tabl exp1 temp1
         code2 <- transExp tabl exp2 temp2
         return (code1 ++ code2 ++ [REST dest temp1 temp2])

transCond :: Table -> Exp -> Label -> Label -> State Count [Instr]
transCond tabl (BoolE (Equal_Equal exp1 exp2)) labelt labelf
   = do temp1 <- newTemp
        temp2 <- newTemp
        code1 <- transExp tabl exp1 temp1
        code2 <- transExp tabl exp2 temp2
        return (code1 ++ code2 ++ [EQUAL_EQUAL temp1 temp2 labelt labelf])

transCond tabl (BoolE (Less exp1 exp2)) labelt labelf
    = do temp1 <- newTemp
         temp2 <- newTemp
         code1 <- transExp tabl exp1 temp1
         code2 <- transExp tabl exp2 temp2
         return (code1 ++ code2 ++ [LESS temp1 temp2 labelt labelf])

transCond tabl (BoolE (L_Equal exp1 exp2)) labelt labelf
    = do temp1 <- newTemp
         temp2 <- newTemp
         code1 <- transExp tabl exp1 temp1
         code2 <- transExp tabl exp2 temp2
         return (code1 ++ code2 ++ [L_EQUAL temp1 temp2 labelt labelf])

transCond tabl (BoolE (Greater exp1 exp2)) labelt labelf
    = do temp1 <- newTemp
         temp2 <- newTemp
         code1 <- transExp tabl exp1 temp1
         code2 <- transExp tabl exp2 temp2
         return (code1 ++ code2 ++ [GREATER temp1 temp2 labelt labelf])

transCond tabl (BoolE (G_Equal exp1 exp2)) labelt labelf
    = do temp1 <- newTemp
         temp2 <- newTemp
         code1 <- transExp tabl exp1 temp1
         code2 <- transExp tabl exp2 temp2
         return (code1 ++ code2 ++ [G_EQUAL temp1 temp2 labelt labelf])

transCond tabl (BoolE (Diff exp1 exp2)) labelt labelf
    = do temp1 <- newTemp
         temp2 <- newTemp
         code1 <- transExp tabl exp1 temp1
         code2 <- transExp tabl exp2 temp2
         return (code1 ++ code2 ++ [DIFF temp1 temp2 labelt labelf])

transStm :: Table -> Inst -> State Count [Instr]
transStm tabl (AssignExp var expr)
    = case Map.lookup var tabl of
        Nothing -> error "undefined variable"
        Just dest -> do temp1 <- newTemp
                        code1 <- transExp tabl expr temp1
                        return (code1 ++ [MOVE dest temp1])

transStm tabl (If cond stm1)
    = do ltrue <- newLabel
         lfalse <- newLabel
         code0 <- transCond tabl cond ltrue lfalse
         code1 <- transStm tabl stm1
         return (code0 ++ [LABEL ltrue] ++ code1 ++ [LABEL lfalse])

transStm tabl (If_Else cond stm1 stm2)
    = do ltrue <- newLabel
         lfalse <- newLabel
         lend <- newLabel
         code0 <- transCond tabl cond ltrue lfalse
         code1 <- transStm tabl stm1
         code2 <- transStm tabl stm2
         return (code0 ++ [LABEL ltrue] ++ code1 ++
                [JUMP lend, LABEL lfalse] ++ code2 ++ [LABEL lend])
transStm tabl (While cond stm1)
    = do lloop <- newLabel
         lcond <- newLabel
         lend <- newLabel
         code0 <- transBlo_Inst tabl stm1
         code1 <- transCond tabl cond lloop lend
         return ([JUMP lcond]++[LABEL lloop] ++ code0 ++[LABEL lcond] ++
                 code1 ++ [LABEL lend])
transStm tabl (Bloco blo_inst)
  = transBlo_Inst tabl blo_inst

transStm tabl (Return expr)
  = do temp1 <- newTemp
       code1 <- transExp tabl expr temp1
       return (code1 ++ [RETURN temp1])


transStm tabl (Print_Int stm1)
  = do temp1 <- newTemp
       code1 <- transExp tabl stm1 temp1
       return (code1 ++ [PRINT_INT temp1])

transSimple :: Table -> Simple -> State Count [Instr]
transSimple tabl (AssignTExp _ at ex) = case Map.lookup at tabl of
  Just t -> transExp tabl ex t
  Nothing -> error "undefined variable"

transSimple tabl (AssignType _ at) = case Map.lookup at tabl of
  Just t -> transExp tabl (Id_Exp at) t
  Nothing -> error "undefined variable"


transBlo_Inst :: Table -> Blo_Inst -> State Count [Instr]
transBlo_Inst tabl (inst:insts) = do code1 <- transStm tabl inst
                                     code2 <- transBlo_Inst tabl insts
                                     return (code1 ++ code2)
transBlo_Inst tabl [] = return []

transBlo_Dec :: Table -> Blo_Dec -> State Count [Instr]
transBlo_Dec tbl (dec:decs) = do code1 <- transSimple tbl dec
                                 code2 <- transBlo_Dec tbl decs
                                 return $ code1 ++ code2
transBlo_Dec _ [] = return []


---chamar esta
tableSimp :: Table -> Simple -> State Count Table
tableSimp tbl (AssignType _ at) = do t1 <- newTemp
                                     return $ Map.insert at t1 tbl
tableSimp tbl (AssignTExp _ at _) = do t1 <- newTemp
                                       return $ Map.insert at t1 tbl


transFunc_Dec :: Table -> Func_Dec -> State Count [Instr]
transFunc_Dec tabl (Declaration ty at args blo_dec blo_inst) =
  do tabl1 <- tableArgs tabl args
     tabl2 <- tableBloc_Dec tabl1 blo_dec
     code1 <- transBlo_Dec tabl2 blo_dec
     code2 <- transBlo_Inst tabl2 blo_inst
     ts <- funcArgs tabl2 args
     return $ [FDEC at ts] ++ code1 ++ code2

funcArgs :: Table -> Args -> State Count [Temp]
funcArgs tabl (a:as) = do t1 <- newTemp
                          ts <- funcArgs tabl as
                          return $ t1:ts
funcArgs _ [] = return []

transFunc_C :: Table -> Func_C -> State Count [Instr]
transFunc_C tabl (Function_Call at exps) =
  do t1 <- newTemp
     (code1, ts) <- argTemps tabl exps
     return $ code1 ++ [FCALL t1 at ts]

tableBloc_Dec :: Table -> Blo_Dec -> State Count Table
tableBloc_Dec tabl (dec:decs) = case dec of
  AssignType _ at -> do t0 <- newTemp
                        tableBloc_Dec (Map.insert at t0 tabl) decs
  AssignTExp _ at _ -> do t0 <- newTemp
                          tableBloc_Dec (Map.insert at t0 tabl) decs
tableBloc_Dec tabl [] = return tabl

tableArgs :: Table -> Args -> State Count Table
tableArgs tabl (arg:args) = case arg of
  AssignType _ at -> do t0 <- newTemp
                        tableArgs (Map.insert at t0 tabl) args
  AssignTExp _ at _ -> do t0 <- newTemp
                          tableArgs (Map.insert at t0 tabl) args
tableArgs tabl [] = return tabl

argTemps :: Table -> Exps -> State Count ([Instr], [Temp])
argTemps tabl (expr:exprs) = do t1 <- newTemp
                                code1 <- transExp tabl expr t1
                                (code2, ts) <- argTemps tabl exprs
                                return (code1 ++ code2, t1:ts)
argTemps tabl [] = return ([], [])

transProg :: Table -> Prog -> State Count [Instr]
transProg tabl (prog:progs) = do code1 <- transFunc_Dec tabl prog
                                 code2 <- transProg tabl progs
                                 return (code1 ++ code2)
transProg tabl [] = return []

tableFuncDec :: Table -> Func_Dec -> State Count Table
tableFuncDec tabl (Declaration ty at args blo_dec blo_inst) =
  do t1 <- tableArgs tabl args
     tableBloc_Dec t1 blo_dec

tableProg :: Table -> Prog -> State Count Table
tableProg tabl (f:fs) = do t1 <- tableFuncDec tabl f
                           tableProg t1 fs
tableProg tabl [] = return tabl

genProg :: Prog -> [Instr]
genProg p = let (t, count) = runState (tableProg Map.empty p) (0,0) in
  evalState (transProg t p) count
