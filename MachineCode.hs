module MachineCode where

import Interm
import Parser

showTemp :: Temp -> String
showTemp t
  | t == "t0" = "$t0"
  | t == "t1" = "$t1"
  | t == "t2" = "$t2"
  | t == "t3" = "$t3"
  | t == "t4" = "$t4"
  | t == "t5" = "$t5"
  | t == "t6" = "$t6"
  | t == "t7" = "$t7"
  | t == "t8" = "$t8"
  | t == "t9" = "$t9"
  | t == "t10" = "$s0"
  | t == "t11" = "$s1"
  | t == "t12" = "$s2"
  | t == "t13" = "$s3"
  | t == "t14" = "$s4"
  | t == "t15" = "$s5"
  | t == "t16" = "$s6"
  | t == "t17" = "$s7"
  | t == "t18" = "$a0"
  | t == "t19" = "$a1"
  | t == "t20" = "$a2"

  --adcionar mais redistos e erros

machineCode :: [Instr] -> [String]
machineCode (MOVEI t1 i:insts) = ["\tmove " ++ (showTemp t1) ++ ", " ++ (show i)] ++ machineCode insts
machineCode (MOVE t1 t2:insts) = ["\tmove " ++ (showTemp t1) ++ ", " ++ (showTemp t2)] ++ machineCode insts
machineCode (PLUS t1 t2 t3:insts) = ["\tadd " ++ (showTemp t1) ++ ", " ++ (showTemp t2) ++ ", " ++ (showTemp t3)] ++ machineCode insts
machineCode (MINUS t1 t2 t3:insts) = ["\tsub " ++ (showTemp t1) ++ ", " ++ (showTemp t2) ++ ", " ++ (showTemp t3)] ++ machineCode insts
machineCode (MULT t1 t2 t3:insts) = ["\tmul " ++ (showTemp t1) ++ ", " ++ (showTemp t2) ++ ", " ++ (showTemp t3)] ++ machineCode insts
machineCode (DIV t1 t2 t3:insts) = ["\tdiv " ++ (showTemp t1) ++ ", " ++ (showTemp t2) ++ ", " ++ (showTemp t3)] ++ machineCode insts
machineCode (REST t1 t2 t3:insts) = ["\tdiv " ++ (showTemp t1) ++ ", " ++ (showTemp t2) ++ ", " ++ (showTemp t3)] ++ machineCode insts

-- condições
machineCode (LESS t1 t2 l1 l2:LABEL l:insts)
  | l == l1 = ["\tbge " ++ (showTemp t1)  ++ ", " ++ (showTemp t2) ++ ", " ++ l2, l ++ ": "] ++ machineCode insts
  | l == l2 = ["\tblt " ++ (showTemp t1)  ++ ", " ++ (showTemp t2) ++ ", " ++ l1, l ++ ": "] ++ machineCode insts
  | otherwise = ["\tblt " ++ (showTemp t1)  ++ ", " ++ (showTemp t2) ++ ", " ++ l1, "\tj " ++ l2, l ++ ": "] ++ machineCode insts
machineCode (LESS t1 t2 l1 l2:insts) = ["\tblt " ++ (showTemp t1) ++ ", " ++ (showTemp t2) ++ ", " ++ l1, "\tj " ++ l2, l1 ++ ": "] ++ machineCode insts

machineCode (EQUAL_EQUAL t1 t2 l1 l2:LABEL l:insts)
  | l == l1 = ["\tbne " ++ (showTemp t1)  ++ ", " ++ (showTemp t2) ++ ", " ++ l2, l ++ ": "] ++ machineCode insts
  | l == l2 = ["\tbeq " ++ (showTemp t1)  ++ ", " ++ (showTemp t2) ++ ", " ++ l1, l ++ ": "] ++ machineCode insts
  | otherwise = ["\tbeq " ++ (showTemp t1)  ++ ", " ++ (showTemp t2) ++ ", " ++ l1, "\tj " ++ l2, l ++ ": "] ++ machineCode insts
machineCode (EQUAL_EQUAL t1 t2 l1 l2:insts) = ["\tbeq " ++ (showTemp t1) ++ ", " ++ (showTemp t2) ++ ", " ++ l1, "\tj " ++ l2, l1 ++ ": "] ++ machineCode insts

machineCode (GREATER t1 t2 l1 l2:LABEL l:insts)
  | l == l1 = ["\tble " ++ (showTemp t1)  ++ ", " ++ (showTemp t2) ++ ", " ++ l2, l ++ ": "] ++ machineCode insts
  | l == l2 = ["\tbgt " ++ (showTemp t1)  ++ ", " ++ (showTemp t2) ++ ", " ++ l1, l ++ ": "] ++ machineCode insts
  | otherwise = ["\tbgt " ++ (showTemp t1)  ++ ", " ++ (showTemp t2) ++ ", " ++ l1, "\tj " ++ l2, l ++ ": "] ++ machineCode insts
machineCode (GREATER t1 t2 l1 l2:insts) = ["\tbgt " ++ (showTemp t1) ++ ", " ++ (showTemp t2) ++ ", " ++ l1, "\tj " ++ l2, l1 ++ ": "] ++ machineCode insts

machineCode (L_EQUAL t1 t2 l1 l2:LABEL l:insts)
  | l == l1 = ["\tbgt " ++ (showTemp t1)  ++ ", " ++ (showTemp t2) ++ ", " ++ l2, l ++ ": "] ++ machineCode insts
  | l == l2 = ["\tble " ++ (showTemp t1)  ++ ", " ++ (showTemp t2) ++ ", " ++ l1, l ++ ": "] ++ machineCode insts
  | otherwise = ["\tble " ++ (showTemp t1)  ++ ", " ++ (showTemp t2) ++ ", " ++ l1, "\tj " ++ l2, l ++ ": "] ++ machineCode insts
machineCode (L_EQUAL t1 t2 l1 l2:insts) = ["\tble " ++ (showTemp t1) ++ ", " ++ (showTemp t2) ++ ", " ++ l1, "\tj " ++ l2, l1 ++ ": "] ++ machineCode insts

machineCode (G_EQUAL t1 t2 l1 l2:LABEL l:insts)
  | l == l1 = ["\tblt " ++ (showTemp t1)  ++ ", " ++ (showTemp t2) ++ ", " ++ l2, l ++ ": "] ++ machineCode insts
  | l == l2 = ["\tbge " ++ (showTemp t1)  ++ ", " ++ (showTemp t2) ++ ", " ++ l1, l ++ ": "] ++ machineCode insts
  | otherwise = ["\tbge " ++ (showTemp t1)  ++ ", " ++ (showTemp t2) ++ ", " ++ l1, "\tj " ++ l2, l ++ ": "] ++ machineCode insts
machineCode (G_EQUAL t1 t2 l1 l2:insts) = ["\tbge " ++ (showTemp t1) ++ ", " ++ (showTemp t2) ++ ", " ++ l1, "\tj " ++ l2, l1 ++ ": "] ++ machineCode insts

  -- otherwise = machineCode (LESS t1 t2 l1 l2:LABEL l:insts)
--machineCode (EQUAL_EQUAL t1 t2 l1 l2:insts) = ["\tbeq" ++ " " ++ (showTemp t1) ++ ", " ++ (showTemp t2) ++ ]

machineCode (FDEC at args:insts) = case at of
  Id id -> [id ++ ": ", "\tsw $fp, -4($sp)", "\tsw $ra, -8($sp)", "\tla $fp, 0($sp)", "\tla $sp, -" ++ (show $ length args) ++ "($sp)"] ++ machineCode insts

machineCode (FCALL t1 at args:insts) = case at of
  Id id -> (storeArgs args 4)++ ["\tjal " ++ id, "\tla $sp, " ++ (show $ length args) ++ "($sp)"] ++ ["\tmove " ++ t1 ++ ", $v0", "\tlw $ra, -4($sp)"] ++ machineCode insts

machineCode (JUMP l:insts) = ["\tj" ++ l] ++ machineCode insts
machineCode (LABEL l:insts) = [l ++ ": "] ++ machineCode insts

machineCode (RETURN t:insts) = ["\tmove $v0, " ++ t, "\tla $sp, 0($fp)", "\tlw $ra, -8($sp)", "\tlw fp, -4($sp)", "\tjr $ra"] ++ machineCode insts

machineCode (PRINT_INT t:insts) = ["\tsw $v0, -4($sp)", "\tsw $a0, -8($sp)", "\tmove $a0, " ++ t, "\tmovei $v0, 1", "\tsyscall", "\tlw $v0, -4($sp)", "\tlw $a0, -8($sp)"] ++ machineCode insts
machineCode (SCAN_INT t:insts) = ["\tsw $v0, -4($sp)", "\tmovei $v0, 5", "\tsyscall", "\tmove " ++ t ++ ", $v0", "\tlw $v0, -4($sp)"] ++ machineCode insts

machineCode [] = []

storeArgs :: [Temp] -> Int -> [String]
storeArgs (t:ts) c = ["\tsw " ++ t ++ ", -" ++ (show c) ++ "($sp)"] ++ storeArgs ts (c+4)
storeArgs [] c = ["\tla $sp, -" ++ (show (c-4)) ++ "($sp)"]

printProg :: [String] -> IO()
printProg (p:ps) = do putStrLn p
                      printProg ps
printProg [] = putStrLn ""
