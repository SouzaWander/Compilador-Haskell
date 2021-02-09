module Main where
import Parser
import Lexer
import AST
import Interm
import MachineCode
import Typecheck
import Control.Monad.State -- para correr computação State Count
import qualified Data.Map as Map -- para tabela de símbolos

main :: IO ()
main = do
    txt <- getContents      -- ler toda a entrada padrão
    let stm = parser (alexScanTokens txt)   
    let mc = machineCode code
    print stm -- resultado do parser
    putStrLn "\n\n"
    print code -- resultado do intermedio
    putStrLn "\n\n"
    print mc -- resultado do maquina
    putStrLn "\n\n"
    printProg mc -- resultado final do maquina
