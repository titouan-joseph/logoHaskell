import Prelude hiding(Left,Right)

data Programme = Liste_instru [Instruction]

uneInstruction = (read "Forward 100":: Instruction)

type Crayon = [Float]
data Instruction = Forward Int |Right Int | Left Int | Repeat Int deriving(Read)

Forward x = [cos((pos_crayon !! 2)*pi/180)*x,sin((pos_crayon !! 2)**pi/180)*x]

Repeat i instru = case i of
    0 -> []
    int -> Repeat i-1

obtenirDescription :: Instruction -> String
obtenirDescription inst = case inst of
    (Forward x) -> "Deplacer le crayon de " ++ (show x)
    (Right x) -> "tourner de "++ (show x)
    (Left x) -> "tourner de "++ (show x)

function :: [Instruction] -> [Instruction]
function instruction = case instruction of
    []-> instruction
    (x:[]) -> case x of
        Forward c -> instruction
        Left c -> instruction
        Right c -> instruction
        Repeat c list -> case c of
            1 -> function list
            _ -> (function list) ++ (function[Repeat c-1 list])
    (x:xs) -> function x ++ (function xs)