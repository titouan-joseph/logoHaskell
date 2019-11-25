
data Instruction = Forward Int | Left Int | Right Int | Repeat Int [Instruction] deriving (Read, Show)

crayon :: Float -> Float -> Float
crayon x y angle =

generator :: [Instruction] -> [String]
generator [] = "</svg>"
generator (currentInstruction:restOfList) = case currentInstruction of
    (Forward x) -> generator restOfList
        where
    (Right x) ->
        where
    (Left x) ->
        where
    (Reapeat x [Instruction]) ->
        where
