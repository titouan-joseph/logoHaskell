import System.IO
import System.Environment
import Data.List

data Instruction = Forward Int | Left Int | Right Int | Repeat Int [Instruction] deriving (Read, Show)

crayon :: Float -> Float -> Float
crayon x y angle = x y angle

generator :: [Instruction] -> [String]
generator [] = "</svg>"
generator (currentInstruction:restOfList) = case currentInstruction of
    (Forward x) -> generator restOfList ("<line x1=\"" ++ (show crayon.x ) ++ "\" y1=\""++ (show crayon.y )++"\" x2=\""++ (show crayon.x + x*cos angle) ++ "\" y2=\""++ (show crayon.y+ x*-sin angle) ++ "\" stroke=\"red\" stroke-width=\"1\" /> \n")
       where crayon (x+ x*cos angle) (y+ x*-sin angle )(angle)
    (Right a) -> generator restOfList
       where crayon (x) (y) (angle - a*pi/180)
    (Left a) -> generator restOfList
       where crayon (x) (y) (angle + a*pi/180)
--    (Repeat x [Instruction]) ->
--        where
main = do
    args <- getArgs
    inputfile <- openFile (args !! 0) ReadMode
    content <- hGetContents inputfile
    writeFile (args !! 1) (generator content)