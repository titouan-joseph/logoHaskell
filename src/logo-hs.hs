module Main where

import Prelude hiding (Left, Right)
import System.IO
import System.Environment
import Data.List
import Data.Maybe (fromMaybe)

--Definition des data
data Crayonrayon = Crayon Float Float Float deriving(Show)
data Instruction = Forward Float
                 | Left Float
                 | Right Float
                 | Repeat Int [Instruction]
                 deriving (Read, Show)
type ListInstruction = [Instruction]

--Variables
originCrayon = Crayon 200.0 200.0 0.0
svgContent = "<?xml version=\"1.0\" encoding=\"utf-8\"?> \n <svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"400\" height=\"400\"> \n <title>LogoHaskell Generator</title> \n"
defaultColor = "black"
colorList = ["green","red","black","pink","maroon","orange","yellow","olive","purple","fuchsia","lime","teal","aqua","blue","navy","gray","silver","white"]
errorType = "Couleur non reconnue, utiliser le format hexadecimal ou les noms suivant: \n" ++ (show colorList)

--Fonction ajout d'element dans une liste pour la construction du xml
addElemInList x c lst
    | c <= 0 = lst
    | c > 0 = addElemInList x (c-1) (x ++ lst)

--Fonction parse to convert String to Instruction
parse string = read (string) :: ListInstruction

--Fonction pour generer le code xml
generator [] _ svgContent _ = svgContent ++ "</svg>" --Fin de la recurtion
generator (currentInstruction:restOfList) (Crayon x y angle) svgContent color = case currentInstruction of
                (Forward value) -> (generator restOfList newCrayon newSvgContent color)
                    where newCrayon = Crayon (x+ value*cos(angle*pi/180)) (y+ value*(-sin(angle*pi/180))) (angle)
                          newSvgContent = svgContent ++ "<line x1=\"" ++ (show x) ++ "\" y1=\""++ (show y)++"\" x2=\""++ (show (x+value*cos(angle*pi/180))) ++ "\" y2=\""++ (show (y+value*(-sin(angle*pi/180)))) ++ "\" stroke=\"" ++ color ++ "\" stroke-width=\"1\" /> \n"
                (Right value) -> (generator restOfList newCrayon svgContent color)
                   where newCrayon = Crayon (x) (y) (angle + value)
                (Left value) -> (generator restOfList newCrayon svgContent color)
                   where newCrayon = Crayon (x) (y) (angle - value)
                (Repeat nbInstru instruction) -> (generator restOfListWithRepeat newCrayon svgContent color)
                    where newCrayon = Crayon x y angle
                          restOfListWithRepeat = (addElemInList instruction nbInstru restOfList)

main = do
    args <- getArgs  --Recupere les arguments passer en parametre lors de l'appel du programme
    inputfile <- openFile (args !! 0) ReadMode --Ouverture du fichier en mode lecture
    content <- hGetContents inputfile  --Get instruction

-- Verification de l'extension
    let outputfile = if (('g' == (last (args !! 1))) &&         --On test si les 4 dernier caractere correspondent a .svg
                         ('v' == (last(init(args !! 1)))) &&
                         ('s' == (last(init(init(args !! 1))))) &&
                         ('.' == (last(init(init(init(args !! 1)))))))
                         then (args !! 1)
                         else ((args !! 1) ++ ".svg")

-- On regarde si l'utilisateur a defini une couleur
    if length args == 3
    then
        if (((args !! 2) `elem` colorList ) || ((head (args !! 2) == '#') && ((length(tail (args !! 2))) == 6)))  --verfication des noms de couleur dans la preiere partie, pour le format hexadecimal, nous faissons confiance a l'utilisateur our utiliser les bon caracteres
        then
            writeFile outputfile (generator (parse content) originCrayon svgContent (args !! 2))
        else
            print errorType  -- Indication a l'utilisateur que la couleur n'est pas reconnue
    else
        writeFile outputfile (generator (parse content) originCrayon svgContent defaultColor)