import System.IO
import System.Environment
import Data.List

string_until (s:rs) sym l = if s==sym then l else string_until rs sym (s:l)
string_until [] sym l = []
tab_before element string = reverse $ string_until string element []
tab_after element string = string_until (reverse string) element []
just_after element string = head $ tab_after element string
data Instruction = MonForward Int | MonLeft Int | MonRight Int | MonRepeat Int [Instruction] deriving (Read, Show)
verif_num x = if x>0 then x else error "-1"
gen_prgm :: [String] -> [Instruction] -> Int -> [Instruction]
gen_prgm [] prg i = prg
gen_prgm (x:xs) prg prev = case x of
    "" -> gen_prgm xs prg prev
    " " -> gen_prgm xs prg prev
    "," -> gen_prgm xs prg prev
    "]" -> (MonRepeat (read (just_after "[" xs) ::Int) (reverse (gen_prgm (tab_before "["  xs) [] 0))):(gen_prgm (tab_after "[" xs) prg (-1))
    "[" -> prg
    "Repeat" -> gen_prgm xs prg prev
    "Forward" -> (MonForward $ verif_num prev):(gen_prgm (xs) prg (-1))
    "Left" -> (MonLeft $ verif_num prev):(gen_prgm (xs) prg (-1))
    "Right" -> (MonRight $ verif_num prev):(gen_prgm (xs) prg (-1))
    _ -> gen_prgm xs prg (read x :: Int)
add_space [] l = l
add_space (c:rs) l = case c of
    '['-> add_space rs (' ':'[':' ':l)
    ']' -> add_space rs (' ':c:' ':l)
    ',' -> add_space rs (' ':c:' ':l)
    '\n' -> add_space rs l
    _ ->add_space rs (c:l)
delete_space :: String -> String
delete_space []=[]
delete_space [c]=[c]
delete_space (s:v:rs)
    | s==' ' && v==' ' = delete_space (v:rs)
    | otherwise = s:(delete_space (v:rs))
remove_start_end string = reverse (tail (tail (reverse (tail (tail string)))))

parse_logoskell string = gen_prgm (reverse (words (remove_start_end (delete_space (reverse (add_space string [])))))) [] 0

aplatir [] = []
aplatir (x:xs) = case x of
    (MonRepeat x xi) -> xi++(aplatir (if x>0 then ((MonRepeat (x-1) xi):(aplatir xs)) else (aplatir xs)))
    _ -> x:(aplatir xs)
tracer :: [Instruction] -> Float -> Float ->Float-> String->String
tracer [] _ _ _ s=s
tracer (i:xs) x y t s= case i of
    (MonForward z) -> tracer xs x1 y1 t ("<line x1=\"" ++(show (x+100))++ "\" y1=\"" ++(show (y+100))++ "\" x2=\"" ++(show (x1+100))++ "\" y2=\"" ++(show (y1+100))++ "\" stroke=\"red\" stroke-width=\"1\" />\n"++s)
       where x1=(realToFrac z)*(cos t)+x
             y1=(realToFrac z)*(sin t)+y
    (MonRight a) -> tracer xs x y (t-(realToFrac a)*3.1415/180) s
    (MonLeft a) -> tracer xs x y (t+(realToFrac a)*3.1415/180) s

tout_faire string = "<?xml version=\"1.0\" encoding=\"utf-8\"?><svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"200\" height=\"200\"> \n <title></title> \n "++(tracer (aplatir (parse_logoskell string)) 0.0 0.0 0.0 "")++"</svg>"

main = do
    args <- getArgs
    inputfile <- openFile (args !! 0) ReadMode
    content <- hGetContents inputfile
    putStrLn content
    writeFile (args !! 1) (tout_faire content)
    putStrLn (tout_faire content)

--     prs <- getLine
--     putStr (tout_faire prs)