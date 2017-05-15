import System.IO
import System.Environment
import System.Exit
import Control.Monad
import Data.Ord
import Data.List
import Data.List.Split
import Data.Word
import Data.Char
import Data.Maybe (fromJust)
import Numeric (showHex, showIntAtBase)
import qualified Data.Binary.Put as P
import qualified Data.Binary.Get as G
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Internal as I

data Huffman = Folha Int Char
            | No Int Huffman Huffman
            deriving (Eq,Ord,Show,Read)

-- USO: gerar uma árvore a partir de string: let a = easytree "string"
--            gerar código de huffman a partir da string: let cod = easycode "string"
--            decodificar código gerado: decodificar cod a

binario :: Num a => [Char] -> a
binario xs = binario' (reverse xs) -- escopo de binario' é binario
            where
                binario' [x] = bin x
                binario' (x:xs) = bin x + 2 * binario' xs
                bin '0' = 0
                bin '1' = 1

bin2dec :: Num a => [Char] -> a
bin2dec = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i
    where c2i c = if c == '0' then 0 else 1

-- Decimal p/ Binário

dec2bin x = showIntAtBase 2 intToDigit x ""

dec2binWord8 :: (Show a, Integral a) => a -> String
dec2binWord8 x = dec2binHelper . dec2bin $ x
    where
        dec2binHelper x
            | length x < 8 = dec2binHelper ( "0" ++ x )
            | otherwise = x

dec2binWord8Full :: (Show a, Integral a) => [a] -> String
dec2binWord8Full x = filter (/= ' ') (unwords $ map dec2binWord8 (init x) ++ [dec2bin . last $ x])

freqSimb :: String -> [Huffman]
freqSimb [] = []
freqSimb (x:xs) = (Folha (freq x (x:xs)) x):freqSimb (filter (/= x) xs) where
    freq _ [] = 0
    freq x y = 1 + freq x (tail(filter (== x) y))


freq :: Huffman -> Int
freq (Folha i _) = i
freq (No i _ _) = i

getHuffFromFile [] = []
getHuffFromFile ((c,f):xs) = (Folha f c):getHuffFromFile xs

compareH :: Huffman -> Huffman -> Ordering
compareH l r = compare (freq l) (freq r)

sortH :: [Huffman] -> [Huffman]
sortH h = sortBy (compareH) h

constructH:: [Huffman] -> Huffman
constructH s = constructH' (sortH s)
    where
        constructH' [x] = x
        constructH' (x:y:xs) = constructH' . sortH $ (No (freq x + freq y) x y):xs

easytree :: String -> Huffman
easytree s = constructH . freqSimb $ s

serializeC :: Huffman -> [(Char, String)]
serializeC h = serializeCH h ""
        where
            serializeCH (Folha _ c) v = [(c, v)]
            serializeCH (No i h1 h2) v = serializeCH h1 (v ++ "0") ++ serializeCH h2 (v ++ "1")

codebook :: [(Char, String)] -> [(Char, String, Int)]
codebook s = codebookHelper $ sortBy (comparing fst) s
    where
        codebookHelper [] = []
        codebookHelper ((c,v):xs) = [(c, v, length v)] ++ codebook xs

easycode :: String -> String
easycode a = codificar a (constructH $ freqSimb a)

codeToDecWord8 :: Num a => [Char] -> [a]
codeToDecWord8 input = map binario ( chunksOf 8 input )

codificar :: String -> Huffman -> String
codificar a h = codificar' a (serializeC $ h)
        where
            codificar' [] s = []
            codificar' (x:xs) s = fromJust (lookup x s) ++ codificar' xs s

decodificar :: String -> Huffman -> String
decodificar a h = decodificar' (a ++ " ") h h
        where
            decodificar' [] _ h = []
            decodificar' s (Folha _ c) h = [c] ++ decodificar' s h h
            decodificar' (x:xs) (No _ h1 h2) h 
                | x == '0' = decodificar' xs h1 h 
                | otherwise = decodificar' xs h2 h

-------------------------------------
-- Binary / File Handling / Word8/32
type Reg = (Word8, Word32)

freqf :: Eq a => [a] -> [(a, Int)]
freqf [] = []
freqf (x:xs) = (x, length (filter (==x) xs) + 1):freqf(filter (/=x) xs)

put :: [(Char, Int)] -> P.PutM ()
put [] = P.flush
put ((c,f):xs) =
    do 
        P.putWord8 (I.c2w c)     -- I.c2w converte char p/ word
        P.putWord32be (toEnum f) -- toEnum converte de Int p/ Word32
        put xs

putCode :: [Int] -> P.PutM ()
putCode [] = P.flush
putCode (x:xs) =
    do
        P.putWord8 (toEnum x)
        putCode xs

getregs :: (Num t, Eq t) => t -> G.Get [(Char, Int)]
getregs n =
    do
        if n == 0 then return []
            else do {
                    r <- getreg ; rs <- getregs (n-1); return (r:rs) }

getreg :: G.Get (Char, Int)
getreg =
    do
        c <- G.getWord8
        f <- G.getWord32be
        return (I.w2c c, fromEnum f)

getbin :: G.Get Int
getbin =
    do
        b <- G.getWord8
        return (fromEnum b)

getBins :: (Num t, Eq t) => t -> G.Get [Int]
getBins f =
    do
        empty <- G.isEmpty
        if empty || (f == 0) then return []
            else do {r <- getbin; rs <- getBins (f-1); return (r:rs)}

getMain :: G.Get ([(Char, Int)], [Int])
getMain =
    do
        n <- G.getWord8
        t <- G.getWord32be
        cbook <- getregs n
        bin <- getBins t
        return (cbook, bin)

getFirstListFromTuple :: (t, t1) -> t
getFirstListFromTuple (x, _) = x

getSecondListFromTuple :: (t, t1) -> t1
getSecondListFromTuple (_, y) = y

---------------------------- Main functions
encode :: FilePath -> FilePath -> IO()
encode inname outname  = 
    do
        txt          <- readFile inname
        let f        = freqf txt                                             -- pares (c, f)
        let n        = P.runPut $ P.putWord8 (toEnum . length $ f)           -- numero de caracteres
        let huffm    = constructH . getHuffFromFile $ f                      -- Árvore
        let wordlist = codeToDecWord8 $ codificar txt huffm                  -- vetor de ints a partir do codigo
        let t        = P.runPut $ P.putWord32be (toEnum . length $ wordlist) -- n. chars cod
        let cbook    = P.runPut (put f)                                      -- codebook
        let bincode  = P.runPut (putCode wordlist)                           -- codigo p/ int

        L.writeFile outname (L.concat ([n,t,cbook,bincode]))

encodeDebug :: FilePath -> FilePath -> IO()
encodeDebug inname outname  = 
    do
        txt          <- readFile inname
        let f        = freqf txt                                             -- pares (c, f)
        let n        = P.runPut $ P.putWord8 (toEnum . length $ f)           -- numero de caracteres
        let huffm    = constructH . getHuffFromFile $ f                      -- árvore
        let wordlist = codeToDecWord8 $ codificar txt huffm       
        let t        = P.runPut $ P.putWord32be (toEnum . length $ wordlist) -- n. chars cod
        let cbook    = P.runPut (put f)     
        let bincode  = P.runPut (putCode wordlist)                           -- codigo p/ int
        
        putStrLn (show wordlist ++ "\n" ++ show (length f) ++ "\n" ++ show (length wordlist) ++ "\n")
        putStrLn (show f ++ "\n")
        putStrLn (show huffm ++ "\n")

        L.writeFile outname (L.concat ([n,t,cbook,bincode]))

decode :: FilePath -> FilePath -> IO()
decode inname outname = 
    do
        file         <- L.readFile inname
        let ret      = G.runGet getMain file              -- Tupla de vetores contendo codebook e vetor de Ints
        let cbook    = getFirstListFromTuple ret          -- Pega codebook
        let wordlist = getSecondListFromTuple ret         -- Pega vetor de Ints (código a ser decodiicado)
        let bincode  = dec2binWord8Full wordlist          -- Transforma vetor de Ints em String, com o código em binário
        let folhas   = getHuffFromFile cbook              -- Cria folhas a partir do codebook
        let huffm    = constructH folhas                  -- Cria arvore a partir das folhas

        writeFile outname (decodificar bincode huffm)     -- Decodifica código utilizando árvore e escreve no arquivo

decodeDebug :: FilePath -> FilePath -> IO()
decodeDebug inname outname = 
    do
        file         <- L.readFile inname
        let ret      = G.runGet getMain file
        let cbook    = getFirstListFromTuple ret
        let wordlist = getSecondListFromTuple ret
        let bincode  = dec2binWord8Full wordlist
        let folhas   = getHuffFromFile cbook
        let huffm    = constructH folhas

        putStrLn (show cbook ++ "\n")
        putStrLn (show huffm ++ "\n")

        writeFile outname (decodificar bincode huffm)

main :: IO()
main = do
    args <- getArgs
    case args of
        ["c", a, b]  -> encode a b
        ["cv", a, b] -> encodeDebug a b
        ["vc", a, b] -> encodeDebug a b
        ["d", a, b]  -> decode a b
        ["vd", a, b] -> decodeDebug a b
        ["dv", a, b] -> decodeDebug a b
        ["-h"] -> do
            putStrLn "Usage: ./huff [c|d] 'input' 'output'"
            putStrLn "Examples:\n\t./huff c 'input' 'output' --> compress file\n\t./huff d 'input' 'output' --> decompress file"
            putStrLn "Use 'v' argument for verbose. (as in ./huff cv ...)"
        ["-v"]       -> putStrLn "Huffman Text File compressor/decompressor. Version 1.0\n(c) Marcos Felipe Eipper / 2017 ~ @markx3"
        ["--demo"]   -> do 
            { encodeDebug "huff.hs" "compressed.bin";
             decodeDebug "compressed.bin" "decompressed.hs" }
        otherwise -> putStrLn "Invalid arguments. Use -h for help."

-------------------------------------
encodeDecode :: String -> String ---- by LKRaider
encodeDecode input =
        let huffm = easytree input
            coded = easycode input
            dcode = decodificar coded huffm
            codDec = codeToDecWord8 coded
            decodDec = dec2binWord8Full codDec
            dcode2 = decodificar decodDec huffm
        in unlines [show huffm, coded, dcode, show (input == dcode), show codDec, show decodDec, show (decodDec == coded), dcode2]

--main :: IO()
--main =  getArgs >>= putStr . encodeDecode . unwords