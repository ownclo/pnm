import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)

import Control.Monad.State
import Data.Maybe

data Greymap = Greymap {
    greyWidth  :: Int,
    greyHeight :: Int,
    greyMax    :: Int,
    greyData   :: L.ByteString
} deriving (Eq)

instance Show Greymap where
    show (Greymap w h m _) = "Greymap " ++
        show w ++ "x" ++ show h ++ " " ++ show m


type ParseState = L.ByteString
type Parser = StateT ParseState Maybe

runParser :: Parser a -> ParseState -> Maybe (a, ParseState)
runParser = runStateT


-- Sample of a valid PNM
-- P5
-- 640 480
-- 255
-- ...BINARY DATA...

-- matchHeader (L8.pack "P5")
matchHeader :: L.ByteString -> Parser ()
matchHeader prefix = do
    guard . (prefix `L8.isPrefixOf`) =<< get
    modify $ L.drop (L.length prefix)

getNatural :: Parser Int
getNatural = do
    num <- StateT L8.readInt
    guard (num > 0)
    return num

getBytes :: Int -> Parser L.ByteString
getBytes n = do
    let count = fromIntegral n
    prefix <- state $ L.splitAt count
    guard (L.length prefix == count)
    return prefix

skipSpace :: Parser ()
skipSpace = modify $ L8.dropWhile isSpace

(<<) :: (Monad m) => m b -> m a -> m b
a << b = b >> a

parseP5 :: Parser Greymap
parseP5 = do
    matchHeader (L8.pack "P5")
    [width, height, maxgrey] <- replicateM 3 $
        getNatural << skipSpace
    guard (maxgrey < 256)
    skipSpace
    bitmap <- getBytes (width * height)
    return $ Greymap width height maxgrey bitmap


main :: IO ()
main = do
    contents <- L.readFile "mountain.pnm"
    let s = case runParser parseP5 contents of
         Just (g, _) -> show g
         Nothing -> "Parsing error occured..."
    putStrLn . show $ s
