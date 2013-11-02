import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)

import Control.Monad.State

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


-- Sample valid PNM
-- P5
-- 640 480
-- 255
-- BINARY DATA

-- matchHeader (L8.pack "P5")
matchHeader :: L.ByteString -> Parser ()
matchHeader prefix = do
    str <- get
    guard (prefix `L8.isPrefixOf` str)
    put $ L8.dropWhile isSpace (L.drop (L.length prefix) str)

getNatural :: Parser Int
getNatural = do
    str <- get
    case L8.readInt str of
         Nothing -> lift Nothing
         Just (num, rest)
            | num <= 0 -> lift Nothing
            | otherwise -> put rest >> return num

getBytes :: Int -> Parser L.ByteString
getBytes n = do
    str <- get
    let count = fromIntegral n
        (prefix, postfix) = L.splitAt count str
    guard (L.length prefix == count)
    put postfix
    return prefix

skipSpace :: Parser ()
skipSpace = modify $ L8.dropWhile isSpace

(<<) :: (Monad m) => m b -> m a -> m b
a << b = b >> a

parseP5 :: Parser Greymap
parseP5 = do
    matchHeader (L8.pack "P5")
    width   <- getNatural
    height  <- getNatural << skipSpace
    maxgrey <- getNatural << skipSpace
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
