import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)

type LS = L.ByteString

data Greymap = Greymap {
    greyWidth  :: Int,
    greyHeight :: Int
--     greyMax    :: Int,
--     greyData   :: LS
} deriving (Eq)

-- instance Show Greymap where
--     show (Greymap w h m _) = "Greymap " ++
--         show w ++ "x" ++ show h ++ " " ++ show m

instance Show Greymap where
    show (Greymap w h) = "Greymap " ++ show w ++ "x" ++ show h

skipSpace :: LS -> LS
skipSpace = L8.dropWhile isSpace

parseHeader :: LS -> Maybe ((), LS)
parseHeader s = if L8.pack "P5" `L8.isPrefixOf` s
                then Just ((), L.drop 2 s)
                else Nothing

parseWidth :: LS -> Maybe (Int, LS)
parseWidth s = L8.readInt s

parseHeight :: LS -> Maybe (Int, LS)
parseHeight s = L8.readInt s

dummyParseP5 :: LS -> Maybe (Greymap, LS)
dummyParseP5 s = do
    (_, s1) <- parseHeader s
    (w, s2) <- parseWidth $ skipSpace s1
    (h, s3) <- parseHeight $ skipSpace s2
    return (Greymap w h, s3)


main :: IO ()
main = do
    contents <- L.readFile "mountain.pnm"
    let s = case dummyParseP5 contents of
         Just (g, _) -> show g
         Nothing -> "NOPE"
    putStrLn . show $ s
