{-# LANGUAGE OverloadedStrings #-}

import Data.Word (Word8)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<*), (*>), (<|>))
import System.Environment (getArgs)
import qualified Data.Attoparsec.ByteString as A (parseOnly, Parser, many', skipWhile, endOfInput)
import Data.Attoparsec.ByteString.Char8 (char)
import qualified Data.ByteString as B
import qualified Data.IntMap as IM
import Control.Monad.State.Strict

data BFCommand = MoveLeft
               | MoveRight
               | Increment
               | Decrement
               | Input
               | Output
               | Loop BFProgram
               deriving (Show)

type BFProgram = [BFCommand]

parseChar :: Char -> BFCommand -> A.Parser BFCommand
parseChar c cmd = char c >> return cmd

parseComment :: A.Parser ()
parseComment = A.skipWhile $ \x -> x `B.notElem` "+-<>.,[]"

parseMoveLeft :: A.Parser BFCommand
parseMoveLeft = parseChar '<' MoveLeft

parseMoveRight :: A.Parser BFCommand
parseMoveRight = parseChar '>' MoveRight

parseIncrement :: A.Parser BFCommand
parseIncrement = parseChar '+' Increment

parseDecrement :: A.Parser BFCommand
parseDecrement = parseChar '-' Decrement

parseInput :: A.Parser BFCommand
parseInput = parseChar ',' Input

parseOutput :: A.Parser BFCommand
parseOutput = parseChar '.' Output

parseLoop :: A.Parser BFCommand
parseLoop = do
    char '['
    program <- parseBFProgram
    char ']'
    return $ Loop program

parseInstruction :: A.Parser BFCommand
parseInstruction = parseMoveLeft
                <|> parseMoveRight
                <|> parseIncrement
                <|> parseDecrement
                <|> parseInput
                <|> parseOutput
                <|> parseLoop

parseBFProgram :: A.Parser BFProgram
parseBFProgram = parseComment *> A.many' (parseInstruction <* parseComment)

parseBF :: B.ByteString -> Either String BFProgram
parseBF = A.parseOnly (parseBFProgram <* A.endOfInput)

getOrZero :: Maybe Word8 -> Word8
getOrZero = fromMaybe 0

runBFCommand :: BFCommand -> StateT (Int, IM.IntMap Word8) IO ()
runBFCommand cmd = do
    (n, tape) <- get
    let w = getOrZero $ IM.lookup n tape
    case cmd of
      MoveLeft -> put (n - 1, tape)
      MoveRight -> put (n + 1, tape)
      Increment -> put (n, IM.insert n (w + 1) tape)
      Decrement -> put (n, IM.insert n (w - 1) tape)
      Input -> do
        c <- liftIO getChar
        put (n, IM.insert n (fromIntegral $ fromEnum c) tape)
      Output -> liftIO $ putChar $ toEnum $ fromIntegral w
      loop@(Loop program) -> case w of
                               0 -> return ()
                               _ -> interpret program >> runBFCommand loop

interpret :: BFProgram -> StateT (Int, IM.IntMap Word8) IO ()
interpret = mapM_ runBFCommand

main :: IO()
main = do
    bfFilePath <- fmap (!! 0) getArgs
    bfFile <- B.readFile bfFilePath
    case parseBF bfFile of
      Left e -> putStr e
      Right p -> evalStateT (interpret p) (0, IM.empty)
