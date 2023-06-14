{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- | Chapter 9: Input and Output
module Chapter09
  ( showShortLines,
    respondPalindromes,
    printFile,
    printFile2,
    printFile3,
    deleteTodoItem,
    printEnvs,
    todoApp,
    threeCoins,
    randoms',
    finiteRandoms,
    randomStr,
    askForNumber,
    askForNumber2,
    copyFile',
    catchExample,
  )
where

import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Data.List (delete)
import System.Directory
import System.Environment
import System.IO
import System.IO.Error (ioeGetFileName, isDoesNotExistError)
import System.Random

showShortLines :: IO ()
showShortLines = interact $ unlines . filter ((< 10) . length) . lines

respondPalindromes :: IO ()
respondPalindromes =
  interact $
    unlines
      . map
        (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome")
      . lines
  where
    isPalindrome xs = xs == reverse xs

printFile :: FilePath -> IO ()
printFile path = do
  h <- openFile path ReadMode
  contents <- hGetContents h
  putStr contents
  hClose h

printFile2 :: FilePath -> IO ()
printFile2 path =
  withFile
    path
    ReadMode
    ( \h -> do
        contens <- hGetContents h
        putStr contens
    )

printFile3 :: FilePath -> IO ()
printFile3 path =
  withFile
    path
    ReadMode
    ( \h -> do
        hSetBuffering h $ BlockBuffering (Just 2048)
        contents <- hGetContents h
        putStr contents
    )

deleteTodoItem :: IO ()
deleteTodoItem = do
  h <- openFile "todo.txt" ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents h
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0 ..] todoTasks
  putStrLn "These are yout TODO items:"
  putStr $ unlines numberedTasks
  putStrLn "Which one do you want to delete?"
  numberString <- getLine
  let number = read numberString
      newTodoItems = delete (todoTasks !! number) todoTasks
  hPutStr tempHandle $ unlines newTodoItems
  hClose h
  hClose tempHandle
  removeFile "todo.txt"
  renameFile tempName "todo.txt"

printEnvs :: IO ()
printEnvs = do
  args <- getArgs
  progName <- getProgName
  putStrLn "The arguments are:"
  mapM_ putStrLn args
  putStrLn "The progname name is:"
  putStrLn progName

todoApp :: IO ()
todoApp = do
  (command : args) <- getArgs
  let (Just action) = lookup command distpatch
  action args

distpatch :: [(String, [String] -> IO ())]
distpatch =
  [ ("add", add),
    ("view", view),
    ("remove", remove)
  ]

add :: [String] -> IO ()
add [fileNmae, todoItem] = appendFile fileNmae (todoItem ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0 ..] todoTasks
  putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, numberString] = do
  h <- openFile fileName ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents h
  let number = read numberString
      todoTasks = lines contents
      newTodoItems = delete (todoTasks !! number) todoTasks
  hPutStr tempHandle $ unlines newTodoItems
  hClose h
  hClose tempHandle
  removeFile fileName
  renameFile tempName fileName

threeCoins :: StdGen -> (Bool, Bool, Bool, StdGen)
threeCoins gen =
  let (first, newGen) = random gen
      (second, newGen') = random newGen
      (third, newGen'') = random newGen'
   in (first, second, third, newGen'')

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = let (value, newGen) = random gen in value : randoms' newGen

finiteRandoms :: (RandomGen g, Random a, Num n, Eq n) => n -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
  let (value, newGen) = random gen
      (restOfList, finalGen) = finiteRandoms (n - 1) newGen
   in (value : restOfList, finalGen)

randomStr :: IO ()
randomStr = do
  gen <- newStdGen
  let randomChars = randomRs ('a', 'z') gen
      (first20, rest) = splitAt 20 randomChars
      (second20, _) = splitAt 20 rest
  putStrLn first20
  putStrLn second20

askForNumber :: StdGen -> IO ()
askForNumber gen = do
  let (randNumber, newGen) = randomR (1, 10) gen :: (Int, StdGen)
  putStrLn "guess a number"
  numberString <- getLine
  unless (null numberString) $ do
    let number = read numberString :: Int
    if randNumber == number
      then putStrLn "you are correnct"
      else putStrLn $ "Sorry, it was " ++ show randNumber
    askForNumber newGen

askForNumber2 :: IO ()
askForNumber2 = do
  gen <- getStdGen
  let (randNumber, _) = randomR (1, 10) gen :: (Int, StdGen)
  putStrLn "guess a number"
  numberString <- getLine
  unless (null numberString) $ do
    let number = read numberString :: Int
    if randNumber == number
      then putStrLn "you are correnct"
      else putStrLn $ "Sorry, it was " ++ show randNumber
    _ <- newStdGen
    askForNumber2

copyFile' :: FilePath -> FilePath -> IO ()
copyFile' source dest = do
  contents <- B.readFile source
  B.writeFile dest contents

catchExample :: FilePath -> IO ()
catchExample path = toTry path `catch` handler
  where
    handler :: IOError -> IO ()
    handler e
      | isDoesNotExistError e =
          case ioeGetFileName e of
            Just path' -> putStrLn $ path' ++ " does not exits"
            Nothing -> putStrLn "some trouple here"
      | otherwise = ioError e
    toTry fileName = do
      contents <- readFile fileName
      putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines."
