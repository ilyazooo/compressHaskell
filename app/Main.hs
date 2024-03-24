module Main (main) where

import qualified Statistic.Huffman as Huffman
import qualified Statistic.ShannonFano as ShannonFano
import qualified RLE
import qualified LZ.LZ78 as LZ78
import qualified LZ.LZW as LZW
import Statistic.EncodingTree (compress, uncompress)
import Statistic.Bit (Bit(..))
import System.IO (hFlush, hPutStr, stdout, Handle, IOMode(..), withBinaryFile, openBinaryFile, hClose, hGetContents, hSeek, SeekMode(AbsoluteSeek))
import System.Directory (doesFileExist, getFileSize)
import System.FilePath ((</>), takeBaseName)
import System.IO.Temp (withSystemTempFile, writeSystemTempFile)
import Data.Char (isSpace)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Control.Monad (forM_, when, replicateM_)
import Data.List (sortBy)
import Data.Ord (comparing)
import System.Process (system)
import Control.Concurrent (threadDelay)


main :: IO ()
main = do
  clearScreen
  putStrLn "Entrez le nom du fichier à compresser dans le dossier 'txt' :"
  fileName <- getLine
  clearScreen
  sizes <- newIORef []
  loadingEffect "Lecture du fichier"
  clearScreen
  let filePath = "txt" </> fileName
  fileExists <- doesFileExist filePath
  if fileExists
    then do
      content <- readFile filePath
      putStrLn $ "Contenu du fichier " ++ filePath ++ " lu."
      putStrLn "\n"
      if all isSpace content
        then putStrLn "Le fichier est vide."
        else do
          putStrLn "\n"
          putStrLn "Choisissez la méthode de compression :"
          putStrLn "1: RLE"
          putStrLn "2: Huffman"
          putStrLn "3: Shannon-Fano"
          putStrLn "4: LZ78"
          putStrLn "5: LZW"
          putStrLn "6: Toutes les méthodes"
          putStrLn "\n"
          putStr "Entrez le numéro de votre choix : "
          hFlush stdout 
          choice <- getLine
          let symbols = chars content
          clearScreen

          case choice of
            "1" ->  do
              loadingEffect "Compression RLE en cours"
              clearScreen
              compressAndDecompressRLE content filePath sizes

            "2" -> do
              loadingEffect "Compression Huffman en cours"
              clearScreen
              compressAndDecompressHuffman symbols filePath sizes

            "3" -> do
              loadingEffect "Compression ShannonFano en cours"
              clearScreen
              compressAndDecompressShannonFano symbols filePath sizes

            "4" -> do
              loadingEffect "Compression LZ78 en cours"
              clearScreen
              compressAndDecompressLZ78 content filePath sizes

            "5" -> do
              loadingEffect "Compression LZW en cours"
              clearScreen
              compressAndDecompressLZW content filePath sizes

            "6" -> do
              loadingEffect "Compression RLE en cours"
              compressAndDecompressRLE content filePath sizes
              clearScreen

              loadingEffect "Compression Huffman en cours"
              compressAndDecompressHuffman symbols filePath sizes
              clearScreen


              loadingEffect "Compression ShannonFano en cours"
              compressAndDecompressShannonFano symbols filePath sizes
              clearScreen

              loadingEffect "Compression LZ78 en cours"
              compressAndDecompressLZ78 content filePath sizes
              clearScreen

              loadingEffect "Compression LZW en cours"
              compressAndDecompressLZW content filePath sizes
              clearScreen
            _ -> putStrLn "Choix non valide."
          
    
          -- Comparaison des tailles de fichiers compressés
          finalSizes <- readIORef sizes
          let sortedSizes = sortBy (comparing snd) finalSizes
          putStrLn "Classement des méthodes de compression par taille de fichier :"
          forM_ sortedSizes $ \(method, size) ->
            putStrLn $ method ++ ": " ++ show size ++ " bytes"
    
    else putStrLn $ "Le fichier " ++ filePath ++ " n'existe pas."



writeTempFile :: FilePath -> String -> String -> IO FilePath
writeTempFile dir template content = do
  tempFilePath <- writeSystemTempFile (dir </> template) content
  return tempFilePath

bitsToString :: [Bit] -> String
bitsToString = concatMap show

chars :: String -> [Char]
chars = id

writeCompressedToFile :: FilePath -> String -> IO ()
writeCompressedToFile filePath content = do
  handle <- openBinaryFile filePath WriteMode
  hPutStr handle content
  hClose handle

printBits :: [Bit] -> IO ()
printBits bits = putStrLn $ concatMap show bits

printBitsToFile :: Handle -> [Bit] -> IO ()
printBitsToFile handle bits = hPutStr handle (concatMap show bits)

bitFromChar :: Char -> Bit
bitFromChar '0' = Zero
bitFromChar '1' = One
bitFromChar _ = error "Invalid bit character"

-- Convertit le résultat de RLE.compress en String pour l'écriture dans un fichier
rleToString :: Show a => [(a, Int)] -> String
rleToString = concatMap (\(c, n) -> show c ++ "," ++ show n ++ ";")

-- Convertit une String en une liste de tuples pour la décompression RLE
stringToRLE :: Read a => String -> [(a, Int)]
stringToRLE str = map readTuple $ splitOn ";" $ init str
  where
    readTuple t = let [c, n] = splitOn "," t in (read c, read n)
    splitOn delimiter = foldr f [[]]
      where
        f c l@(x:xs) | c == head delimiter = [] : l
                     | otherwise = (c : x) : xs


-- Fonction pour nettoyer le terminal
clearScreen :: IO ()
clearScreen = do
  _ <- system "clear || cls"
  return ()

  -- Votre fonction writeCompressedToFile modifiée pour écrire une liste d'entiers
writeCompressedToFileInt :: FilePath -> [Int] -> IO ()
writeCompressedToFileInt filePath compressedData = do
  let content = show compressedData -- Convertit la liste d'entiers en String
  handle <- openBinaryFile filePath WriteMode
  hPutStr handle content
  hClose handle


loadingEffect :: String -> IO ()
loadingEffect message = do
  putStr message
  hFlush stdout
  replicateM_ 7 $ do
    threadDelay 500000  
    putStr "."
    hFlush stdout
  putStrLn ""

-- Fonction pour compresser et décompresser avec RLE
compressAndDecompressRLE :: String -> FilePath -> IORef [(String, Integer)] -> IO ()
compressAndDecompressRLE content filePath sizes = do
  -- Compression avec RLE
  let compressedRLE = RLE.compress content
  let tempFileNameRLE = takeBaseName filePath ++ "-compressed-rle.tmp"
  let tempFilePathRLE = "txt" </> tempFileNameRLE
  writeCompressedToFile tempFilePathRLE (rleToString compressedRLE)
  putStrLn "\n"
  putStrLn $ "Fichier RLE compressé écrit dans: " ++ tempFilePathRLE
  putStrLn "\n"
  compressedRLESize <- getFileSize tempFilePathRLE
  modifyIORef sizes (("RLE", compressedRLESize) :)

  -- Décompression avec RLE
  case RLE.uncompress compressedRLE of
    Just decompressedRLE -> do
      let outputFileNameRLE = takeBaseName filePath ++ "-decompressed-rle.txt"
      let outputFilePathRLE = "txt" </> outputFileNameRLE
      writeFile outputFilePathRLE decompressedRLE
      putStrLn "\n"
      putStrLn $ "Texte décompressé avec RLE écrit dans: " ++ outputFilePathRLE
      putStrLn "\n"
    Nothing -> putStrLn "Erreur lors de la décompression RLE."



-- Fonction pour compresser et décompresser avec LZ78
compressAndDecompressLZ78 :: String -> FilePath -> IORef [(String, Integer)] -> IO ()
compressAndDecompressLZ78 content filePath sizes = do
  -- Compression avec LZ78
  let compressedLZ78 = LZ78.compress content
  let tempFileNameLZ78 = takeBaseName filePath ++ "-compressed-lz78.tmp"
  let tempFilePathLZ78 = "txt" </> tempFileNameLZ78
  writeCompressedToFile tempFilePathLZ78 (show compressedLZ78)
  putStrLn "\n"
  putStrLn $ "Fichier LZ78 compressé écrit dans: " ++ tempFilePathLZ78
  putStrLn "\n"
  compressedLZ78Size <- getFileSize tempFilePathLZ78
  modifyIORef sizes (("LZ78", compressedLZ78Size) :)

  -- Décompression avec LZ78
  case LZ78.uncompress compressedLZ78 of
    Just decompressedLZ78 -> do
      let outputFileNameLZ78 = takeBaseName filePath ++ "-decompressed-lz78.txt"
      let outputFilePathLZ78 = "txt" </> outputFileNameLZ78
      writeFile outputFilePathLZ78 decompressedLZ78
      putStrLn "\n"
      putStrLn $ "Texte décompressé avec LZ78 écrit dans: " ++ outputFilePathLZ78
      putStrLn "\n"
    Nothing -> putStrLn "Erreur lors de la décompression LZ78."



-- Fonction pour compresser et décompresser avec LZW
compressAndDecompressLZW :: String -> FilePath -> IORef [(String, Integer)] -> IO ()
compressAndDecompressLZW symbols filePath sizes = do
  -- Compression avec LZW
  let compressedLZW = LZW.compress symbols
  let tempFileNameLZW = takeBaseName filePath ++ "-compressed-lzw.tmp"
  let tempFilePathLZW = "txt" </> tempFileNameLZW
  writeCompressedToFileInt tempFilePathLZW compressedLZW
  putStrLn $ "Fichier LZW compressé écrit dans: " ++ tempFilePathLZW

  -- Obtention de la taille du fichier compressé
  compressedLZWSize <- getFileSize tempFilePathLZW
  modifyIORef sizes (("LZW", compressedLZWSize) :)

  -- Décompression avec LZW
  let decompressedLZW = LZW.uncompress compressedLZW
  case decompressedLZW of
    Just text -> do
      let outputFileNameLZW = takeBaseName filePath ++ "-decompressed-lzw.txt"
      let outputFilePathLZW = "txt" </> outputFileNameLZW
      writeFile outputFilePathLZW text
      putStrLn $ "Texte décompressé avec LZW écrit dans: " ++ outputFilePathLZW
    Nothing -> putStrLn "Erreur lors de la décompression LZW."


compressAndDecompressHuffman :: String -> FilePath -> IORef [(String, Integer)] -> IO ()
compressAndDecompressHuffman symbols filePath sizes = do
  -- Compression avec Huffman
  let (treeMaybeHuffman, compressedHuffman) = compress Huffman.tree symbols
  case treeMaybeHuffman of
    Nothing -> putStrLn "Impossible de construire l'arbre de Huffman"
    Just _ -> do
      putStrLn "Arbre de Huffman construit avec succès"
      let tempFileNameHuffman = takeBaseName filePath ++ "-compressed-huffman.tmp"
      let tempFilePathHuffman = "txt" </> tempFileNameHuffman
      writeCompressedToFile tempFilePathHuffman (bitsToString compressedHuffman)
      putStrLn "\n"
      putStrLn $ "Fichier Huffman compressé écrit dans: " ++ tempFilePathHuffman
      putStrLn "\n"
      compressedHuffmanSize <- getFileSize tempFilePathHuffman
      modifyIORef sizes (("Huffman", compressedHuffmanSize) :)

  -- Décompression avec Huffman
  case treeMaybeHuffman of
    Nothing -> putStrLn "Décompression Huffman impossible sans arbre"
    Just treeHuffman -> do
      case uncompress (Just treeHuffman, compressedHuffman) of
        Just decompressedHuffman -> do
          let outputFileNameHuffman = takeBaseName filePath ++ "-decompressed-huffman.txt"
          let outputFilePathHuffman = "txt" </> outputFileNameHuffman
          writeFile outputFilePathHuffman decompressedHuffman
          putStrLn "\n"
          putStrLn $ "Texte décompressé avec Huffman écrit dans: " ++ outputFilePathHuffman
          putStrLn "\n"
        Nothing -> putStrLn "Erreur lors de la décompression Huffman."


        
compressAndDecompressShannonFano :: String -> FilePath -> IORef [(String, Integer)] -> IO ()
compressAndDecompressShannonFano symbols filePath sizes = do
  -- Compression avec Shannon-Fano
  let (treeMaybeShannonFano, compressedShannonFano) = compress ShannonFano.tree symbols
  case treeMaybeShannonFano of
    Nothing -> putStrLn "Impossible de construire l'arbre de Shannon-Fano"
    Just _ -> do
      putStrLn "\n"
      putStrLn "Arbre de Shannon-Fano construit avec succès"
      putStrLn "\n"
      let tempFileNameShannonFano = takeBaseName filePath ++ "-compressed-shannonfano.tmp"
      let tempFilePathShannonFano = "txt" </> tempFileNameShannonFano
      writeCompressedToFile tempFilePathShannonFano (bitsToString compressedShannonFano)
      putStrLn "\n"
      putStrLn $ "Fichier Shannon-Fano compressé écrit dans: " ++ tempFilePathShannonFano
      putStrLn "\n"
      compressedShannonFanoSize <- getFileSize tempFilePathShannonFano
      modifyIORef sizes (("Shannon-Fano", compressedShannonFanoSize) :)

  -- Décompression avec Shannon-Fano
  case treeMaybeShannonFano of
    Nothing -> putStrLn "Décompression Shannon-Fano impossible sans arbre"
    Just treeShannonFano -> do
      case uncompress (Just treeShannonFano, compressedShannonFano) of
        Just decompressedShannonFano -> do
          let outputFileNameShannonFano = takeBaseName filePath ++ "-decompressed-shannonfano.txt"
          let outputFilePathShannonFano = "txt" </> outputFileNameShannonFano
          writeFile outputFilePathShannonFano decompressedShannonFano
          putStrLn "\n"
          putStrLn $ "Texte décompressé avec Shannon-Fano écrit dans: " ++ outputFilePathShannonFano
          putStrLn "\n"
        Nothing -> putStrLn "Erreur lors de la décompression Shannon-Fano."