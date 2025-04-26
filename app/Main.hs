module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Control.Monad
import Lib


--getArgs - возвращает список аргументов командной строки, кот были переданы программе
main :: IO ()
main = do
    args <- getArgs 
    case args of
        [inputTextFile, inputBmpFile, shiftStr, bitsPByte] -> do
            let shift = read shiftStr :: Int  -- Преобразуем строку в целое число
            let bitsPerByte = read bitsPByte :: Int
            -- Проверка значений
            when (bitsPerByte < 1 || bitsPerByte > 8) $ do
                putStrLn "Ошибка: bitsPerByte должно быть от 1 до 8."
                exitFailure            
            when (shift < 1 || shift > 5) $ do
                putStrLn "Ошибка: shift должно быть от 1 до 5."
                exitFailure
            content <- readFile inputTextFile
            let encryptedContent = encryptByCaesar shift content
            let outputEncryptedTextFile = "D:/LabHaskell/lab2/EncryptedTextFile.txt"
            writeFile outputEncryptedTextFile encryptedContent
            putStrLn "Файл успешно зашифрован шифром Цезаря"
            embedTextInBMP inputBmpFile encryptedContent bitsPerByte shift
            putStrLn "Текст зашифрован в изображение"
            decodeTextFromBMP ("D:/LabHaskell/lab2/" ++ (show shift) ++ ".bmp") bitsPerByte
            putStrLn "Файл успешно расшифрован!"
            decryptedStr <- readFile "D:/LabHaskell/lab2/DecryptedTextFile.txt"
            --let encryptedStr = readFile "D:/LabHaskell/lab2/EncryptedTextFile.txt"
            if content == decryptedStr
                then putStrLn "Файлы до шифровки и после расшифровки совпадают!"
                else putStrLn "Файлы до шифровки и после расшифровки не совпадают!"          
        _ -> putStrLn "Аргументы некорректные, формат: программа <входной текстовый файл> <входной файл изображения> <сдвиг> <кол-во бит на байт>"