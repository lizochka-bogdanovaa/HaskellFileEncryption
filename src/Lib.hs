module Lib where

import System.IO
import Data.Bits
import Data.Char
import Data.Word
import qualified Data.ByteString as B
import Data.Word (Word8)
import Numeric (readInt)
import System.FilePath (takeBaseName)

decryptByCaesar :: Int -> String -> String
decryptByCaesar shift str = map (caesarCipher (-shift)) str

bitsToInt :: [Bool] -> Int
bitsToInt bits = foldl (\acc b -> acc * 2 + if b then 1 else 0) 0 bits

bitsToText :: [Bool] -> Int-> String
bitsToText bits bitsPerByte = map (toEnum . bitsToInt) (chunksOf 8 bits)

extractBits :: [Word8] -> Int -> [Bool]
extractBits bytes bitsPerByte =
     concatMap (\byte ->  [testBit byte i | i <- reverse[0 .. (bitsPerByte - 1)]]) bytes


-- Функция для извлечения текстовых битов до маркера завершения
extractTextBits :: [Bool] -> [Bool]
extractTextBits bits = go bits []
  where
    go [] acc = reverse acc
    go xs acc
      | take 32 xs == replicate 32 True = reverse acc  -- Если нашли 8 нулей, завершаем
      | otherwise = go (tail xs) (head xs : acc)


decodeTextFromBMP :: FilePath -> Int -> IO ()
decodeTextFromBMP inputFile bitsPerByte = do
    handle <- openBinaryFile inputFile ReadMode
    contents <- B.hGetContents handle
    let bmpHeaderSize = 54  
        bmpData = B.drop bmpHeaderSize contents
        bmpBytes = B.unpack bmpData
        extractedBits = extractBits bmpBytes bitsPerByte 
        textBits = extractTextBits extractedBits  -- извлекаем текстовые биты до маркера завершения
        decodedText = bitsToText textBits bitsPerByte
        shift = read (takeBaseName inputFile) :: Int 
        decodedCaesarStr = decryptByCaesar shift decodedText 
    let outputDecryptedFile = "D:/LabHaskell/lab2/DecryptedTextFile.txt"  
    writeFile outputDecryptedFile decodedCaesarStr
    hClose handle


--шифр Цезаря: 
--принимает - смещение и символ, кот нужно зашифровать
--возвращает - зашифрованный символ
--fromEnum c - возвращает целое число, соответствующее этому символу в таблице ASCII, toEnum - возвращает символ по ASCII коду 
--если символ между a и z или A и Z, то сдвигаем
--иначе - возвращаем неизмененный символ 
caesarCipher :: Int -> Char -> Char
caesarCipher shift c
    | 'a' <= c && c <= 'z' = toEnum ((fromEnum c - fromEnum 'a' + shift) `mod` 26 + fromEnum 'a')
    | 'A' <= c && c <= 'Z' = toEnum ((fromEnum c - fromEnum 'A' + shift) `mod` 26 + fromEnum 'A')
    | otherwise = c 

--Применяем шифр Цезаря ко всем символам строки 
--принимает: сдвиг и строку
--с помощью map применяет к каждому символу ф-ю caesarCipher
--возвращает: зашифрованную строку
encryptByCaesar :: Int -> String -> String
encryptByCaesar shift str = map (caesarCipher shift) str


-- Ф-я для преобразования целого числа Int в список булевых значений [Bool], представляющих двоичное представление этого числа
-- создаем список, где для каждого i от 0 до 7 вызывается функция testBit
-- Ф-я testBit проверяет, установлен ли i-й бит числа n: стандартная функция из модуля Data.Bits, которая принимает два аргумента: число и индекс бита 
-- возвращает True, если указанный бит установлен = 1, и False, если = 0
intToBits :: Int -> [Bool]
intToBits n = reverse [testBit n i | i <- [0..7]]


-- Ф-я для преобразования текста в список битов
-- concatMap - функция высшего порядка, принимает: функцию и список, применяет функцию к каждому элементу списка и затем конкатенирует все полученные списки в один
-- оператор . позволяет комбинировать две функции так, чтобы результат одной функции передавался в качестве аргумента другой функции
-- сначала вызывается fromEnum, которая преобразует символ в его ASCII код, а затем результат передается в intToBits, 
-- которая преобразует это числовое значение в список битов
-- затем добавляем 32 True к списку, полученному от concatMap, для обозначения конца последовательности битов
textToBits :: String -> [Bool]
textToBits text = concatMap (intToBits . fromEnum) text ++ replicate 32 True


-- Функция для разделения списка на подсписки фиксированной длины
-- если список пустой, то возвращает пустой список
-- иначе рекурсивно: take - берет первые n элементов из списка xs
-- если xs содержит меньше n элементов, то будет возвращен весь оставшийся список
-- drop - отбрасывает первые n элементов из списка xs, возвращая оставшуюся часть списка
-- : - оператор конкатенации - результат take n xs добавляется в начало списка, который формируется рекурсивным вызовом chunksOf 
-- создаем новый список, состоящий из подсписков
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Функция для встраивания битов в байт, начиная с младшего бита
-- принимает: подсписок битов, текущий байт для встраивания битов
-- foldl - проходится по всем парам (bit, pos), сформированным с помощью zip из bitChunk (+reverse, чтобы с младших битов байта идти)
-- применяет анонимную ф-ю к текущему значению байта b
-- анонимн ф-я устанавливает в соответствии с pos биты в байте (делает 0 или 1)
embedByte :: [Bool] -> Int -> Word8 -> Word8
embedByte bitChunk bitsPerByte byte = foldl (\b (bit, pos) -> if bit then setBit b pos else clearBit b pos) byte (zip paddedBits positions)
  where
    positions = reverse [0 .. bitsPerByte - 1]
    -- Если bitChunk меньше, чем bitsPerByte, добавляем нули
    paddedBits = take bitsPerByte (bitChunk ++ repeat False)

-- Ф-я для встраивания битов текста в массив байтов файла
-- принимает: список байтов, в который будут встроены биты; cписок битов, которые нужно встроить; количество битов, которое будет встроено в каждый байт; список байтов после встраивания битов
-- ф-я chunksOf разбивает список битов на подсписки заданного размера bitsPerByte
-- zipWith3 - функция, которая принимает функцию embedByte и два списка в качестве параметров список подсписков bits и список bytes 
-- она объединяет эти два списка, применяя embedByte между соответствующими элементами
embedBits :: [Word8] -> [Bool] -> Int -> [Word8] 
embedBits bytes bits bitsPerByte = 
    let bitsLength = length bits
        availableBytes = length bytes
        maxBits = availableBytes * bitsPerByte
        embeddedBits = take maxBits bits ++ repeat False  -- Заполнение нулями
        modifiedBytes = zipWith3 embedByte (chunksOf bitsPerByte embeddedBits) (take availableBytes (repeat bitsPerByte)) bytes 
    in modifiedBytes


-- Ф-я для встраивания текста в Bitmap файл
-- принимает: имя входного файла; текст, который нужно встроить; количество бит на байт, ключ - смещение в шифре Цезаря
-- ф-я openBinaryFile открывает файл в бинарном режиме, принимает путь к файлу inputFile и режим ReadMode,
-- если файл успешно открыт, openBinaryFile возвращает дескриптор файла, который затем присваивается переменной handle
-- ф-я hGetContents из handle считываем все содержимое файда в contents
-- устанавливаем размер заголовка bmpHeaderSize 
-- с помощью drop отбрасываем от contents заголовок и передаем в bmpData
-- преобразовываем bmpData в список байтов bmpBytes
-- вызываем ф-ю textToBits и преобразовываем строку в список битов
-- вызываем ф-ю embedBits и встраиваем биты в байты
-- задаем имя нового файла - название - ключ
-- записываем в новый файл измененные байты
embedTextInBMP :: FilePath -> String -> Int -> Int -> IO ()
embedTextInBMP inputFile text bitsPerByte shift = do
    handle <- openBinaryFile inputFile ReadMode
    contents <- B.hGetContents handle
    let bmpHeaderSize = 54  
        bmpData = B.drop bmpHeaderSize contents
        bmpBytes = B.unpack bmpData -- Преобразуем ByteString в список байтов
        bits = textToBits text
        modifiedBytes = embedBits bmpBytes bits bitsPerByte 
    let newOutputFile = "D:/LabHaskell/lab2/" ++ show shift ++ ".bmp"
   -- Открываем файл для записи в бинарном режиме
    outputHandle <- openBinaryFile newOutputFile WriteMode
    -- Записываем заголовок
    B.hPut outputHandle (B.take bmpHeaderSize contents)
    -- Записываем измененные данные
    B.hPut outputHandle (B.pack modifiedBytes)
    hClose handle
    hClose outputHandle

