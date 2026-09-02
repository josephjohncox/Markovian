{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad (forM_, unless, void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as BL
import Data.Char (digitToInt, isHexDigit, ord)
import Data.List (find, isInfixOf)
import Data.Proxy (Proxy (..))
import Data.Word (Word64)
import GHC.Float (castDoubleToWord64, castWord64ToDouble)
import GHC.TypeLits (KnownNat)
import Markovian.Tensor
import Markovian.Tensor.SafeTensors
import Paths_markovian_safetensors (getDataFileName)
import System.Exit (exitFailure)

sessionLimits :: SessionLimits
sessionLimits = tensorSessionLimits 8 1024 4096 32768 262144 128 65536

largeSafeLimits :: SafeTensorLimits
largeSafeLimits = requireLimits 1048576 262144 64 1024 8 1024 4096 32768

main :: IO ()
main = do
    limitConstructorTests
    canonicalAndRoundTrip
    tensorSessionAllocationPlan
    malformedCorpus
    exactAndOneBelowLimits
    putStrLn "markovian-safetensors: bounded canonical profile tests passed"

limitConstructorTests :: IO ()
limitConstructorTests = do
    case safeTensorLimits 100000000 100000000 1 1 1 1 1 1 of
        Right _ -> pure ()
        Left problem -> failTest ("pinned header maximum was rejected: " ++ renderSafeTensorError problem)
    case safeTensorLimits 100000001 100000001 1 1 1 1 1 1 of
        Left problem | "pinned format maximum" `isInfixOf` renderSafeTensorError problem -> pure ()
        _ -> failTest "header limit above the pinned format maximum was admitted"
    let maximumWireDimension = fromIntegral (maxBound :: Word64)
    case safeTensorLimits 1024 512 1 16 4 maximumWireDimension 1 8 of
        Right _ -> pure ()
        Left problem -> failTest ("exact Word64 dimension limit was rejected: " ++ renderSafeTensorError problem)
    case safeTensorLimits 1024 512 1 16 4 (maximumWireDimension + 1) 1 8 of
        Left problem | "Word64 wire maximum" `isInfixOf` renderSafeTensorError problem -> pure ()
        _ -> failTest "one-above Word64 dimension limit was admitted"

canonicalAndRoundTrip :: IO ()
canonicalAndRoundTrip = expectSession "canonical session" $ \session -> do
    (scalar, _) <- expectTensor "scalar" (hostTensorFromList session SF64 SNil [3.5])
    (signedZero, _) <- expectTensor "signed zero" (hostTensorFromList session SF64 SNil [-0.0])
    (nanPayload, _) <- expectTensor "NaN payload" (hostTensorFromList session SF64 SNil [castWord64ToDouble 0x7ff8000000000042])
    (infinity, _) <- expectTensor "infinity" (hostTensorFromList session SF64 SNil [castWord64ToDouble 0x7ff0000000000000])
    (subnormal, _) <- expectTensor "subnormal" (hostTensorFromList session SF64 SNil [castWord64ToDouble 1])
    (matrix, _) <- expectTensor "matrix" (hostTensorFromList session SF64 (matrixShape @2 @2) [1, 2, 3, 4])
    (empty, _) <- expectTensor "empty" (hostTensorFromList session SF64 (matrixShape @0 @3) [])
    scalarName <- requireName "scalar"
    zeroName <- requireName "signed-zero"
    nanName <- requireName "nan"
    infinityName <- requireName "infinity"
    subnormalName <- requireName "subnormal"
    matrixName <- requireName "matrix"
    emptyName <- requireName "empty"
    let entries =
            [ (zeroName, someHostTensor signedZero)
            , (scalarName, someHostTensor scalar)
            , (matrixName, someHostTensor (transpose2D matrix))
            , (infinityName, someHostTensor infinity)
            , (emptyName, someHostTensor empty)
            , (subnormalName, someHostTensor subnormal)
            , (nanName, someHostTensor nanPayload)
            ]
    encoded <- expectSafeIO "encode canonical" (encodeSafeTensors largeSafeLimits entries)
    encodedAgain <- expectSafeIO "encode deterministic" (encodeSafeTensors largeSafeLimits (reverse entries))
    assertEqual "canonical bytes independent of input order" encoded encodedAgain
    duplicateEncoding <- encodeSafeTensors largeSafeLimits [(scalarName, someHostTensor scalar), (scalarName, someHostTensor scalar)]
    case duplicateEncoding of
        Left problem | "DuplicateTensorName" `isInfixOf` renderSafeTensorError problem -> pure ()
        _ -> failTest "encoder admitted a duplicate name"
    goldenPath <- getDataFileName "test/golden/canonical-f64.hex"
    goldenText <- readFile goldenPath
    golden <- either failTest pure (decodeHex (filter (/= '\n') goldenText))
    assertEqual "canonical byte golden" golden encoded

    decoded <- expectSafeIO "decode canonical" (decodeSafeTensors session largeSafeLimits encoded)
    let decodedEntries = safeTensorEntries decoded
    assertEqual "canonical decoded name order" ["empty", "infinity", "matrix", "nan", "scalar", "signed-zero", "subnormal"] (map (safeTensorNameText . fst) decodedEntries)
    checkEntry decodedEntries "empty" [0, 3] []
    checkEntry decodedEntries "infinity" [] [0x7ff0000000000000]
    checkEntry decodedEntries "matrix" [2, 2] (map castDoubleToWord64 [1, 3, 2, 4])
    checkEntry decodedEntries "nan" [] [0x7ff8000000000042]
    checkEntry decodedEntries "scalar" [] [castDoubleToWord64 3.5]
    checkEntry decodedEntries "signed-zero" [] [0x8000000000000000]
    checkEntry decodedEntries "subnormal" [] [1]

    case find ((== "nan") . safeTensorNameText . fst) decodedEntries of
        Nothing -> failTest "decoded NaN entry absent"
        Just (_, tensor) -> do
            refinement <- withSomeHostTensor tensor $ \raw -> fmap void (finiteTensor raw)
            case refinement of
                Left _ -> pure ()
                Right () -> failTest "SafeTensors decoding incorrectly created finite evidence for NaN"

    reencoded <- expectSafeIO "re-encode decoded" (encodeSafeTensors largeSafeLimits decodedEntries)
    assertEqual "decode/encode canonical identity" encoded reencoded
    decodedAgain <- expectSafeIO "decode re-encoded" (decodeSafeTensors session largeSafeLimits reencoded)
    assertEqual "decode/encode/decode names" (map (safeTensorNameText . fst) decodedEntries) (map (safeTensorNameText . fst) (safeTensorEntries decodedAgain))
    pure (Right ())

tensorSessionAllocationPlan :: IO ()
tensorSessionAllocationPlan = do
    let constrained = tensorSessionLimits 1 1 1 8 8 1 1
        twoTensors = wire "{\"a\":{\"dtype\":\"F64\",\"shape\":[1],\"data_offsets\":[0,8]},\"b\":{\"dtype\":\"F64\",\"shape\":[1],\"data_offsets\":[8,16]}}" (payloadWords [1, 2])
        oneTensor = wire "{\"a\":{\"dtype\":\"F64\",\"shape\":[1],\"data_offsets\":[0,8]}}" (payloadWords [1])
    result <- withTensorSession constrained $ \session -> do
        rejected <- decodeSafeTensors session largeSafeLimits twoTensors
        case rejected of
            Left problem | "BufferLimitExceeded" `isInfixOf` renderSafeTensorError problem -> pure ()
            _ -> failTest "decoder did not reject the complete two-buffer allocation plan"
        admitted <- decodeSafeTensors session largeSafeLimits oneTensor
        case admitted of
            Right _ -> pure (Right ())
            Left problem -> failTest ("failed decode consumed session budget: " ++ renderSafeTensorError problem)
    case result of
        Right () -> pure ()
        Left problem -> failTest ("allocation-plan session failed: " ++ show problem)

malformedCorpus :: IO ()
malformedCorpus = expectSession "malformed corpus session" $ \session -> do
    let one = payloadWords [castDoubleToWord64 1]
        two = payloadWords [castDoubleToWord64 1, castDoubleToWord64 2]
        three = payloadWords [1, 2, 3]
        descriptor dtype shape offsets = "{\"dtype\":\"" ++ dtype ++ "\",\"shape\":" ++ shape ++ ",\"data_offsets\":" ++ offsets ++ "}"
        entry name body = "\"" ++ name ++ "\":" ++ body
        object fields = "{" ++ fields ++ "}"
        validOne = object (entry "x" (descriptor "F64" "[1]" "[0,8]"))
    reject session "malformed JSON" "MalformedJSON" (wire "{\"x\":" BS.empty)
    reject session "header must begin with object" "MalformedJSON" (wire " {}" BS.empty)
    reject session "padding must be spaces" "MalformedJSON" (wire "{}\t" BS.empty)
    reject session "malformed UTF-8" "InvalidUTF8" (wireBytes (BS.pack ([0x7b, 0x22, 0xff, 0x22, 0x3a] ++ map (fromIntegral . ord) (descriptor "F64" "[0]" "[0,0]") ++ [0x7d])) BS.empty)
    reject session "duplicate names" "DuplicateTensorName" (wire (object (entry "x" (descriptor "F64" "[0]" "[0,0]") ++ "," ++ entry "x" (descriptor "F64" "[0]" "[0,0]"))) BS.empty)
    reject session "escaped duplicate names" "DuplicateTensorName" (wire ("{\"a\":" ++ descriptor "F64" "[0]" "[0,0]" ++ ",\"\\u0061\":" ++ descriptor "F64" "[0]" "[0,0]" ++ "}") BS.empty)
    reject session "duplicate descriptor key" "DuplicateDescriptorField" (wire (object (entry "x" "{\"dtype\":\"F64\",\"dtype\":\"F64\",\"shape\":[0],\"data_offsets\":[0,0]}")) BS.empty)
    reject session "trailing descriptor comma" "MalformedJSON" (wire (object (entry "x" "{\"dtype\":\"F64\",\"shape\":[0],\"data_offsets\":[0,0],}")) BS.empty)
    reject session "unsupported dtype" "UnsupportedDType" (wire (object (entry "x" (descriptor "F32" "[1]" "[0,4]"))) (BS.replicate 4 0))
    reject session "metadata excluded" "UnsupportedMetadata" (wire "{\"__metadata__\":{}}" BS.empty)
    reject session "shape payload mismatch" "PayloadSizeMismatch" (wire (object (entry "x" (descriptor "F64" "[2]" "[0,8]"))) one)
    reject session "shape product limit" "ElementLimitExceeded" (wire (object (entry "x" (descriptor "F64" "[1024,1024]" "[0,0]"))) BS.empty)
    reject session "offset order" "OffsetOrderInvalid" (wire (object (entry "x" (descriptor "F64" "[1]" "[8,0]"))) one)
    reject session "hole" "PayloadHole" (wire (object (entry "a" (descriptor "F64" "[1]" "[0,8]") ++ "," ++ entry "b" (descriptor "F64" "[1]" "[16,24]"))) three)
    reject session "overlap" "PayloadOverlap" (wire (object (entry "a" (descriptor "F64" "[2]" "[0,16]") ++ "," ++ entry "b" (descriptor "F64" "[1]" "[8,16]"))) two)
    reject session "truncated payload" "PayloadTruncated" (wire validOne (BS.take 7 one))
    reject session "trailing payload" "TrailingBytes" (wire validOne (one <> BS.singleton 0))
    reject session "integer overflow" "NumericOverflow" (wire (object (entry "x" (descriptor "F64" "[1]" "[0,18446744073709551616]"))) one)
    forM_ (["[0,1024]", "[1024,0]", "[1,0,1024]"] :: [String]) $ \shape -> do
        admitted <- decodeSafeTensors session largeSafeLimits (wire (object (entry "empty" (descriptor "F64" shape "[0,0]"))) BS.empty)
        case admitted of
            Right _ -> pure ()
            Left problem -> failTest ("zero-coordinate shape " ++ shape ++ " was order-dependent: " ++ renderSafeTensorError problem)
    reject session "unknown descriptor key" "UnknownDescriptorField" (wire (object (entry "x" "{\"dtype\":\"F64\",\"shape\":[0],\"data_offsets\":[0,0],\"device\":\"cpu\"}")) BS.empty)
    reject session "missing descriptor key" "MissingDescriptorField" (wire (object (entry "x" "{\"dtype\":\"F64\",\"shape\":[0]}")) BS.empty)
    reject session "header truncation" "HeaderTruncated" (BL.toStrict (Builder.toLazyByteString (Builder.word64LE 100 <> Builder.byteString (asciiBytes "{}"))))
    reject session "file too short" "FileTooShort" (BS.replicate 7 0)
    pure (Right ())

exactAndOneBelowLimits :: IO ()
exactAndOneBelowLimits = expectSession "limit session" $ \session -> do
    (matrix, _) <- expectTensor "limit matrix" (hostTensorFromList session SF64 (matrixShape @2 @2) [1, 2, 3, 4])
    name <- requireName "abcdefghijk"
    encoded <- expectSafeIO "limit fixture encode" (encodeSafeTensors largeSafeLimits [(name, someHostTensor matrix)])
    let fileBytes = fromIntegral (BS.length encoded)
        headerBytes = fromIntegral (word64Prefix encoded)
        payloadBytes = 32
        exact = requireLimits fileBytes headerBytes 1 11 2 2 4 payloadBytes
    _ <- expectSafeIO "all exact limits" (decodeSafeTensors session exact encoded)
    let oneBelow =
            [ ("file", requireLimits (fileBytes - 1) (min headerBytes (fileBytes - 1)) 1 11 2 2 4 payloadBytes)
            , ("header", requireLimits fileBytes (headerBytes - 1) 1 11 2 2 4 payloadBytes)
            , ("tensor count", requireLimits fileBytes headerBytes 0 11 2 2 4 payloadBytes)
            , ("name", requireLimits fileBytes headerBytes 1 10 2 2 4 payloadBytes)
            , ("rank", requireLimits fileBytes headerBytes 1 11 1 2 4 payloadBytes)
            , ("dimension", requireLimits fileBytes headerBytes 1 11 2 1 4 payloadBytes)
            , ("elements", requireLimits fileBytes headerBytes 1 11 2 2 3 payloadBytes)
            , ("payload", requireLimits fileBytes headerBytes 1 11 2 2 4 (payloadBytes - 1))
            ]
    forM_ oneBelow $ \(label, limits) -> do
        result <- decodeSafeTensors session limits encoded
        case result of
            Left _ -> pure ()
            Right _ -> failTest (label ++ " one-below limit was admitted")
    forM_ oneBelow $ \(label, constrained) -> do
        encodeBelow <- encodeSafeTensors constrained [(name, someHostTensor matrix)]
        case encodeBelow of
            Left _ -> pure ()
            Right _ -> failTest ("encoder admitted one-below " ++ label ++ " limit")
    exactEncoded <- expectSafeIO "encoder all exact limits" (encodeSafeTensors exact [(name, someHostTensor matrix)])
    assertEqual "exact-limit encode/decode identity bytes" encoded exactEncoded
    pure (Right ())

matrixShape :: forall rows columns. (KnownNat rows, KnownNat columns) => SShape '[rows, columns]
matrixShape = SCons (Proxy @rows) (SCons (Proxy @columns) SNil)

checkEntry :: [(SafeTensorName, SomeHostTensor region)] -> String -> [Integer] -> [Word64] -> IO ()
checkEntry entries wantedName wantedShape wantedWords = case find ((== wantedName) . safeTensorNameText . fst) entries of
    Nothing -> failTest ("missing decoded entry " ++ wantedName)
    Just (_, tensor) -> do
        assertEqual (wantedName ++ " shape") wantedShape (map toInteger (someHostTensorDimensions tensor))
        values <- someHostTensorToList tensor
        assertEqual (wantedName ++ " raw words") wantedWords (map castDoubleToWord64 values)

reject :: TensorSession region -> String -> String -> ByteString -> IO ()
reject session label expected bytes = do
    result <- decodeSafeTensors session largeSafeLimits bytes
    case result of
        Left problem -> unless (expected `isInfixOf` renderSafeTensorError problem) (failTest (label ++ ": wrong error " ++ renderSafeTensorError problem))
        Right _ -> failTest (label ++ " was admitted")

wire :: String -> ByteString -> ByteString
wire = wireBytes . asciiBytes

wireBytes :: ByteString -> ByteString -> ByteString
wireBytes header payload = BL.toStrict (Builder.toLazyByteString (Builder.word64LE (fromIntegral (BS.length header)) <> Builder.byteString header <> Builder.byteString payload))

payloadWords :: [Word64] -> ByteString
payloadWords words64 = BL.toStrict (Builder.toLazyByteString (foldMap Builder.word64LE words64))

asciiBytes :: String -> ByteString
asciiBytes = BS.pack . map (fromIntegral . ord)

word64Prefix :: ByteString -> Word64
word64Prefix bytes = sum [fromIntegral (BS.index bytes index) * 2 ^ (8 * index) | index <- [0 .. 7]]

decodeHex :: String -> Either String ByteString
decodeHex [] = Right BS.empty
decodeHex (high : low : rest)
    | isHexDigit high && isHexDigit low = BS.cons (fromIntegral (digitToInt high * 16 + digitToInt low)) <$> decodeHex rest
decodeHex _ = Left "invalid canonical-f64.hex"

requireName :: String -> IO SafeTensorName
requireName input = case safeTensorName input of
    Left problem -> failTest ("invalid test name: " ++ renderSafeTensorError problem)
    Right name -> pure name

requireLimits :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> SafeTensorLimits
requireLimits fileBytes headerBytes tensors names rank dimensions elements payload =
    case safeTensorLimits (fromInteger fileBytes) (fromInteger headerBytes) (fromInteger tensors) (fromInteger names) (fromInteger rank) (fromInteger dimensions) (fromInteger elements) (fromInteger payload) of
        Left problem -> error (renderSafeTensorError problem)
        Right limits -> limits

expectSession :: String -> (forall region. TensorSession region -> IO (Either TensorError ())) -> IO ()
expectSession label action = do
    result <- withTensorSession sessionLimits action
    case result of
        Left problem -> failTest (label ++ ": " ++ show problem)
        Right () -> pure ()

expectTensor :: (Show error) => String -> IO (Either error value) -> IO value
expectTensor label action = action >>= either (failTest . ((label ++ ": ") ++) . show) pure

expectSafeIO :: String -> IO (Either SafeTensorError value) -> IO value
expectSafeIO label action = action >>= either (failTest . ((label ++ ": ") ++) . renderSafeTensorError) pure

assertEqual :: (Eq value, Show value) => String -> value -> value -> IO ()
assertEqual label expected actual = unless (expected == actual) (failTest (label ++ ": expected " ++ show expected ++ ", got " ++ show actual))

failTest :: String -> IO value
failTest message = do
    putStrLn ("FAIL: " ++ message)
    exitFailure
