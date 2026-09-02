{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}

{- | A bounded canonical SafeTensors F64 profile.

The accepted wire format is the SafeTensors format at revision
@6eb4dc9a28ebce297606e0f4836bbf28839cacef@, restricted to F64 tensors and no
metadata. Decoding validates the complete file and allocation plan before the
first tensor allocation. Encoding materializes every view in logical
row-major order and emits deterministic compact JSON.
-}
module Markovian.Tensor.SafeTensors (
    SafeTensorLimits,
    safeTensorLimits,
    SafeTensorName,
    safeTensorName,
    safeTensorNameText,
    SafeTensorError,
    renderSafeTensorError,
    SafeTensorFile,
    safeTensorEntries,
    SomeHostTensor,
    someHostTensor,
    withSomeHostTensor,
    someHostTensorDimensions,
    someHostTensorToList,
    decodeSafeTensors,
    encodeSafeTensors,
) where

import Control.Monad (foldM, unless, when)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as BL
import Data.Char (chr, ord)
import Data.List (sortBy)
import Data.Maybe (isJust)
import Data.Word (Word64, Word8)
import GHC.Float (castDoubleToWord64, castWord64ToDouble)
import Markovian.Tensor (
    DType (F64),
    DynamicHostTensor,
    HostTensor,
    TensorSession,
    dynamicHostTensor,
    dynamicHostTensorDimensions,
    dynamicHostTensorToList,
    hostTensorBatchFromLists,
    withDynamicHostTensor,
 )
import Numeric.Natural (Natural)

-- Limits ----------------------------------------------------------------------

-- | File, header, tensor-count, name, rank, dimension, element, and payload limits.
data SafeTensorLimits = SafeTensorLimits
    { limitFileBytes :: !Natural
    , limitHeaderBytes :: !Natural
    , limitTensorCount :: !Natural
    , limitNameBytes :: !Natural
    , limitTensorRank :: !Natural
    , limitTensorDimension :: !Natural
    , limitTensorElements :: !Natural
    , limitTotalPayloadBytes :: !Natural
    }
    deriving (Eq, Show)

{- | Construct limits in file, header, tensor-count, name, rank, dimension,
per-tensor element, and total payload byte order.
-}
safeTensorLimits :: Natural -> Natural -> Natural -> Natural -> Natural -> Natural -> Natural -> Natural -> Either SafeTensorError SafeTensorLimits
safeTensorLimits fileBytes headerBytes tensors maximumNameBytes rank dimension elements payloadBytes
    | headerBytes > maximumPinnedHeaderBytes = Left (InvalidLimits "header bytes exceed the pinned format maximum")
    | headerBytes > fileBytes = Left (InvalidLimits "header bytes exceed file bytes")
    | payloadBytes > fileBytes = Left (InvalidLimits "payload bytes exceed file bytes")
    | fileBytes > machineMaximum = Left (InvalidLimits "file bytes exceed the machine ByteString index")
    | headerBytes > machineMaximum = Left (InvalidLimits "header bytes exceed the machine ByteString index")
    | tensors > machineMaximum = Left (InvalidLimits "tensor count exceeds the machine list index")
    | rank > machineMaximum = Left (InvalidLimits "rank exceeds the machine list index")
    | dimension > wireDimensionMaximum = Left (InvalidLimits "dimension exceeds the SafeTensors Word64 wire maximum")
    | otherwise = Right (SafeTensorLimits fileBytes headerBytes tensors maximumNameBytes rank dimension elements payloadBytes)
  where
    machineMaximum = fromIntegral (maxBound :: Int)
    maximumPinnedHeaderBytes = 100000000

-- Public opaque values --------------------------------------------------------

-- | A validated UTF-8 tensor name.
newtype SafeTensorName = SafeTensorName ByteString
    deriving (Eq, Ord)

instance Show SafeTensorName where
    show = show . safeTensorNameText

-- | Validate a Haskell string as a Unicode tensor name.
safeTensorName :: String -> Either SafeTensorError SafeTensorName
safeTensorName input = SafeTensorName . BS.pack <$> encodeUnicode input

-- | Recover the Unicode tensor name.
safeTensorNameText :: SafeTensorName -> String
safeTensorNameText (SafeTensorName bytes) = decodeUtf8 bytes

-- | One dynamically shaped raw host tensor. It carries no owner evidence.
newtype SomeHostTensor region = SomeHostTensor (DynamicHostTensor region)

type role SomeHostTensor nominal

-- | Wrap an existing raw F64 host tensor for encoding.
someHostTensor :: HostTensor region 'F64 shape -> SomeHostTensor region
someHostTensor = SomeHostTensor . dynamicHostTensor

-- | Eliminate a dynamically shaped decoded tensor.
withSomeHostTensor :: SomeHostTensor region -> (forall shape. HostTensor region 'F64 shape -> value) -> value
withSomeHostTensor (SomeHostTensor tensor) = withDynamicHostTensor tensor

-- | Runtime dimensions in outermost-to-innermost order.
someHostTensorDimensions :: SomeHostTensor region -> [Natural]
someHostTensorDimensions (SomeHostTensor tensor) = dynamicHostTensorDimensions tensor

-- | Values in logical contiguous row-major order.
someHostTensorToList :: SomeHostTensor region -> IO [Double]
someHostTensorToList (SomeHostTensor tensor) = dynamicHostTensorToList tensor

-- | A decoded collection sorted by UTF-8 name bytes.
newtype SafeTensorFile region = SafeTensorFile [(SafeTensorName, SomeHostTensor region)]

type role SafeTensorFile nominal

-- | Enumerate decoded names and tensors in canonical name order.
safeTensorEntries :: SafeTensorFile region -> [(SafeTensorName, SomeHostTensor region)]
safeTensorEntries (SafeTensorFile entries) = entries

-- Errors ----------------------------------------------------------------------

-- | Bounded profile rejection. Constructors are intentionally private.
data SafeTensorError
    = InvalidLimits !String
    | FileLimitExceeded !Natural !Natural
    | FileTooShort !Natural
    | HeaderLengthOverflow !Word64
    | HeaderLimitExceeded !Natural !Natural
    | HeaderTruncated !Natural !Natural
    | MalformedJSON !Natural !String
    | InvalidUTF8 !Natural
    | NameLimitExceeded !Natural !Natural
    | TensorCountExceeded !Natural !Natural
    | DuplicateTensorName !String
    | DuplicateDescriptorField !String !String
    | MissingDescriptorField !String !String
    | UnknownDescriptorField !String !String
    | UnsupportedMetadata
    | UnsupportedDType !String !String
    | RankLimitExceeded !String !Natural !Natural
    | DimensionLimitExceeded !String !Natural !Natural
    | ElementLimitExceeded !String !Natural !Natural
    | NumericOverflow !Natural
    | OffsetOrderInvalid !String !Natural !Natural
    | PayloadSizeMismatch !String !Natural !Natural
    | PayloadLimitExceeded !Natural !Natural
    | PayloadHole !Natural !Natural
    | PayloadOverlap !Natural !Natural
    | PayloadTruncated !Natural !Natural
    | TrailingBytes !Natural !Natural
    | TensorRuntimeFailure !String
    | InternalProfileFailure !String
    deriving (Eq, Show)

-- | Stable diagnostic rendering without addresses or timing.
renderSafeTensorError :: SafeTensorError -> String
renderSafeTensorError = show

-- Parsing ---------------------------------------------------------------------

data Cursor = Cursor !ByteString !Int

data ParsedTensor = ParsedTensor
    { parsedName :: !SafeTensorName
    , parsedShape :: ![Natural]
    , parsedStart :: !Natural
    , parsedEnd :: !Natural
    }
    deriving (Eq, Show)

data Descriptor = Descriptor
    { descriptorDType :: !(Maybe ByteString)
    , descriptorShape :: !(Maybe [Natural])
    , descriptorOffsets :: !(Maybe [Natural])
    }

emptyDescriptor :: Descriptor
emptyDescriptor = Descriptor Nothing Nothing Nothing

cursorPosition :: Cursor -> Natural
cursorPosition (Cursor _ position) = fromIntegral position

peekByte :: Cursor -> Maybe Word8
peekByte (Cursor bytes position)
    | position >= BS.length bytes = Nothing
    | otherwise = Just (BS.index bytes position)

advance :: Cursor -> Cursor
advance (Cursor bytes position) = Cursor bytes (position + 1)

skipWhitespace :: Cursor -> Cursor
skipWhitespace cursor = case peekByte cursor of
    Just byte | byte `elem` [0x20, 0x09, 0x0a, 0x0d] -> skipWhitespace (advance cursor)
    _ -> cursor

skipPadding :: Cursor -> Cursor
skipPadding cursor = case peekByte cursor of
    Just 0x20 -> skipPadding (advance cursor)
    _ -> cursor

malformed :: Cursor -> String -> Either SafeTensorError value
malformed cursor = Left . MalformedJSON (cursorPosition cursor)

expectByte :: Word8 -> Cursor -> Either SafeTensorError Cursor
expectByte expected cursor = case peekByte cursor of
    Just actual | actual == expected -> Right (advance cursor)
    _ -> malformed cursor ("expected byte " ++ show expected)

parseString :: Natural -> Cursor -> Either SafeTensorError (ByteString, Cursor)
parseString byteLimit cursor0 = do
    cursor <- expectByte 0x22 (skipWhitespace cursor0)
    go [] 0 cursor
  where
    go reversed count cursor = case peekByte cursor of
        Nothing -> malformed cursor "unterminated string"
        Just 0x22 -> do
            let bytes = BS.pack (reverse reversed)
            validateUtf8 (cursorPosition cursor0) bytes
            Right (bytes, advance cursor)
        Just byte
            | byte < 0x20 -> malformed cursor "unescaped control byte in string"
            | byte == 0x5c -> parseEscape reversed count (advance cursor)
            | otherwise -> appendBytes reversed count [byte] (advance cursor)

    parseEscape reversed count cursor = case peekByte cursor of
        Nothing -> malformed cursor "unterminated string escape"
        Just byte -> case byte of
            0x22 -> appendBytes reversed count [0x22] (advance cursor)
            0x5c -> appendBytes reversed count [0x5c] (advance cursor)
            0x2f -> appendBytes reversed count [0x2f] (advance cursor)
            0x62 -> appendBytes reversed count [0x08] (advance cursor)
            0x66 -> appendBytes reversed count [0x0c] (advance cursor)
            0x6e -> appendBytes reversed count [0x0a] (advance cursor)
            0x72 -> appendBytes reversed count [0x0d] (advance cursor)
            0x74 -> appendBytes reversed count [0x09] (advance cursor)
            0x75 -> do
                (first, afterFirst) <- parseHex4 (advance cursor)
                if first >= 0xd800 && first <= 0xdbff
                    then do
                        slash <- expectByte 0x5c afterFirst
                        marker <- expectByte 0x75 slash
                        (second, afterSecond) <- parseHex4 marker
                        if second < 0xdc00 || second > 0xdfff
                            then malformed afterFirst "invalid Unicode surrogate pair"
                            else appendCodePoint reversed count (0x10000 + (first - 0xd800) * 0x400 + second - 0xdc00) afterSecond
                    else
                        if first >= 0xdc00 && first <= 0xdfff
                            then malformed cursor "unpaired Unicode low surrogate"
                            else appendCodePoint reversed count first afterFirst
            _ -> malformed cursor "unknown string escape"

    parseHex4 cursor = foldM step (0, cursor) [1 :: Int .. 4]
      where
        step (value, current) _ = case peekByte current >>= hexValue of
            Nothing -> malformed current "invalid Unicode escape"
            Just nibble -> Right (value * 16 + nibble, advance current)

    appendCodePoint reversed count codePoint =
        appendBytes reversed count (utf8CodePoint codePoint)

    appendBytes reversed count bytes cursor =
        let next = count + fromIntegral (length bytes)
         in if next > byteLimit
                then Left (NameLimitExceeded byteLimit next)
                else go (reverse bytes ++ reversed) next cursor

hexValue :: Word8 -> Maybe Int
hexValue byte
    | byte >= 0x30 && byte <= 0x39 = Just (fromIntegral (byte - 0x30))
    | byte >= 0x41 && byte <= 0x46 = Just (fromIntegral (byte - 0x41 + 10))
    | byte >= 0x61 && byte <= 0x66 = Just (fromIntegral (byte - 0x61 + 10))
    | otherwise = Nothing

parseNatural :: Cursor -> Either SafeTensorError (Natural, Cursor)
parseNatural cursor0 = case peekByte cursor of
    Just byte
        | byte >= 0x30 && byte <= 0x39 ->
            if byte == 0x30
                then case peekByte (advance cursor) of
                    Just next | next >= 0x30 && next <= 0x39 -> malformed cursor "leading zero in number"
                    _ -> Right (0, advance cursor)
                else go 0 cursor
    _ -> malformed cursor "expected unsigned integer"
  where
    cursor = skipWhitespace cursor0
    maximumWord64 = fromIntegral (maxBound :: Word64)
    go accumulator current = case peekByte current of
        Just byte
            | byte >= 0x30 && byte <= 0x39 ->
                let digit = fromIntegral (byte - 0x30)
                 in if accumulator > (maximumWord64 - digit) `div` 10
                        then Left (NumericOverflow (cursorPosition current))
                        else go (accumulator * 10 + digit) (advance current)
        _ -> Right (accumulator, current)

parseNaturalArray :: Natural -> Cursor -> Either SafeTensorError ([Natural], Cursor)
parseNaturalArray maximumCount cursor0 = do
    opened <- expectByte 0x5b (skipWhitespace cursor0)
    let first = skipWhitespace opened
    case peekByte first of
        Just 0x5d -> Right ([], advance first)
        _ -> go [] 0 first
  where
    go reversed count cursor
        | count >= maximumCount = Left (RankLimitExceeded "shape" maximumCount (count + 1))
        | otherwise = do
            (value, afterValue) <- parseNatural cursor
            let next = skipWhitespace afterValue
            case peekByte next of
                Just 0x2c -> go (value : reversed) (count + 1) (skipWhitespace (advance next))
                Just 0x5d -> Right (reverse (value : reversed), advance next)
                _ -> malformed next "expected comma or array close"

parseDescriptor :: SafeTensorLimits -> SafeTensorName -> Cursor -> Either SafeTensorError (ParsedTensor, Cursor)
parseDescriptor limits name cursor0 = do
    opened <- expectByte 0x7b (skipWhitespace cursor0)
    (descriptor, closed) <- fields emptyDescriptor (skipWhitespace opened)
    dtype <- required "dtype" (descriptorDType descriptor)
    shape <- required "shape" (descriptorShape descriptor)
    offsets <- required "data_offsets" (descriptorOffsets descriptor)
    dtypeText <- decodeChecked dtype
    unless (dtypeText == "F64") (Left (UnsupportedDType (safeTensorNameText name) dtypeText))
    case offsets of
        [start, end] -> Right (ParsedTensor name shape start end, closed)
        _ -> malformed closed "data_offsets must contain exactly two integers"
  where
    label = safeTensorNameText name
    required :: String -> Maybe value -> Either SafeTensorError value
    required field = maybe (Left (MissingDescriptorField label field)) Right
    decodeChecked bytes = validateUtf8 0 bytes >> Right (decodeUtf8 bytes)

    fields descriptor cursor = case peekByte cursor of
        Just 0x7d -> Right (descriptor, advance cursor)
        _ -> do
            (keyBytes, afterKey) <- parseString 32 cursor
            validateUtf8 (cursorPosition cursor) keyBytes
            let key = decodeUtf8 keyBytes
            colon <- expectByte 0x3a (skipWhitespace afterKey)
            (nextDescriptor, afterValue) <- case key of
                "dtype" -> do
                    when (isJust (descriptorDType descriptor)) (Left (DuplicateDescriptorField label key))
                    (value, after) <- parseString 16 (skipWhitespace colon)
                    Right (descriptor{descriptorDType = Just value}, after)
                "shape" -> do
                    when (isJust (descriptorShape descriptor)) (Left (DuplicateDescriptorField label key))
                    (value, after) <- parseNaturalArray (limitTensorRank limits) (skipWhitespace colon)
                    Right (descriptor{descriptorShape = Just value}, after)
                "data_offsets" -> do
                    when (isJust (descriptorOffsets descriptor)) (Left (DuplicateDescriptorField label key))
                    (value, after) <- parseNaturalArray 2 (skipWhitespace colon)
                    Right (descriptor{descriptorOffsets = Just value}, after)
                _ -> Left (UnknownDescriptorField label key)
            let next = skipWhitespace afterValue
            case peekByte next of
                Just 0x2c ->
                    let afterComma = skipWhitespace (advance next)
                     in case peekByte afterComma of
                            Just 0x7d -> malformed afterComma "trailing comma in descriptor"
                            _ -> fields nextDescriptor afterComma
                Just 0x7d -> Right (nextDescriptor, advance next)
                _ -> malformed next "expected comma or object close"

parseHeader :: SafeTensorLimits -> ByteString -> Either SafeTensorError [ParsedTensor]
parseHeader limits bytes = do
    opened <- expectByte 0x7b (Cursor bytes 0)
    (entries, closed) <- case peekByte (skipWhitespace opened) of
        Just 0x7d -> Right ([], advance (skipWhitespace opened))
        _ -> fields [] 0 (skipWhitespace opened)
    let final = skipPadding closed
        sorted = sortBy compareParsedName entries
    checkDuplicateNames sorted
    case peekByte final of
        Nothing -> Right sorted
        Just _ -> malformed final "bytes after top-level object"
  where
    fields reversed count cursor = do
        when (count >= limitTensorCount limits) (Left (TensorCountExceeded (limitTensorCount limits) (count + 1)))
        (rawName, afterName) <- parseString (limitNameBytes limits) cursor
        let name = SafeTensorName rawName
        when (rawName == BS.pack (map (fromIntegral . ord) "__metadata__")) (Left UnsupportedMetadata)
        colon <- expectByte 0x3a (skipWhitespace afterName)
        (entry, afterEntry) <- parseDescriptor limits name (skipWhitespace colon)
        let next = skipWhitespace afterEntry
        case peekByte next of
            Just 0x2c -> fields (entry : reversed) (count + 1) (skipWhitespace (advance next))
            Just 0x7d -> Right (reverse (entry : reversed), advance next)
            _ -> malformed next "expected comma or top-level object close"

    checkDuplicateNames (left : right : rest)
        | parsedName left == parsedName right = Left (DuplicateTensorName (safeTensorNameText (parsedName left)))
        | otherwise = checkDuplicateNames (right : rest)
    checkDuplicateNames _ = Right ()

compareParsedName :: ParsedTensor -> ParsedTensor -> Ordering
compareParsedName left right = compare (nameBytes (parsedName left)) (nameBytes (parsedName right))

nameBytes :: SafeTensorName -> ByteString
nameBytes (SafeTensorName bytes) = bytes

-- Validation ------------------------------------------------------------------

validateParsed :: SafeTensorLimits -> Natural -> [ParsedTensor] -> Either SafeTensorError [ParsedTensor]
validateParsed limits available entries = do
    checked <- traverse checkEntry entries
    let byOffset = sortBy compareOffset checked
    declared <- checkCoverage 0 byOffset
    when (declared > available) (Left (PayloadTruncated declared available))
    when (declared < available) (Left (TrailingBytes declared available))
    when (declared > limitTotalPayloadBytes limits) (Left (PayloadLimitExceeded (limitTotalPayloadBytes limits) declared))
    Right checked
  where
    checkEntry entry = do
        let label = safeTensorNameText (parsedName entry)
            dimensions = parsedShape entry
            rank = fromIntegral (length dimensions)
        when (rank > limitTensorRank limits) (Left (RankLimitExceeded label (limitTensorRank limits) rank))
        mapM_ (\dimension -> when (dimension > limitTensorDimension limits) (Left (DimensionLimitExceeded label (limitTensorDimension limits) dimension))) dimensions
        elements <- cappedProductFor label (limitTensorElements limits) dimensions
        bytes <- cappedMultiplyFor (limitTotalPayloadBytes limits) elements 8
        when (parsedEnd entry < parsedStart entry) (Left (OffsetOrderInvalid label (parsedStart entry) (parsedEnd entry)))
        let represented = parsedEnd entry - parsedStart entry
        when (represented /= bytes) (Left (PayloadSizeMismatch label bytes represented))
        Right entry

    compareOffset left right = compare (parsedStart left, parsedEnd left, nameBytes (parsedName left)) (parsedStart right, parsedEnd right, nameBytes (parsedName right))

    checkCoverage cursor [] = Right cursor
    checkCoverage cursor (entry : rest)
        | parsedStart entry > cursor = Left (PayloadHole cursor (parsedStart entry))
        | parsedStart entry < cursor = Left (PayloadOverlap (parsedStart entry) cursor)
        | otherwise = checkCoverage (parsedEnd entry) rest

cappedProductFor :: String -> Natural -> [Natural] -> Either SafeTensorError Natural
cappedProductFor label maximumAllowed values
    | 0 `elem` values = Right 0
    | otherwise = go 1 values
  where
    go accumulator [] = Right accumulator
    go accumulator (value : rest)
        | accumulator > maximumAllowed `div` value = Left (ElementLimitExceeded label maximumAllowed (maximumAllowed + 1))
        | otherwise = go (accumulator * value) rest

cappedMultiplyFor :: Natural -> Natural -> Natural -> Either SafeTensorError Natural
cappedMultiplyFor maximumAllowed left right
    | left == 0 || right == 0 = Right 0
    | left > maximumAllowed `div` right = Left (PayloadLimitExceeded maximumAllowed (maximumAllowed + 1))
    | otherwise = Right (left * right)

-- Decode ----------------------------------------------------------------------

-- | Decode a completely validated F64 file into one region-owned collection.
decodeSafeTensors :: TensorSession region -> SafeTensorLimits -> ByteString -> IO (Either SafeTensorError (SafeTensorFile region))
decodeSafeTensors session limits bytes = case decodePlan limits bytes of
    Left problem -> pure (Left problem)
    Right (entries, payload) -> do
        let requests = map (\entry -> (parsedShape entry, decodePayload entry payload)) entries
        allocated <- hostTensorBatchFromLists session requests
        pure $ case allocated of
            Left problem -> Left (TensorRuntimeFailure (show problem))
            Right (tensors, _) ->
                if length tensors /= length entries
                    then Left (InternalProfileFailure "tensor allocation result count changed")
                    else Right (SafeTensorFile (zip (map parsedName entries) (map SomeHostTensor tensors)))

decodePlan :: SafeTensorLimits -> ByteString -> Either SafeTensorError ([ParsedTensor], ByteString)
decodePlan limits bytes = do
    let total = fromIntegral (BS.length bytes)
    when (total > limitFileBytes limits) (Left (FileLimitExceeded (limitFileBytes limits) total))
    when (total < 8) (Left (FileTooShort total))
    let headerWord = word64At bytes 0
        machineMaximum = fromIntegral (maxBound :: Int) :: Word64
    when (headerWord > machineMaximum) (Left (HeaderLengthOverflow headerWord))
    let headerLength = fromIntegral headerWord
    when (headerLength > limitHeaderBytes limits) (Left (HeaderLimitExceeded (limitHeaderBytes limits) headerLength))
    when (headerLength > total - 8) (Left (HeaderTruncated headerLength (total - 8)))
    let header = BS.take (fromIntegral headerLength) (BS.drop 8 bytes)
        payload = BS.drop (8 + fromIntegral headerLength) bytes
        payloadLength = fromIntegral (BS.length payload)
    entries <- parseHeader limits header
    validated <- validateParsed limits payloadLength entries
    Right (validated, payload)

decodePayload :: ParsedTensor -> ByteString -> [Double]
decodePayload entry payload =
    let start = fromIntegral (parsedStart entry)
        count = fromIntegral ((parsedEnd entry - parsedStart entry) `div` 8)
        bytes = BS.drop start payload
     in map (castWord64ToDouble . word64At bytes . (* 8)) [0 .. count - 1]

-- Encode ----------------------------------------------------------------------

data EncodePlan region = EncodePlan !SafeTensorName !(SomeHostTensor region) ![Natural] !Natural !Natural

-- | Encode names and raw host tensors as canonical compact F64 SafeTensors bytes.
encodeSafeTensors :: SafeTensorLimits -> [(SafeTensorName, SomeHostTensor region)] -> IO (Either SafeTensorError ByteString)
encodeSafeTensors limits requested = case encodePlan limits requested of
    Left problem -> pure (Left problem)
    Right (plans, header, payloadBytes) -> do
        payloads <- traverse readPlan plans
        pure $ do
            values <- sequence payloads
            let payload = BL.toStrict (Builder.toLazyByteString (foldMap encodeValues values))
            unless (fromIntegral (BS.length payload) == payloadBytes) (Left (InternalProfileFailure "materialized payload size changed"))
            let headerLength = fromIntegral (BS.length header) :: Word64
                result = BL.toStrict (Builder.toLazyByteString (Builder.word64LE headerLength <> Builder.byteString header <> Builder.byteString payload))
            Right result
  where
    readPlan (EncodePlan name tensor _ start end) = do
        values <- someHostTensorToList tensor
        let expected = fromIntegral ((end - start) `div` 8)
        pure $ if length values == expected then Right values else Left (InternalProfileFailure ("logical element count changed for " ++ safeTensorNameText name))

encodePlan :: SafeTensorLimits -> [(SafeTensorName, SomeHostTensor region)] -> Either SafeTensorError ([EncodePlan region], ByteString, Natural)
encodePlan limits requested = do
    inputs <- boundedTensors (limitTensorCount limits) requested
    let sorted = sortBy (\left right -> compare (nameBytes (fst left)) (nameBytes (fst right))) inputs
    checkNames sorted
    (reversedPlans, payloadBytes) <- foldM addPlan ([], 0) sorted
    let plans = reverse reversedPlans
        rawHeaderLength = encodedHeaderLength plans
        paddingLength = (8 - rawHeaderLength `mod` 8) `mod` 8
        headerLength = rawHeaderLength + paddingLength
        fileLength = 8 + headerLength + payloadBytes
    when (headerLength > limitHeaderBytes limits) (Left (HeaderLimitExceeded (limitHeaderBytes limits) headerLength))
    when (payloadBytes > limitTotalPayloadBytes limits) (Left (PayloadLimitExceeded (limitTotalPayloadBytes limits) payloadBytes))
    when (fileLength > limitFileBytes limits) (Left (FileLimitExceeded (limitFileBytes limits) fileLength))
    let rawHeader = BL.toStrict (Builder.toLazyByteString (encodeHeader plans))
    unless (fromIntegral (BS.length rawHeader) == rawHeaderLength) (Left (InternalProfileFailure "canonical header length changed"))
    let header = rawHeader <> BS.replicate (fromIntegral paddingLength) 0x20
    Right (plans, header, payloadBytes)
  where
    checkNames [] = Right ()
    checkNames ((name, _) : rest) = do
        let size = fromIntegral (BS.length (nameBytes name))
        when (size > limitNameBytes limits) (Left (NameLimitExceeded (limitNameBytes limits) size))
        when (nameBytes name == BS.pack (map (fromIntegral . ord) "__metadata__")) (Left UnsupportedMetadata)
        case rest of
            (next, _) : _ | name == next -> Left (DuplicateTensorName (safeTensorNameText name))
            _ -> checkNames rest

    addPlan (reversed, offset) (name, tensor) = do
        let dimensions = someHostTensorDimensions tensor
            label = safeTensorNameText name
            rank = fromIntegral (length dimensions)
        when (rank > limitTensorRank limits) (Left (RankLimitExceeded label (limitTensorRank limits) rank))
        mapM_ (\dimension -> when (dimension > wireDimensionMaximum) (Left (DimensionLimitExceeded label wireDimensionMaximum dimension))) dimensions
        mapM_ (\dimension -> when (dimension > limitTensorDimension limits) (Left (DimensionLimitExceeded label (limitTensorDimension limits) dimension))) dimensions
        elements <- cappedProductFor label (limitTensorElements limits) dimensions
        bytes <- cappedMultiplyFor (limitTotalPayloadBytes limits) elements 8
        when (offset > limitTotalPayloadBytes limits - bytes) (Left (PayloadLimitExceeded (limitTotalPayloadBytes limits) (limitTotalPayloadBytes limits + 1)))
        let end = offset + bytes
        Right (EncodePlan name tensor dimensions offset end : reversed, end)

wireDimensionMaximum :: Natural
wireDimensionMaximum = fromIntegral (maxBound :: Word64)

boundedTensors :: Natural -> [value] -> Either SafeTensorError [value]
boundedTensors maximumCount = go 0 []
  where
    go _ reversed [] = Right (reverse reversed)
    go seen _ (_ : _) | seen >= maximumCount = Left (TensorCountExceeded maximumCount (seen + 1))
    go seen reversed (value : rest) = go (seen + 1) (value : reversed) rest

encodedHeaderLength :: [EncodePlan region] -> Natural
encodedHeaderLength plans = 2 + separators + sum (map encodedEntryLength plans)
  where
    separators
        | null plans = 0
        | otherwise = fromIntegral (length plans - 1)
    encodedEntryLength (EncodePlan name _ dimensions start end) =
        encodedJSONStringLength (nameBytes name)
            + asciiLength ":{\"dtype\":\"F64\",\"shape\":["
            + separatedLength (map (asciiLength . show) dimensions)
            + asciiLength "],\"data_offsets\":["
            + asciiLength (show start)
            + 1
            + asciiLength (show end)
            + asciiLength "]}"

encodedJSONStringLength :: ByteString -> Natural
encodedJSONStringLength bytes = 2 + BS.foldl' (\total byte -> total + encodedByteLength byte) 0 bytes
  where
    encodedByteLength byte
        | byte == 0x22 || byte == 0x5c = 2
        | byte < 0x20 = 6
        | otherwise = 1

separatedLength :: [Natural] -> Natural
separatedLength [] = 0
separatedLength values = sum values + fromIntegral (length values - 1)

asciiLength :: String -> Natural
asciiLength = fromIntegral . length

encodeHeader :: [EncodePlan region] -> Builder.Builder
encodeHeader plans = Builder.word8 0x7b <> separated (map encodeEntry plans) <> Builder.word8 0x7d
  where
    encodeEntry (EncodePlan name _ dimensions start end) =
        encodeJSONString (nameBytes name)
            <> ascii ":{\"dtype\":\"F64\",\"shape\":["
            <> separated (map (ascii . show) dimensions)
            <> ascii "],\"data_offsets\":["
            <> ascii (show start)
            <> Builder.word8 0x2c
            <> ascii (show end)
            <> ascii "]}"

separated :: [Builder.Builder] -> Builder.Builder
separated [] = mempty
separated (value : values) = value <> foldMap (Builder.word8 0x2c <>) values

ascii :: String -> Builder.Builder
ascii = foldMap (Builder.word8 . fromIntegral . ord)

encodeJSONString :: ByteString -> Builder.Builder
encodeJSONString bytes = Builder.word8 0x22 <> BS.foldl' (\builder byte -> builder <> encodeJSONByte byte) mempty bytes <> Builder.word8 0x22

encodeJSONByte :: Word8 -> Builder.Builder
encodeJSONByte byte = case byte of
    0x22 -> ascii "\\\""
    0x5c -> ascii "\\\\"
    _ | byte < 0x20 -> ascii ("\\u00" ++ [hexDigit (byte `shiftR` 4), hexDigit byte])
    _ -> Builder.word8 byte
  where
    hexDigit value = "0123456789abcdef" !! fromIntegral (value .&. 0x0f)

encodeValues :: [Double] -> Builder.Builder
encodeValues = foldMap (Builder.word64LE . castDoubleToWord64)

-- UTF-8 and words -------------------------------------------------------------

encodeUnicode :: String -> Either SafeTensorError [Word8]
encodeUnicode = fmap concat . traverse encodeCharacter
  where
    encodeCharacter character =
        let codePoint = ord character
         in if (codePoint >= 0xd800 && codePoint <= 0xdfff) || codePoint > 0x10ffff
                then Left (InvalidUTF8 0)
                else Right (utf8CodePoint codePoint)

utf8CodePoint :: Int -> [Word8]
utf8CodePoint codePoint
    | codePoint <= 0x7f = [fromIntegral codePoint]
    | codePoint <= 0x7ff =
        [ fromIntegral (0xc0 .|. (codePoint `shiftR` 6))
        , fromIntegral (0x80 .|. (codePoint .&. 0x3f))
        ]
    | codePoint <= 0xffff =
        [ fromIntegral (0xe0 .|. (codePoint `shiftR` 12))
        , fromIntegral (0x80 .|. ((codePoint `shiftR` 6) .&. 0x3f))
        , fromIntegral (0x80 .|. (codePoint .&. 0x3f))
        ]
    | otherwise =
        [ fromIntegral (0xf0 .|. (codePoint `shiftR` 18))
        , fromIntegral (0x80 .|. ((codePoint `shiftR` 12) .&. 0x3f))
        , fromIntegral (0x80 .|. ((codePoint `shiftR` 6) .&. 0x3f))
        , fromIntegral (0x80 .|. (codePoint .&. 0x3f))
        ]

validateUtf8 :: Natural -> ByteString -> Either SafeTensorError ()
validateUtf8 origin bytes = go 0
  where
    total = BS.length bytes
    at = BS.index bytes
    continuation byte = byte >= 0x80 && byte <= 0xbf
    invalid index = Left (InvalidUTF8 (origin + fromIntegral index))
    need index count = index + count < total

    go index
        | index >= total = Right ()
        | byte <= 0x7f = go (index + 1)
        | byte >= 0xc2 && byte <= 0xdf =
            if need index 1 && continuation (at (index + 1)) then go (index + 2) else invalid index
        | byte == 0xe0 =
            if need index 2 && at (index + 1) >= 0xa0 && at (index + 1) <= 0xbf && continuation (at (index + 2)) then go (index + 3) else invalid index
        | (byte >= 0xe1 && byte <= 0xec) || (byte >= 0xee && byte <= 0xef) =
            if need index 2 && continuation (at (index + 1)) && continuation (at (index + 2)) then go (index + 3) else invalid index
        | byte == 0xed =
            if need index 2 && at (index + 1) >= 0x80 && at (index + 1) <= 0x9f && continuation (at (index + 2)) then go (index + 3) else invalid index
        | byte == 0xf0 =
            if need index 3 && at (index + 1) >= 0x90 && at (index + 1) <= 0xbf && continuation (at (index + 2)) && continuation (at (index + 3)) then go (index + 4) else invalid index
        | byte >= 0xf1 && byte <= 0xf3 =
            if need index 3 && continuation (at (index + 1)) && continuation (at (index + 2)) && continuation (at (index + 3)) then go (index + 4) else invalid index
        | byte == 0xf4 =
            if need index 3 && at (index + 1) >= 0x80 && at (index + 1) <= 0x8f && continuation (at (index + 2)) && continuation (at (index + 3)) then go (index + 4) else invalid index
        | otherwise = invalid index
      where
        byte = at index

decodeUtf8 :: ByteString -> String
decodeUtf8 bytes = go 0
  where
    total = BS.length bytes
    at = fromIntegral . BS.index bytes
    go index
        | index >= total = []
        | first <= 0x7f = chr first : go (index + 1)
        | first <= 0xdf = chr (((first .&. 0x1f) `shiftL` 6) .|. (at (index + 1) .&. 0x3f)) : go (index + 2)
        | first <= 0xef = chr (((first .&. 0x0f) `shiftL` 12) .|. ((at (index + 1) .&. 0x3f) `shiftL` 6) .|. (at (index + 2) .&. 0x3f)) : go (index + 3)
        | otherwise = chr (((first .&. 0x07) `shiftL` 18) .|. ((at (index + 1) .&. 0x3f) `shiftL` 12) .|. ((at (index + 2) .&. 0x3f) `shiftL` 6) .|. (at (index + 3) .&. 0x3f)) : go (index + 4)
      where
        first = at index

word64At :: ByteString -> Int -> Word64
word64At bytes offset = foldr step 0 [0 .. 7]
  where
    step index accumulator = accumulator .|. (fromIntegral (BS.index bytes (offset + index)) `shiftL` (8 * index))
