{-# LANGUAGE OverloadedStrings #-}
-- | This module implements the two algorithms from RFC 3490. (<http://tools.ietf.org/html/rfc3490>)
module Text.IDNA (acePrefix, toASCII, toUnicode)
where

import Text.StringPrep
import Text.StringPrep.Profiles
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Punycode as Puny
import Data.Text.Encoding as E

-- | The ASCII Compatible Encoding prefix (currently \'@xn--@\').
acePrefix :: Text
acePrefix = "xn--"

-- | Implements the ToASCII algorithm.
toASCII :: Bool -- ^ Whether to allow unassigned code points (in RFC: AllowUnassigned).
        -> Bool -- ^ Whether to disallow certain ASCII characters (in RFC: UseSTD3ASCIIRules).
        -> Text -- ^ The text to transform.
        -> Maybe Text
toASCII allowUnassigned useSTD3ASCIIRules t = do
                let profile = (namePrepProfile allowUnassigned) { maps = [] }
                step1 <- if Text.null t
                        then Nothing
                        else return t

                step2 <- if Text.any (>'\x7f') step1
                        then runStringPrep profile step1
                        else return step1

                step3 <- if (useSTD3ASCIIRules &&
                             (Text.any nonLDHascii step2 ||
                              Text.head step2 == '-' ||
                              Text.last step2 == '-'))
                        then Nothing
                        else return step2

                step7 <- if (Text.any (>'\x7f') step2)
                                then if acePrefix `Text.isPrefixOf` step3
                                        then Nothing
                                        else case return (Puny.encode step3) of -- TODO: this can fail?
                                                Left _ -> Nothing
                                                Right t' -> return $ acePrefix `Text.append` E.decodeUtf8 t'
                                else return step3

                if Text.length step7 <= 63
                        then return step7
                        else Nothing

nonLDHascii :: Char -> Bool
nonLDHascii c =
        '\x0' <= c && c <= '\x2c' ||
        '\x2e' <= c && c <= '\x2f' ||
        '\x3a' <= c && c <= '\x40' ||
        '\x5b' <= c && c <= '\x60' ||
        '\x7b' <= c && c <= '\x7f'

toUnicode :: Bool -- ^ Whether to allow unassigned code points (in RFC: AllowUnassigned).
        -> Bool -- ^ Whether to disallow certain ASCII characters (in RFC: UseSTD3ASCIIRules).
        -> Text -- ^ The text to transform.
        -> Text
toUnicode allowUnassigned useSTD3ASCIIRules t = mergeEither $ do
        step2 <- if Text.any (>'\x7f') t
                then case runStringPrep (namePrepProfile allowUnassigned) t of
                        Nothing -> Left t
                        Just t' -> return t'
                else return t

        step3 <- if not $ acePrefix `Text.isPrefixOf` step2
                then Left step2
                else return step2

        let step4 = Text.drop (Text.length acePrefix) step3
        step5 <- case Puny.decode $ E.encodeUtf8 step4 of
                Left _ -> Left step3
                Right s -> return s

        case toASCII allowUnassigned useSTD3ASCIIRules step5 of
                Nothing -> return step3
                Just t' -> if t' == step3
                        then return step5
                        else return step3

mergeEither :: Either a a -> a
mergeEither (Left x) = x
mergeEither (Right y) = y
