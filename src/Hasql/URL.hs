-- |
-- Module      :  Hasql.URL
-- Copyright   :  2020 © Kanso, Théophile Batoz
-- License     :  MIT (see the file LICENSE)
--
-- Maintainer  :  Théophile Batoz
module Hasql.URL
  ( parseDatabaseUrl,
    uriToConnectInfo,
  )
where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.List.Split (splitOn, splitOneOf)
import Data.String (IsString (fromString))
import Data.Word (Word16)
import Debug.Trace (traceShowId)
import Hasql.Connection (Settings, settings)
import Network.URI (URI (uriAuthority, uriPath, uriScheme), URIAuth (URIAuth, uriPort, uriRegName, uriUserInfo), parseURI)
import Prelude

-- | Parse string url into `Settings`.
-- A password is required (because 'settings' in "Hasql.Connection" does).
-- The input string should follow this pattern:
-- @postgresql://[user[:password]@][netloc][:port][/dbname][?param1=value1&...]@
--
-- >>> parseDatabaseUrl "postgres://foo:bar@example.com:2345/database"
-- Just "host=example.com port=2345 user=foo password=bar dbname=database"
--
-- >>> parseDatabaseUrl "postgresql://foo:bar@example.com:2345/database"
-- Just "host=example.com port=2345 user=foo password=bar dbname=database"
--
-- >>> parseDatabaseUrl "postgres://my_user:p@localhost:5432/my_test_db"
-- Just "host=localhost port=5432 user=my_user password=p dbname=my_test_db"
--
parseDatabaseUrl :: String -> Maybe Settings
parseDatabaseUrl databaseUrl = parseURI databaseUrl >>= uriToConnectInfo

-- | Same as 'parseDatabaseUrl' but from a 'URI'.
uriToConnectInfo :: URI -> Maybe Settings
uriToConnectInfo uri
  | scheme /= "postgres:" && scheme /= "postgresql:" = Nothing
  | otherwise = do
    uriAuthValue <- uriAuthority uri
    (user, password) <- parseURIUserInfo uriAuthValue
    database <- parseUriPath $ uriPath uri
    port <- case splitOn ":" $ uriPort uriAuthValue of
      [_, portNumber] -> Just (read portNumber)
      _ -> Nothing
    let host = fromString $ uriRegName uriAuthValue
    pure $ settings host port user password database
  where
    scheme = uriScheme uri

-- | We require a password here because the "Hasql" 'settings' function does.
-- It's not the case in many other DB libs.
-- 'URIAuth' contains [user[:password]@][netloc][:port], it's the 'uriAuthority' of 'URI'
--
-- >>> let input = URIAuth{ uriUserInfo="foo:bar@", uriRegName="www.haskell.com", uriPort=":123" }
-- >>> parseURIUserInfo input
-- Just ("foo","bar")
--
-- This on the other hand does not work because there is no password:
-- >>> let input = URIAuth{ uriUserInfo="foo", uriRegName="www.haskell.com", uriPort=":123" }
-- >>> parseURIUserInfo input
-- Nothing
parseURIUserInfo :: URIAuth -> Maybe (ByteString, ByteString)
parseURIUserInfo input = case fromString <$> (splitOneOf ":@" $ uriUserInfo input) of
  [user, password, _] -> Just (user, password)
  _ -> Nothing

-- [/dbname] <=> 'uriPath'
parseUriPath :: String -> Maybe ByteString
parseUriPath uriPathValue = case splitOn "/" uriPathValue of
  [_, prefixRemoved] -> Just $ fromString prefixRemoved
  _ -> Nothing
-- [?param1=value1&...] <=> 'uriPath'
-- Not supported in Hasql ?
-- parseUriQuery :: String -> (String, String, String, Word16)
-- parseUriQuery = undefined
