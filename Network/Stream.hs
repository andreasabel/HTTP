{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Stream
-- Copyright   :  See LICENSE file
-- License     :  BSD
--
-- Maintainer  :  Ganesh Sittampalam <ganesh@earth.li>
-- Stability   :  experimental
-- Portability :  non-portable (not tested)
--
-- An library for creating abstract streams. Originally part of Gray's\/Bringert's
-- HTTP module.
--
-- * Changes by Robin Bate Boerop <robin@bateboerop.name>:
--      - Removed unnecessary import statements.
--      - Moved Debug code to StreamDebugger.hs
--      - Moved Socket-related code to StreamSocket.hs.
--
-- * Changes by Simon Foster:
--      - Split Network.HTTPmodule up into to separate
--        Network.[Stream,TCP,HTTP] modules
-----------------------------------------------------------------------------
module Network.Stream
   ( Stream(..)
   , ConnError(..)
   , Result
   , bindE
   , fmapE

   , failParse -- :: String -> Result a
   , failWith  -- :: ConnError -> Result a
   , failMisc  -- :: String -> Result a
#if MIN_VERSION_base(4,4,0)
   , module Data.Either
#endif
   ) where

#if MIN_VERSION_base(4,4,0)
import Data.Either()  -- instance Monad (Either e)
#endif

data ConnError
 = ErrorReset
 | ErrorClosed
 | ErrorParse String
 | ErrorMisc String
   deriving(Show,Eq)

-- in GHC 7.0 the Monad instance for Error no longer
-- uses fail x = Left (strMsg x). failMisc is therefore
-- used instead.
failMisc :: String -> Result a
failMisc x = failWith (ErrorMisc x)

failParse :: String -> Result a
failParse x = failWith (ErrorParse x)

failWith :: ConnError -> Result a
failWith x = Left x

bindE :: Result a -> (a -> Result b) -> Result b
bindE (Left e)  _ = Left e
bindE (Right v) f = f v

fmapE :: (a -> Result b) -> IO (Result a) -> IO (Result b)
fmapE f a = do
 x <- a
 case x of
   Left  e -> return (Left e)
   Right r -> return (f r)

-- | This is the type returned by many exported network functions.
type Result a = Either ConnError   {- error  -}
                       a           {- result -}

-- @instance Monad Result@ follows from this instance, added in base-4.4:
#if !MIN_VERSION_base(4,4,0)
instance Monad (Either e) where
  Left e  >>= k = Left e
  Right a >>= k = k a
  return = Right
#endif

-- | Streams should make layering of TLS protocol easier in future,
-- they allow reading/writing to files etc for debugging,
-- they allow use of protocols other than TCP/IP
-- and they allow customisation.
--
-- Instances of this class should not trim
-- the input in any way, e.g. leave LF on line
-- endings etc. Unless that is exactly the behaviour
-- you want from your twisted instances ;)
class Stream x where
    readLine   :: x -> IO (Result String)
    readBlock  :: x -> Int -> IO (Result String)
    writeBlock :: x -> String -> IO (Result ())
    close      :: x -> IO ()
    closeOnEnd :: x -> Bool -> IO ()
      -- ^ True => shutdown the connection when response has been read / end-of-stream
      --           has been reached.
