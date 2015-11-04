
module Database.PostgreSQL.Printer
       ( Printer, execPrinter
       , hostAddress
       , v6HostAddress
       , netAddress
       ) where

import Numeric (showInt, showHex)

import Text.Printer.List (token, list, execPrinter)
import qualified Text.Printer.List as P
import Data.PostgreSQL.NetworkAddress
  (HostAddress, hostAddressOctets, V6HostAddress, v6HostAddressWords, NetAddress (..))


type Printer a = P.Printer Char a
type PrintM = P.PrintM Char


mapShowS :: (a -> ShowS) -> Printer a
mapShowS s = list . ($ []) . s

dec :: (Integral a, Show a) => Printer a
dec = mapShowS showInt

hex :: (Integral a, Show a) => Printer a
hex = mapShowS showHex

dot :: PrintM ()
dot = token '.'

colon :: PrintM ()
colon = token ':'

slash :: PrintM ()
slash = token '/'

hostAddress :: Printer HostAddress
hostAddress ha = do
  let (a, b, c, d) = hostAddressOctets ha
  dec a
  dot
  dec b
  dot
  dec c
  dot
  dec d

v6HostAddress :: Printer V6HostAddress
v6HostAddress ha = do
  let (a, b, c, d, e, f, g, h) = v6HostAddressWords ha
  hex a
  colon
  hex b
  colon
  hex c
  colon
  hex d
  colon
  hex e
  colon
  hex f
  colon
  hex g
  colon
  hex h

netAddress :: Printer NetAddress
netAddress = d  where
  d (NetAddress4 ha m) = do
    hostAddress ha
    slash
    dec m
  d (NetAddress6 v6 m) = do
    v6HostAddress v6
    slash
    dec m
