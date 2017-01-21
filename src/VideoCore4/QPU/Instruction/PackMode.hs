module VideoCore4.QPU.Instruction.PackMode
       (
         PackMode
       , pack_32_32
       , pack_32_16A
       , pack_32_16B
       , pack_32_8888
       , pack_32_8A
       , pack_32_8B
       , pack_32_8C
       , pack_32_8D
       , pack_32_32S
       , pack_32_16AS
       , pack_32_16BS
       , pack_32_8888S
       , pack_32_8AS
       , pack_32_8BS
       , pack_32_8CS
       , pack_32_8CD
       ) where

import Data.Typeable
import Data.Word
import VideoCore4.QPU.Instruction.Types

newtype PackMode = PackMode { unPackMode :: Word8 } deriving (Eq, Show, Typeable)

instance To64 PackMode where
  to64 = toEnum . fromEnum . unPackMode

pack_32_32 :: PackMode
pack_32_32 = PackMode 0
pack_32_16A :: PackMode
pack_32_16A = PackMode 1
pack_32_16B :: PackMode
pack_32_16B = PackMode 2
pack_32_8888 :: PackMode
pack_32_8888 = PackMode 3
pack_32_8A :: PackMode
pack_32_8A = PackMode 4
pack_32_8B :: PackMode
pack_32_8B = PackMode 5
pack_32_8C :: PackMode
pack_32_8C = PackMode 6
pack_32_8D :: PackMode
pack_32_8D = PackMode 7
pack_32_32S :: PackMode
pack_32_32S = PackMode 8
pack_32_16AS :: PackMode
pack_32_16AS = PackMode 9
pack_32_16BS :: PackMode
pack_32_16BS = PackMode 10
pack_32_8888S :: PackMode
pack_32_8888S = PackMode 11
pack_32_8AS :: PackMode
pack_32_8AS = PackMode 12
pack_32_8BS :: PackMode
pack_32_8BS = PackMode 13
pack_32_8CS :: PackMode
pack_32_8CS = PackMode 14
pack_32_8CD :: PackMode
pack_32_8CD = PackMode 15
