module VideoCore4.QPU.Instruction.UnpackMode
       (
         UnpackMode
       , unpack_32_32
       , unpack_16A_32
       , unpack_16B_32
       , unpack_8D_8888
       , unpack_8A_32
       , unpack_8B_32
       , unpack_8C_32
       , unpack_8D_32
       ) where

import Data.Typeable
import Data.Word
import VideoCore4.QPU.Instruction.Types

newtype UnpackMode = UnpackMode { unUnpackMode :: Word8 } deriving (Eq, Show, Typeable)

instance To64 UnpackMode where
  to64 = toEnum . fromEnum . unUnpackMode

unpack_32_32 :: UnpackMode
unpack_32_32 = UnpackMode 0

unpack_16A_32 :: UnpackMode
unpack_16A_32 = UnpackMode 1

unpack_16B_32 :: UnpackMode
unpack_16B_32 = UnpackMode 2

unpack_8D_8888 :: UnpackMode
unpack_8D_8888 = UnpackMode 3

unpack_8A_32 :: UnpackMode
unpack_8A_32 = UnpackMode 4

unpack_8B_32 :: UnpackMode
unpack_8B_32 = UnpackMode 5

unpack_8C_32 :: UnpackMode
unpack_8C_32 = UnpackMode 6

unpack_8D_32 :: UnpackMode
unpack_8D_32 = UnpackMode 7
