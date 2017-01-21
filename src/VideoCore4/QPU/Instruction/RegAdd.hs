module VideoCore4.QPU.Instruction.RegAdd
       (
         RegAdd
       , reg_Off
       , reg_On
       ) where

import Data.Typeable
import Data.Word
import VideoCore4.QPU.Instruction.Types

newtype RegAdd = RegAdd { unRegAdd :: Word8 } deriving (Eq, Show, Typeable)

instance To64 RegAdd where
  to64 = toEnum . fromEnum . unRegAdd

reg_Off :: RegAdd
reg_Off = RegAdd 0
reg_On :: RegAdd
reg_On = RegAdd 1
