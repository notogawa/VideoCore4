module VideoCore4.QPU.Instruction.SetFlag
       (
         SetFlag
       , sf_Off
       , sf_On
       ) where

import Data.Typeable
import Data.Word
import VideoCore4.QPU.Instruction.Types

newtype SetFlag = SetFlag { unSetFlag :: Word8 } deriving (Eq, Show, Typeable)

instance To64 SetFlag where
  to64 = toEnum . fromEnum . unSetFlag

sf_Off :: SetFlag
sf_Off = SetFlag 0
sf_On :: SetFlag
sf_On = SetFlag 1
