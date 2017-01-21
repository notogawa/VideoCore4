module VideoCore4.QPU.Instruction.SemaphoreAccess
       (
         SemaphoreAccess
       , sa_Inc
       , sa_Dec
       ) where

import Data.Typeable
import Data.Word
import VideoCore4.QPU.Instruction.Types

newtype SemaphoreAccess = SemaphoreAccess { unSemaphoreAccess :: Word8 } deriving (Eq, Show, Typeable)

instance To64 SemaphoreAccess where
  to64 = toEnum . fromEnum . unSemaphoreAccess

sa_Inc :: SemaphoreAccess
sa_Inc = SemaphoreAccess 0
sa_Dec :: SemaphoreAccess
sa_Dec = SemaphoreAccess 1
