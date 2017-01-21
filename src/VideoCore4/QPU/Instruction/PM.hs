module VideoCore4.QPU.Instruction.PM
       (
         PM
       , pm_Regfile
       , pm_R4MULALU
       ) where

import Data.Typeable
import Data.Word
import VideoCore4.QPU.Instruction.Types

newtype PM = PM { unPM :: Word8 } deriving (Eq, Show, Typeable)

instance To64 PM where
  to64 = toEnum . fromEnum . unPM

pm_Regfile :: PM
pm_Regfile = PM 0

pm_R4MULALU :: PM
pm_R4MULALU = PM 1
