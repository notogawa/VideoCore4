module VideoCore4.QPU.Instruction.WriteSwap
       (
         WriteSwap
       , ws_Off
       , ws_On
       ) where

import Data.Typeable
import Data.Word
import VideoCore4.QPU.Instruction.Types

newtype WriteSwap = WriteSwap { unWriteSwap :: Word8 } deriving (Eq, Show, Typeable)

instance To64 WriteSwap where
  to64 = toEnum . fromEnum . unWriteSwap

ws_Off :: WriteSwap
ws_Off = WriteSwap 0
ws_On :: WriteSwap
ws_On = WriteSwap 1
