module VideoCore4.QPU.Instruction.InputMux
       (
         InputMux
       , mux_R0
       , mux_R1
       , mux_R2
       , mux_R3
       , mux_R4
       , mux_R5
       , mux_RA
       , mux_RB
       ) where

import Data.Typeable
import Data.Word
import VideoCore4.QPU.Instruction.Types

newtype InputMux = InputMux { unInputMux :: Word8 } deriving (Eq, Show, Typeable)

instance To64 InputMux where
  to64 = toEnum . fromEnum . unInputMux

mux_R0 :: InputMux
mux_R0 = InputMux 0
mux_R1 :: InputMux
mux_R1 = InputMux 1
mux_R2 :: InputMux
mux_R2 = InputMux 2
mux_R3 :: InputMux
mux_R3 = InputMux 3
mux_R4 :: InputMux
mux_R4 = InputMux 4
mux_R5 :: InputMux
mux_R5 = InputMux 5
mux_RA :: InputMux
mux_RA = InputMux 6
mux_RB :: InputMux
mux_RB = InputMux 7
