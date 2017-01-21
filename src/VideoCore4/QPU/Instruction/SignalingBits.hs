module VideoCore4.QPU.Instruction.SignalingBits
       (
         SignalingBits
       , sig_SB
       , sig_NS
       , sig_TS
       , sig_PE
       , sig_WS
       , sig_SU
       , sig_LTS
       , sig_LDCOV
       , sig_LDCOL
       , sig_LDCOL_PE
       , sig_LDTMU0
       , sig_LDTMU1
       , sig_AL
       , sig_SIMM
       , sig_LIMM
       , sig_BRANCH
       ) where

import Data.Typeable
import Data.Word
import VideoCore4.QPU.Instruction.Types

newtype SignalingBits = SignalingBits { unSignalingBits :: Word8 } deriving (Eq, Show, Typeable)

instance To64 SignalingBits where
  to64 = toEnum . fromEnum . unSignalingBits

sig_SB :: SignalingBits
sig_SB = SignalingBits 0

sig_NS :: SignalingBits
sig_NS = SignalingBits 1

sig_TS :: SignalingBits
sig_TS = SignalingBits 2

sig_PE :: SignalingBits
sig_PE = SignalingBits 3

sig_WS :: SignalingBits
sig_WS = SignalingBits 4

sig_SU :: SignalingBits
sig_SU = SignalingBits 5

sig_LTS :: SignalingBits
sig_LTS = SignalingBits 6

sig_LDCOV :: SignalingBits
sig_LDCOV = SignalingBits 7

sig_LDCOL :: SignalingBits
sig_LDCOL = SignalingBits 8

sig_LDCOL_PE :: SignalingBits
sig_LDCOL_PE = SignalingBits 9

sig_LDTMU0 :: SignalingBits
sig_LDTMU0 = SignalingBits 10

sig_LDTMU1 :: SignalingBits
sig_LDTMU1 = SignalingBits 11

sig_AL :: SignalingBits
sig_AL = SignalingBits 12

sig_SIMM :: SignalingBits
sig_SIMM = SignalingBits 13

sig_LIMM :: SignalingBits
sig_LIMM = SignalingBits 14

sig_BRANCH :: SignalingBits
sig_BRANCH = SignalingBits 15
