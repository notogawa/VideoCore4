module VideoCore4.QPU.Instruction.Register
       (
         Register

       , RWAB(..)
       , rwab

       , ra0
       , rb0
       , ra1
       , rb1
       , ra2
       , rb2
       , ra3
       , rb3
       , ra4
       , rb4
       , ra5
       , rb5
       , ra6
       , rb6
       , ra7
       , rb7
       , ra8
       , rb8
       , ra9
       , rb9
       , ra10
       , rb10
       , ra11
       , rb11
       , ra12
       , rb12
       , ra13
       , rb13
       , ra14
       , rb14
       , ra15
       , rb15
       , ra16
       , rb16
       , ra17
       , rb17
       , ra18
       , rb18
       , ra19
       , rb19
       , ra20
       , rb20
       , ra21
       , rb21
       , ra22
       , rb22
       , ra23
       , rb23
       , ra24
       , rb24
       , ra25
       , rb25
       , ra26
       , rb26
       , ra27
       , rb27
       , ra28
       , rb28
       , ra29
       , rb29
       , ra30
       , rb30
       , ra31
       , rb31
       , uniform_read
       , r0
       , r1
       , r2
       , r3
       , tmu_noswap
       , r5
       , element_number
       , qpu_number
       , host_int
       , nop
       , uniforms_address
       , vpm_read
       , vpm_write
       , vpm_ld_busy
       , vpm_st_busy
       , vpmvcd_rd_setup
       , vpmvcd_wr_setup
       , vpm_ld_wait
       , vpm_st_wait
       , vpm_ld_addr
       , vpm_st_addr
       , mutex_acquire
       , mutex_release
       , sfu_recip
       , sfu_recipsqrt
       , sfu_exp
       , sfu_log
       , tmu0s
       , tmu0t
       , tmu0r
       , tmu0b
       , tmu1s
       , tmu1t
       , tmu1r
       , tmu1b
       ) where

import Data.Bits
import Data.Typeable
import Data.Word
import VideoCore4.QPU.Instruction.Types

data RWAB = RA
          | RB
          | WA
          | WB
          deriving (Eq, Show, Typeable)

data Register = Register { registerAddr :: Word8
                         , registerRWAB :: [RWAB]
                         } deriving (Eq, Show, Typeable)

instance To64 Register where
  to64 = toEnum . fromEnum . (.&. 0x3F) . registerAddr

rwab :: Register -> [RWAB]
rwab = registerRWAB

ra0 :: Register
ra0 = Register 0 [RA,WA]
rb0 :: Register
rb0 = Register 0 [RB,WB]
ra1 :: Register
ra1 = Register 1 [RA,WA]
rb1 :: Register
rb1 = Register 1 [RB,WB]
ra2 :: Register
ra2 = Register 2 [RA,WA]
rb2 :: Register
rb2 = Register 2 [RB,WB]
ra3 :: Register
ra3 = Register 3 [RA,WA]
rb3 :: Register
rb3 = Register 3 [RB,WB]
ra4 :: Register
ra4 = Register 4 [RA,WA]
rb4 :: Register
rb4 = Register 4 [RB,WB]
ra5 :: Register
ra5 = Register 5 [RA,WA]
rb5 :: Register
rb5 = Register 5 [RB,WB]
ra6 :: Register
ra6 = Register 6 [RA,WA]
rb6 :: Register
rb6 = Register 6 [RB,WB]
ra7 :: Register
ra7 = Register 7 [RA,WA]
rb7 :: Register
rb7 = Register 7 [RB,WB]
ra8 :: Register
ra8 = Register 8 [RA,WA]
rb8 :: Register
rb8 = Register 8 [RB,WB]
ra9 :: Register
ra9 = Register 9 [RA,WA]
rb9 :: Register
rb9 = Register 9 [RB,WB]
ra10 :: Register
ra10 = Register 10 [RA,WA]
rb10 :: Register
rb10 = Register 10 [RB,WB]
ra11 :: Register
ra11 = Register 11 [RA,WA]
rb11 :: Register
rb11 = Register 11 [RB,WB]
ra12 :: Register
ra12 = Register 12 [RA,WA]
rb12 :: Register
rb12 = Register 12 [RB,WB]
ra13 :: Register
ra13 = Register 13 [RA,WA]
rb13 :: Register
rb13 = Register 13 [RB,WB]
ra14 :: Register
ra14 = Register 14 [RA,WA]
rb14 :: Register
rb14 = Register 14 [RB,WB]
ra15 :: Register
ra15 = Register 15 [RA,WA]
rb15 :: Register
rb15 = Register 15 [RB,WB]
ra16 :: Register
ra16 = Register 16 [RA,WA]
rb16 :: Register
rb16 = Register 16 [RB,WB]
ra17 :: Register
ra17 = Register 17 [RA,WA]
rb17 :: Register
rb17 = Register 17 [RB,WB]
ra18 :: Register
ra18 = Register 18 [RA,WA]
rb18 :: Register
rb18 = Register 18 [RB,WB]
ra19 :: Register
ra19 = Register 19 [RA,WA]
rb19 :: Register
rb19 = Register 19 [RB,WB]
ra20 :: Register
ra20 = Register 20 [RA,WA]
rb20 :: Register
rb20 = Register 20 [RB,WB]
ra21 :: Register
ra21 = Register 21 [RA,WA]
rb21 :: Register
rb21 = Register 21 [RB,WB]
ra22 :: Register
ra22 = Register 22 [RA,WA]
rb22 :: Register
rb22 = Register 22 [RB,WB]
ra23 :: Register
ra23 = Register 23 [RA,WA]
rb23 :: Register
rb23 = Register 23 [RB,WB]
ra24 :: Register
ra24 = Register 24 [RA,WA]
rb24 :: Register
rb24 = Register 24 [RB,WB]
ra25 :: Register
ra25 = Register 25 [RA,WA]
rb25 :: Register
rb25 = Register 25 [RB,WB]
ra26 :: Register
ra26 = Register 26 [RA,WA]
rb26 :: Register
rb26 = Register 26 [RB,WB]
ra27 :: Register
ra27 = Register 27 [RA,WA]
rb27 :: Register
rb27 = Register 27 [RB,WB]
ra28 :: Register
ra28 = Register 28 [RA,WA]
rb28 :: Register
rb28 = Register 28 [RB,WB]
ra29 :: Register
ra29 = Register 29 [RA,WA]
rb29 :: Register
rb29 = Register 29 [RB,WB]
ra30 :: Register
ra30 = Register 30 [RA,WA]
rb30 :: Register
rb30 = Register 30 [RB,WB]
ra31 :: Register
ra31 = Register 31 [RA,WA]
rb31 :: Register
rb31 = Register 31 [RB,WB]
uniform_read :: Register
uniform_read = Register 32 [RA,RB]
r0 :: Register
r0 = Register 32 [WA,WB]
r1 :: Register
r1 = Register 33 [WA,WB]
r2 :: Register
r2 = Register 34 [WA,WB]
r3 :: Register
r3 = Register 35 [WA,WB]
tmu_noswap :: Register
tmu_noswap = Register 36 [WA,WB]
r5 :: Register
r5 = Register 37 [WA,WB]
element_number :: Register
element_number = Register 38 [RA]
qpu_number :: Register
qpu_number = Register 38 [RB]
host_int :: Register
host_int = Register 38 [WA,WB]
nop :: Register
nop = Register 39 [RA,RB,WA,WB]
uniforms_address :: Register
uniforms_address = Register 40 [WA,WB]
vpm_read :: Register
vpm_read = Register 48 [RA,RB]
vpm_write :: Register
vpm_write = Register 48 [WA,WB]
vpm_ld_busy :: Register
vpm_ld_busy = Register 49 [RA]
vpm_st_busy :: Register
vpm_st_busy = Register 49 [RB]
vpmvcd_rd_setup :: Register
vpmvcd_rd_setup = Register 49 [WA]
vpmvcd_wr_setup :: Register
vpmvcd_wr_setup = Register 49 [WB]
vpm_ld_wait :: Register
vpm_ld_wait = Register 50 [RA]
vpm_st_wait :: Register
vpm_st_wait = Register 50 [RB]
vpm_ld_addr :: Register
vpm_ld_addr = Register 50 [WA]
vpm_st_addr :: Register
vpm_st_addr = Register 50 [WB]
mutex_acquire :: Register
mutex_acquire = Register 51 [RA,RB]
mutex_release :: Register
mutex_release = Register 51 [WA,WB]
sfu_recip :: Register
sfu_recip = Register 52 [WA,WB]
sfu_recipsqrt :: Register
sfu_recipsqrt = Register 53 [WA,WB]
sfu_exp :: Register
sfu_exp = Register 54 [WA,WB]
sfu_log :: Register
sfu_log = Register 55 [WA,WB]
tmu0s :: Register
tmu0s = Register 56 [WA,WB]
tmu0t :: Register
tmu0t = Register 57 [WA,WB]
tmu0r :: Register
tmu0r = Register 58 [WA,WB]
tmu0b :: Register
tmu0b = Register 59 [WA,WB]
tmu1s :: Register
tmu1s = Register 60 [WA,WB]
tmu1t :: Register
tmu1t = Register 61 [WA,WB]
tmu1r :: Register
tmu1r = Register 62 [WA,WB]
tmu1b :: Register
tmu1b = Register 63 [WA,WB]
