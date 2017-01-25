module VideoCore4.QPU.Instruction.Register
       (
         Register

       , ABRW(..)
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

data ABRW = AR
          | BR
          | AW
          | BW
          deriving (Eq, Show, Typeable)

data Register = Register { registerAddr :: Word8
                         , registerABRW :: [ABRW]
                         } deriving (Eq, Show, Typeable)

instance To64 Register where
  to64 = toEnum . fromEnum . (.&. 0x3F) . registerAddr

rwab :: Register -> [ABRW]
rwab = registerABRW

ra0 :: Register
ra0 = Register 0 [AR,AW]
rb0 :: Register
rb0 = Register 0 [BR,BW]
ra1 :: Register
ra1 = Register 1 [AR,AW]
rb1 :: Register
rb1 = Register 1 [BR,BW]
ra2 :: Register
ra2 = Register 2 [AR,AW]
rb2 :: Register
rb2 = Register 2 [BR,BW]
ra3 :: Register
ra3 = Register 3 [AR,AW]
rb3 :: Register
rb3 = Register 3 [BR,BW]
ra4 :: Register
ra4 = Register 4 [AR,AW]
rb4 :: Register
rb4 = Register 4 [BR,BW]
ra5 :: Register
ra5 = Register 5 [AR,AW]
rb5 :: Register
rb5 = Register 5 [BR,BW]
ra6 :: Register
ra6 = Register 6 [AR,AW]
rb6 :: Register
rb6 = Register 6 [BR,BW]
ra7 :: Register
ra7 = Register 7 [AR,AW]
rb7 :: Register
rb7 = Register 7 [BR,BW]
ra8 :: Register
ra8 = Register 8 [AR,AW]
rb8 :: Register
rb8 = Register 8 [BR,BW]
ra9 :: Register
ra9 = Register 9 [AR,AW]
rb9 :: Register
rb9 = Register 9 [BR,BW]
ra10 :: Register
ra10 = Register 10 [AR,AW]
rb10 :: Register
rb10 = Register 10 [BR,BW]
ra11 :: Register
ra11 = Register 11 [AR,AW]
rb11 :: Register
rb11 = Register 11 [BR,BW]
ra12 :: Register
ra12 = Register 12 [AR,AW]
rb12 :: Register
rb12 = Register 12 [BR,BW]
ra13 :: Register
ra13 = Register 13 [AR,AW]
rb13 :: Register
rb13 = Register 13 [BR,BW]
ra14 :: Register
ra14 = Register 14 [AR,AW]
rb14 :: Register
rb14 = Register 14 [BR,BW]
ra15 :: Register
ra15 = Register 15 [AR,AW]
rb15 :: Register
rb15 = Register 15 [BR,BW]
ra16 :: Register
ra16 = Register 16 [AR,AW]
rb16 :: Register
rb16 = Register 16 [BR,BW]
ra17 :: Register
ra17 = Register 17 [AR,AW]
rb17 :: Register
rb17 = Register 17 [BR,BW]
ra18 :: Register
ra18 = Register 18 [AR,AW]
rb18 :: Register
rb18 = Register 18 [BR,BW]
ra19 :: Register
ra19 = Register 19 [AR,AW]
rb19 :: Register
rb19 = Register 19 [BR,BW]
ra20 :: Register
ra20 = Register 20 [AR,AW]
rb20 :: Register
rb20 = Register 20 [BR,BW]
ra21 :: Register
ra21 = Register 21 [AR,AW]
rb21 :: Register
rb21 = Register 21 [BR,BW]
ra22 :: Register
ra22 = Register 22 [AR,AW]
rb22 :: Register
rb22 = Register 22 [BR,BW]
ra23 :: Register
ra23 = Register 23 [AR,AW]
rb23 :: Register
rb23 = Register 23 [BR,BW]
ra24 :: Register
ra24 = Register 24 [AR,AW]
rb24 :: Register
rb24 = Register 24 [BR,BW]
ra25 :: Register
ra25 = Register 25 [AR,AW]
rb25 :: Register
rb25 = Register 25 [BR,BW]
ra26 :: Register
ra26 = Register 26 [AR,AW]
rb26 :: Register
rb26 = Register 26 [BR,BW]
ra27 :: Register
ra27 = Register 27 [AR,AW]
rb27 :: Register
rb27 = Register 27 [BR,BW]
ra28 :: Register
ra28 = Register 28 [AR,AW]
rb28 :: Register
rb28 = Register 28 [BR,BW]
ra29 :: Register
ra29 = Register 29 [AR,AW]
rb29 :: Register
rb29 = Register 29 [BR,BW]
ra30 :: Register
ra30 = Register 30 [AR,AW]
rb30 :: Register
rb30 = Register 30 [BR,BW]
ra31 :: Register
ra31 = Register 31 [AR,AW]
rb31 :: Register
rb31 = Register 31 [BR,BW]
uniform_read :: Register
uniform_read = Register 32 [AR,BR]
r0 :: Register
r0 = Register 32 [AW,BW]
r1 :: Register
r1 = Register 33 [AW,BW]
r2 :: Register
r2 = Register 34 [AW,BW]
r3 :: Register
r3 = Register 35 [AW,BW]
tmu_noswap :: Register
tmu_noswap = Register 36 [AW,BW]
r5 :: Register
r5 = Register 37 [AW,BW]
element_number :: Register
element_number = Register 38 [AR]
qpu_number :: Register
qpu_number = Register 38 [BR]
host_int :: Register
host_int = Register 38 [AW,BW]
nop :: Register
nop = Register 39 [AR,BR,AW,BW]
uniforms_address :: Register
uniforms_address = Register 40 [AW,BW]
vpm_read :: Register
vpm_read = Register 48 [AR,BR]
vpm_write :: Register
vpm_write = Register 48 [AW,BW]
vpm_ld_busy :: Register
vpm_ld_busy = Register 49 [AR]
vpm_st_busy :: Register
vpm_st_busy = Register 49 [BR]
vpmvcd_rd_setup :: Register
vpmvcd_rd_setup = Register 49 [AW]
vpmvcd_wr_setup :: Register
vpmvcd_wr_setup = Register 49 [BW]
vpm_ld_wait :: Register
vpm_ld_wait = Register 50 [AR]
vpm_st_wait :: Register
vpm_st_wait = Register 50 [BR]
vpm_ld_addr :: Register
vpm_ld_addr = Register 50 [AW]
vpm_st_addr :: Register
vpm_st_addr = Register 50 [BW]
mutex_acquire :: Register
mutex_acquire = Register 51 [AR,BR]
mutex_release :: Register
mutex_release = Register 51 [AW,BW]
sfu_recip :: Register
sfu_recip = Register 52 [AW,BW]
sfu_recipsqrt :: Register
sfu_recipsqrt = Register 53 [AW,BW]
sfu_exp :: Register
sfu_exp = Register 54 [AW,BW]
sfu_log :: Register
sfu_log = Register 55 [AW,BW]
tmu0s :: Register
tmu0s = Register 56 [AW,BW]
tmu0t :: Register
tmu0t = Register 57 [AW,BW]
tmu0r :: Register
tmu0r = Register 58 [AW,BW]
tmu0b :: Register
tmu0b = Register 59 [AW,BW]
tmu1s :: Register
tmu1s = Register 60 [AW,BW]
tmu1t :: Register
tmu1t = Register 61 [AW,BW]
tmu1r :: Register
tmu1r = Register 62 [AW,BW]
tmu1b :: Register
tmu1b = Register 63 [AW,BW]
