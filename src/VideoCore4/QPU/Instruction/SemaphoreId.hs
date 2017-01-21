module VideoCore4.QPU.Instruction.SemaphoreId
       (
         SemaphoreId
       , semaphore_0
       , semaphore_1
       , semaphore_2
       , semaphore_3
       , semaphore_4
       , semaphore_5
       , semaphore_6
       , semaphore_7
       , semaphore_8
       , semaphore_9
       , semaphore_10
       , semaphore_11
       , semaphore_12
       , semaphore_13
       , semaphore_14
       , semaphore_15
       ) where

import Data.Typeable
import Data.Word
import VideoCore4.QPU.Instruction.Types

newtype SemaphoreId = SemaphoreId { unSemaphoreId :: Word8 } deriving (Eq, Show, Typeable)

instance To64 SemaphoreId where
  to64 = toEnum . fromEnum . unSemaphoreId

semaphore_0 :: SemaphoreId
semaphore_0 = SemaphoreId 0
semaphore_1 :: SemaphoreId
semaphore_1 = SemaphoreId 1
semaphore_2 :: SemaphoreId
semaphore_2 = SemaphoreId 2
semaphore_3 :: SemaphoreId
semaphore_3 = SemaphoreId 3
semaphore_4 :: SemaphoreId
semaphore_4 = SemaphoreId 4
semaphore_5 :: SemaphoreId
semaphore_5 = SemaphoreId 5
semaphore_6 :: SemaphoreId
semaphore_6 = SemaphoreId 6
semaphore_7 :: SemaphoreId
semaphore_7 = SemaphoreId 7
semaphore_8 :: SemaphoreId
semaphore_8 = SemaphoreId 8
semaphore_9 :: SemaphoreId
semaphore_9 = SemaphoreId 9
semaphore_10 :: SemaphoreId
semaphore_10 = SemaphoreId 10
semaphore_11 :: SemaphoreId
semaphore_11 = SemaphoreId 11
semaphore_12 :: SemaphoreId
semaphore_12 = SemaphoreId 12
semaphore_13 :: SemaphoreId
semaphore_13 = SemaphoreId 13
semaphore_14 :: SemaphoreId
semaphore_14 = SemaphoreId 14
semaphore_15 :: SemaphoreId
semaphore_15 = SemaphoreId 15
