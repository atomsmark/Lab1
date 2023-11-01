{- Lab 1
   Date: 
   Authors:
   Lab group:
 -}
--------------------------------------------
power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k-1)

-- A ------------------------
-- stepsPower n k gives the number of steps that
-- power n k takes to compute

stepsPower :: Integer -> Integer -> Integer
stepsPower n k = k + 1


-- B -------------------------
-- power1

power1 n k | k < 0 = error "power: negative argument"
power1 n 0         = 1
power1 n k         = product (replicate ((fromInteger k) n))   -- Behöver n fromInteger?


-- C -------------------------
-- power2

power2 n k | k < 0  = error "power: negative argument"
power2 n 0          = 1
power2 n k | odd k  = n * power2 (n (k-1))
           | even k = power2 ((n*n) (k 'div' 2))

-- D -------------------------
{- 

   -- 3 -1 (not defined for k < 0)
   -- 3 0  (k = 0 should result in 1)
   -- 3 3  (odd k)
   -- 3 4  (even k)

 -}

-- 

prop_powers n k = power (n k) == power1 (n k) == power2 (n k)

--
powerTest :: Bool
powerTest = undefined

--
prop_powers' = undefined