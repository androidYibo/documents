type Rate = Double
type Time = Double
type Frequency = 4 -- Quarterly 

class Cashflow a where
  -- returns the amount of the cashflow
  amount :: a -> Double 
  -- Might contain more
  {- undef :: ... -}

class Cashflow a => Coupon a where
  -- In Quantlib this contains a lot of inspector functions, such as rate.
  -- Are those needed?
  -- rate returns the accured rate of the coupon
  rate :: a -> Rate 
  {- undef :: ... -}

data FixedRateCoupon = FixedRateCoupon Rate -- InterestRate?
instance Coupon FixedRateCoupon where
  rate (FixedRateCoupon r) = r 
  {- undef = ... -}

instance CashFlow FixedRateCoupon where
  -- I want to use the compoundFactor function from InterestRate.hs 
  -- But am having some trouble using the InterestRate class.
  -- Preferably i would like the Rate in FixedRateCoupon to be of the
  -- types ContinuousRate, ExponentialRate or SimpleRate. And no matter
  -- the exact type i want to call the compoundFactor function.
  -- I am thinking this can be done using typefamilies?
  amount (FixedRateCoupon r) = 0.0 

data FloatingRateCoupon = FixedRateCoupon Rate -- InterestRate?

instance Coupon FloatingRateCoupon where
  rate (FloatingRateCoupon r) = r 

instance CashFlow FloatingRateCoupon where
  amount (FloatingRateCoupon r) = 0.0 


-- In a new file under the Instrument folder

class Instrument s => Swap s where
  -- | returns whenever the swap is expired
  isExpired :: s -> bool
  -- | startdate of the swap
  startDate :: s -> Date 
  -- | calculates the maturity date of the swap
  maturityDate :: s -> Date
  -- | calculates the net present value of the swap 
  legNPV :: s -> Double 
  -- | calculates the basis point of the swap
  legBPS :: s -> Double 

-- | A vanillaswap contains a floating leg and a fixed leg
data VanillaSwap = VanillaSwap FixedRateCoupon FloatingRateCoupon

{- instance Swap VanillaSwap where -}
  {- value ::   -}

