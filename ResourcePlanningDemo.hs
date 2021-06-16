{-# LANGUAGE FlexibleContexts #-}

          ------------------------------------------------------------
          ------------------------------------------------------------
          --
          -- BEGIN
          --
          -- General script for approximate dynamic programming.
          --
          -- Author: Sean Matthews
          -- Date:   As VCS
          --
          ------------------------------------------------------------
          --
          -- The fundamental  idea of approximate  dynamic programming
          -- as  developed by,  e.g.,  Powell  and Bertsekas  (roughly
          -- formulated) is that under  quite general circumstances we
          -- can  extend a  solvable  bounded-step optimisation  model
          -- with  an   expectation  function   and  still   get  good
          -- convergence   properties,   using   stochastic   gradient
          -- descent.
          --
          -- In  practice, this  idea  can be  applied  to extend  the
          -- classic OR mixed integer  programming model with a convex
          -- (or concave,  as appropriate) expectation function  - and
          -- this in turn provides a principled technique for tackling
          -- multiperiod and multiechelon  optimisation problems which
          -- were  previously  addressed  mostly  with  heuristics  of
          -- various sorts. There is a lot of research in this area at
          -- the moment -  I believe, for instance, that  this is more
          -- or less what the IBM MESO system does.
          -- 
          -- This   script  is   my   first  attempt   at  a   general
          -- implemenation of  the idea. If  I compare this to  what I
          -- know of  other implemenatations, e.g.   Powell's 'Castle'
          -- system,  which appear  to use  C++ or  similar notations,
          -- then I think I can claim the following advantages:
          -- 
          --  - I use the the AD automatic differentiation package for
          --    haskell  to   treat  essentially   arbitrary  (convex)
          --    expectation functions easily (and certainly a lot more
          --    cleanly than the messy  approach that Powell describes
          --    for his  system - note  that I can certainly  use more
          --    complex functions than Powell  restricts himself to in
          --    his discussion
          --
          --  - I  inherit  the  obvious  advantages  of  Haskell,  in
          --    particular, conciseness, declarative transparency, and
          --    'correctness   by   construction'.   These   are   all
          --    _practical_ advantages.
          --
          -- As an  example, I  implement the blood  supply management
          -- (complex inventory managment) problem from Powell, 2011:
          --
          --   exercise 14.4. Blood  management project.  Implement the
          --     blood managment model described in section 14.2.
          --
          -- However   the  script   should  be   (relatively)  easily
          -- modifiable   for  other   multiperiod  and   mulitechelon
          -- problems.
          --
          -- note  that -  for  reasons i  don't  understand, but  are
          -- documented  - if  you try  to run  this dynamically  on a
          -- large model,  it may crash.  This does not happen  if you
          -- compile and run as a stand-alone.
          --
          ------------------------------------------------------------
          ------------------------------------------------------------
          --

import Data.List
import Data.Tuple
import Data.Monoid
import Data.Foldable
import System.Random
import Data.Random.Normal

import Control.Monad.Trans.Class
import qualified Data.Map.Internal (toList)

import Control.Monad
import Control.Monad.Trans.State
import Control.Exception

          -- Major  external modules  are  an API  for  GPLK (the  GNU
          -- Linear  Programming  Kit,  which   has  to  be  installed
          -- externally),   and    AD,   which    provides   automatic
          -- differentiation.

import Numeric.AD

import Control.Monad.LPMonad
import Data.LinearProgram (Direction (Max), LinFunc,
                           linCombination, LP, VarKind(..))
       
import Data.LinearProgram.GLPK

          --
          ------------------------------------------------------------
          ------------------------------------------------------------
          --
          -- Basic model - from Powell, 2nd ed., p588
          --
          -- We  have a  set  of  possible blood  types,  and we  have
          -- properties, constraints  etc. that we want  to keep track
          -- of, etc. on a per-type basis, thus:

data BloodTypes = ABp | ABn | Ap | An | Bp | Bn | Op | On
                deriving (Eq, Show, Enum, Ord, Bounded)

data BTVector a = BTVector { nABp :: a, nABn :: a, nAp  :: a, nAn  :: a,
                             nBp  :: a, nBn  :: a, nOp  :: a, nOn  :: a}
                deriving (Eq, Show)

instance Functor BTVector where
  fmap f (BTVector x1 x2 x3 x4 x5 x6 x7 x8) =
    BTVector (f x1) (f x2) (f x3) (f x4) (f x5) (f x6) (f x7) (f x8)

instance Applicative BTVector where
  pure f = BTVector f f f f f f f f
  (BTVector f1 f2 f3 f4 f5 f6 f7 f8) <*> (BTVector x1 x2 x3 x4 x5 x6 x7 x8) =
    BTVector (f1 x1) (f2 x2) (f3 x3) (f4 x4) (f5 x5) (f6 x6) (f7 x7) (f8 x8)

instance Foldable BTVector where
  foldr f z (BTVector x1 x2 x3 x4 x5 x6 x7 x8) =
    f x1 $ f x2 $ f x3 $ f x4 $ f x5 $ f x6 $ f x7 $ f x8 z

instance Traversable BTVector where
  traverse f (BTVector x1 x2 x3 x4 x5 x6 x7 x8) =
    BTVector <$> f x1 <*> f x2 <*> f x3 <*> f x4 <*> f x5 <*> f x6 <*> f x7 <*> f x8

typesBTVector = BTVector ABp ABn Ap An Bp Bn Op On

          --
          ------------------------------------------------------------
          --
          -- Demand and supply are measured in 'Unit's
          --

type Unit = Int

          --
          ------------------------------------------------------------
          -- 
          -- We  can  substitute  one  type  with  another  (at  least
          -- sometimes)
  
canBeUsedFor :: BTVector [BloodTypes]
canBeUsedFor =
  BTVector { nABp = [ABp                             ] ,
             nABn = [ABp, ABn                        ] ,
             nAp  = [ABp,      Ap                    ] ,
             nAn  = [ABp, ABn, Ap, An                ] ,
             nBp  = [ABp,              Bp            ] ,
             nBn  = [ABp, ABn,         Bp, Bn        ] ,
             nOp  = [ABp,      Ap,     Bp,     Op    ] ,
             nOn  = [ABp, ABn, Ap, An, Bp, Bn, Op, On] }

          -- we also need the inverse

canBeSubedWith :: BTVector [BloodTypes]
canBeSubedWith =
  let
    all_pairs = foldl1 (++) $ f <$> typesBTVector <*> canBeUsedFor
      where f l = fmap (\x -> (l, x))
    g i   = [ fst j | j <- all_pairs, snd j == i]
  in
    fmap g typesBTVector
  
          -- 
          ------------------------------------------------------------
          -- 
          -- We have types of demand
                
data DemandTypes = Urgent | Elective
                 deriving (Eq, Show, Ord, Enum, Bounded)

          -- and substitution conditions

data CanSubstitute = CanSubstitute | CannotSubstitute
                   deriving (Eq, Show, Ord, Enum, Bounded)

          --
          ------------------------------------------------------------
          -- We have supply and demand.
          -- 
          -- A  certain amount [Not specified in  detail in Powell] of
          -- demand is  elective, and  a certain  amount of  demand is
          -- substitutable.
          --
          -- We thus  need a  hiearchy of  sampling objects.
          --
          -- NOTE THAT  FOR SAMPLING WE NEED  TO KEEP TRACK OF  A SEED
          -- (STATE)  FOR THE  RANDOM NUMBER  GENERATOR -  SO SAMPLING
          -- FACILITIES ARE ALL INSTANCES OF THE STATE MONAD
          -- 
          ------------------------------------------------------------
          --
          -- Preparatory  to this  we need  a random  sample generator
          -- that draws from a weighted range of alternatives; e.g. to
          -- sample donations  or demands.  There are  various ways we
          -- might do this;  cleanest would probably be  to define new
          -- instances  of random,  but  that seems  maybe  a bit  OTT
          -- currently.  Instead we  do it as follows.   Note we check
          -- that the provided  pd is reasonably sane  (we could avoid
          -- some of this by dropping one weight - maybe that would be
          -- better?).
          --

buildSampleM  :: (Monad m, RandomGen s, Traversable t) =>
                   t (b, Double) -> StateT s m b
buildSampleM pd =
  let
    cd = snd $ mapAccumL (\a -> \(t,p) -> (a + p, (t, a + p)))
                         (1 - (sum . (fmap snd) $ pd))
                         pd
  in
    assert (let l = fmap snd pd in abs(sum l - 1) < 0.01 && minimum l >= 0) $ do
      sample <- state $ randomR (0, 1)
      return $ (\(Just (t, _)) -> t) $ find (\(t, p) -> sample < p) cd

bTVtoPD btv = foldl1 (++) $ f <$> typesBTVector <*> btv
                where f n pd = [(n, pd)]

          --
          ------------------------------------------------------------
          --

meanPeriodDemand :: Double
meanPeriodDemand = 200

sdPeriodDemand :: Double
sdPeriodDemand =  60

samplePeriodDemand :: (Monad m, RandomGen s) => StateT s m Unit
samplePeriodDemand =
  liftM (max 0 . round) $
    state $
      normal' (meanPeriodDemand, sdPeriodDemand)

meanPeriodSupply :: Double
meanPeriodSupply = 150
sdPeriodSupply   = 45

samplePeriodSupply :: (Monad m, RandomGen s) => StateT s m Unit
samplePeriodSupply =
  liftM (max 0 . round) $
    state $
      normal' (meanPeriodSupply, sdPeriodSupply)

          -- Donated Units have the following type distribution

sampleSupply :: (Monad m, RandomGen s) => StateT s m BloodTypes
sampleSupply =
  buildSampleM $
    bTVtoPD $
      BTVector { nABp = 0.0340,
                 nABn = 0.0065,
                 nAp =  0.2794,
                 nAn =  0.0517,
                 nBp =  0.1163,
                 nBn =  0.0213,
                 nOp =  0.3982,
                 nOn =  0.092 }

          -- ditto Demand (not quite the same distribution)

sampleDemand :: (Monad m, RandomGen s) => StateT s m BloodTypes
sampleDemand =
  buildSampleM $
    bTVtoPD $
      BTVector { nABp = 0.0300,
                 nABn = 0.0065,
                 nAp  = 0.3400,
                 nAn  = 0.0600,
                 nBp  = 0.0900,
                 nBn  = 0.0200,
                 nOp  = 0.3800,
                 nOn  = 0.0700}

sampleDemandType :: (Monad m, RandomGen s) => StateT s m DemandTypes
sampleDemandType =
  buildSampleM [ (Urgent  , 0.75),
                 (Elective, 0.25)]

sampleCanSubstitute :: (Monad m, RandomGen s) => StateT s m CanSubstitute
sampleCanSubstitute =
  buildSampleM [ (CanSubstitute   , 0.75),
                 (CannotSubstitute, 0.25)]

          -- A demand unit is then

data DemandUnit = DemandUnit BloodTypes DemandTypes CanSubstitute
                deriving (Eq, Show)

sampleDemandUnit :: (Monad m, RandomGen s) => StateT s m DemandUnit
sampleDemandUnit = do
  t <- sampleDemand
  d <- sampleDemandType
  s <- sampleCanSubstitute
  return $ DemandUnit t d s

          -- We also need to be able to sample stores
          -- NOTE THIS NEEDS TO BE REFINED

sampleStore :: (Monad m, RandomGen s) => StateT s m (Store Unit)
sampleStore =
  mapM (const sampleTimebuckets) typesBTVector
    where sampleTimebuckets = mapM (const sampleBucket) bucketsTimeBucketVector
            where sampleBucket = liftM (max 0 . round) (state $ normal' (2::Double, 2))

          --
          ------------------------------------------------------------
          --
          -- Once we  have generated  a sequence  of demand  units, we
          -- need to aggregate this (see below). The aggregation unit is
          -- as follows:

data DemandTypeVector a = DemandTypeVector {urgentCanSub   :: a,
                                            urgentNoSub    :: a,
                                            electiveCanSub :: a,
                                            electiveNoSub  :: a }
                        deriving (Eq, Show)

typesDemandTypeVector =
  DemandTypeVector (Urgent  , CanSubstitute   )
                   (Urgent  , CannotSubstitute)
                   (Elective, CanSubstitute   )
                   (Elective, CannotSubstitute)

instance Functor DemandTypeVector where
  fmap f (DemandTypeVector x1 x2 x3 x4) =
    DemandTypeVector (f x1) (f x2) (f x3) (f x4)

instance Applicative DemandTypeVector where
  pure f = DemandTypeVector f f f f
  (DemandTypeVector f1 f2 f3 f4) <*> (DemandTypeVector x1 x2 x3 x4) =
    DemandTypeVector (f1 x1) (f2 x2) (f3 x3) (f4 x4)

          --
          ------------------------------------------------------------
          --
          -- Now we can sample supply  and demand for one period.  For
          -- Supply we simply sample the  number of units donated, and
          -- then aggregate this on a per type basis. Demand units are
          -- more complex: for each unit we sample the properties, and
          -- then aggregate  into a  vector of the  different possible
          -- classes of demand for each type.

data OnePeriod = OnePeriod { supply :: BTVector Unit,
                             demand :: BTVector (DemandTypeVector Unit)}
               deriving (Eq, Show)

sampleOnePeriod :: (Monad m, RandomGen s) => StateT s m OnePeriod
sampleOnePeriod = do
  mdSize <- samplePeriodDemand
  msSize <- samplePeriodSupply
  ms     <- sequence $ replicate mdSize sampleSupply
  md     <- sequence $ replicate mdSize sampleDemandUnit
  
  let f t = length $ filter (== t) ms
  let g t = fmap h typesDemandTypeVector
        where h (u, s) = length $ filter p md
                where p (DemandUnit t' u' s') =
                          (t == t' && u == u' && s == s')

  return OnePeriod {supply = fmap f typesBTVector,
                    demand = fmap g typesBTVector}

          -- An EventSequence is then a sequence of OnePeriod samples

type EventSequence = [ OnePeriod ]

          --
          ------------------------------------------------------------
          ------------------------------------------------------------
          --
          -- We need a  store (state).  This evolves over  time, as we
          -- (i)  receive new  dontations, which  we store,  (ii) draw
          -- down  demands, and  (iii) dispose  of old  supplies which
          -- have become unusable due to age.
          --
          -- We  can  build  a  state  out of  the  BTVector  and  the
          -- following, which can be  glued together effectively using
          -- Applicative.

data AgeBucket = T0 | T1 | T2 | T3 | T4 | T5
               deriving (Eq, Show, Enum, Ord, Bounded)

data TimeBucketVector a = TimeBucketVector a a a a a a
                        deriving (Eq, Show)

bucketsTimeBucketVector :: TimeBucketVector AgeBucket
bucketsTimeBucketVector = TimeBucketVector T0 T1 T2 T3 T4 T5

agesTimeBucketVector :: Floating a => TimeBucketVector a
agesTimeBucketVector = TimeBucketVector  0  1  2  3  4  5

instance Functor TimeBucketVector where
  fmap f (TimeBucketVector x0 x1 x2 x3 x4 x5) =
    TimeBucketVector (f x0) (f x1) (f x2) (f x3) (f x4) (f x5)

instance Applicative TimeBucketVector where
  pure f = TimeBucketVector f f f f f f
  (TimeBucketVector f0 f1 f2 f3 f4 f5) <*> (TimeBucketVector x0 x1 x2 x3 x4 x5) =
    TimeBucketVector (f0 x0) (f1 x1) (f2 x2) (f3 x3) (f4 x4) (f5 x5)

instance Foldable TimeBucketVector where
  foldr f z (TimeBucketVector t0 t1 t2 t3 t4 t5) =
    f t0 $ f t1 $ f t2 $ f t3 $ f t4 $ f t5 z

instance Traversable TimeBucketVector where
  traverse f (TimeBucketVector t0 t1 t2 t3 t4 t5) =
    TimeBucketVector <$> f t0 <*> f t1 <*> f t2 <*> f t3 <*> f t4 <*> f t5

tickTimeBuckets :: a -> TimeBucketVector a -> TimeBucketVector a
tickTimeBuckets nt0 (TimeBucketVector t0 t1 t2 t3 t4 _) =
  TimeBucketVector nt0 t0 t1 t2 t3 t4

drawdownTimeBuckets :: (Ord a, Num a) => a ->
                           TimeBucketVector a -> TimeBucketVector a
drawdownTimeBuckets dd (TimeBucketVector t0 t1 t2 t3 t4 t5) =
  TimeBucketVector nt0 nt1 nt2 nt3 nt4 nt5
    where (_, [nt5, nt4, nt3, nt2, nt1, nt0]) = 
            mapAccumL f dd [t5, t4, t3, t2, t1, t0]
              where f a b = (max 0 $ a - b, max 0 $ b - a) 

          --
          ------------------------------------------------------------
          --
          -- A store is  then a vector of bucket vectors  in which you
          -- can put stuff (one for each period), for each type.
          --

type Store a = BTVector (TimeBucketVector a)

          -- We put new stuff into a store (pushing out old stuff)
          -- with

updateStore :: BTVector Unit -> Store Unit -> Store Unit
updateStore supply store = tickTimeBuckets <$> supply <*> store

          -- and draw down with

drawdownStore :: BTVector Unit -> Store Unit -> Store Unit
drawdownStore dd store = drawdownTimeBuckets <$> dd <*> store

          -- we also an instantiation constant (we start with nothing)

emptyBank :: Store Unit
emptyBank = fmap (const zeroTimeBucketVector) typesBTVector
  where zeroTimeBucketVector = fmap (const 0) bucketsTimeBucketVector

          -- Also useful (for development and  testing) to have a Bank
          -- with stuff in it.

oneBank :: Store Unit
oneBank = fmap (const oneTimeBucketVector) typesBTVector
  where oneTimeBucketVector = fmap (const 1) bucketsTimeBucketVector

          --
          ------------------------------------------------------------
          ------------------------------------------------------------
          --
          -- The expectation function.
          --
          -- We  need an  expectation  function  for the  optimisation
          -- model: it would  be nice if this  were concave/convex (as
          -- appropriate).   Further, since  we  are  already using  a
          -- linear/integer  solver,  it would  be  nice  if we  could
          -- construct   a   good  piecewise   linear   approximation.
          -- Powell's approach looks hacky to me. Since we have access
          -- to clean,  systematic automatic differentiation,  we take
          -- another  approach.  We  maintain an  analytic 'reference'
          -- function, for which we can calculate an analytic gradient
          -- (big win compared  to Powell), and we derive  from this a
          -- piecewise linear  approximation that we can  put into the
          -- optimiser.
          --
          ------------------------------------------------------------
          --
          -- Start  with the  analytic function,  actually the  vector
          -- (one for each type) of analytics functions
          -- 
          -- We have a block of parameters (for each type)

data ThetaType a = ThetaType { intercept :: a,
                               scale     :: a,
                               decay     :: a }
  deriving (Eq, Show)

zeroThetaType = ThetaType 0 0 0
      
instance Functor ThetaType where
  fmap f (ThetaType x1 x2 x3) = ThetaType (f x1) (f x2) (f x3)

instance Applicative ThetaType where
  pure f = ThetaType f f f
  (ThetaType f1 f2 f3) <*> (ThetaType x1 x2 x3) =
      ThetaType (f1 x1) (f2 x2) (f3 x3)

instance Foldable ThetaType where
  foldr f z (ThetaType x1 x2 x3) =
    f x1 $ f x2 $ f x3 z

instance Traversable ThetaType where
  traverse f (ThetaType x1 x2 x3) =
    ThetaType <$> f x1 <*> f x2 <*> f x3

          --
          ------------------------------------------------------------
          --
          -- ThetaType defines  the parameters for _one_  type, but we
          -- actually maintain an  array of these, one  for each type,
          -- in a  BTVector. We call  _this_ type Vparams.   We cannot
          -- simply compose the type constructors, however - for AD we
          -- need to lift the parameter  block into a Traversable type
          -- where no  internal structure  is visible to  the standard
          -- combinators, thus:

newtype Theta a = Theta { getTheta :: BTVector (ThetaType a) }
  deriving (Show)

instance Functor Theta where
  fmap f = Theta . fmap (fmap f) . getTheta

instance Applicative Theta where
  pure f = Theta (pure (pure f))
  (Theta f) <*> (Theta x) =
    Theta ((\f' -> \x' -> (f' <*> x')) <$> f <*> x)

instance Foldable Theta where
  foldr f z (Theta v) =
    (foldl1 (.) (fmap (flip (foldr f)) v)) z

          -- FOLLOWING DEFINITION NEEDS TO BE FIXED!! 

instance Traversable Theta where
  traverse f (Theta (BTVector x1 x2 x3 x4 x5 x6 x7 x8)) =
    Theta <$> (BTVector <$> g x1 <*> g x2 <*> g x3 <*>
                g x4 <*> g x5 <*> g x6 <*> g x7 <*> g x8)
    where g (ThetaType x1 x2 x3) = ThetaType <$> f x1 <*> f x2 <*> f x3

zeroTheta = Theta $ fmap (const zeroThetaType) typesBTVector

          --
          ------------------------------------------------------------
          --
          -- Now we can define the (reference) value function.
          --
          -- [note: (i)  We add exponentials where  necessary to shift
          --  ranges from +R into R; (ii) we factor out the underlying
          --  curve as a separate function.]

curve :: Floating a => a -> a -> a -> a
curve  intercept scale x = 
  exp intercept + exp scale - (exp scale / (1 + x))

decayCoefs decay =
  let
    maximumAge = maximum agesTimeBucketVector
  in
    fmap (\age -> (maximumAge - age) * exp decay) agesTimeBucketVector

expectation :: (Floating a, Ord a) => Store a -> Theta a -> a
expectation point theta =
  sum $ curve <$>
         (fmap intercept theta') <*>
           (fmap scale theta') <*>
             (dot <$> fmap (decayCoefs . decay) theta' <*> point)
    where theta' = getTheta theta

          -- And the gradient is:
          --
          -- [we need to lift the type of each scalar in point so that
          --  AD can go  to work.  We do this with  auto; we also need
          --  to watch  out for NaNs  appearing in the  derivative. We
          --  set these to 0.}

dExpectation :: (Floating a, Ord a) => Store a -> Theta a -> Theta a
dExpectation point = grad (expectation (fmap (fmap auto) point))

          --
          ------------------------------------------------------------
          --
          -- We  also need  a  piecewise linear  approximation to  the
          -- (convex/concave) function f (in this case 'curve') in the
          -- interval x1 < x2
          --
          -- The line tangent to f at x (= point) is

          --  Line (x,fx) dfx

data Line a = Line {point :: (a, a) ,
                    slope :: a      }
              deriving (Eq, Show)

          -- so for curve (as defined above)

line intercept scale x =
  Line (x, fx) dfx
    where (fx, dfx') = diff' (curve (auto intercept) (auto scale)) x
          dfx = assert (isOK dfx') dfx'

          -- We define the Hull  of a (convex/concave, differentiable)
          -- function  f between  two points  left and  right to  be a
          -- structure of lines for which f defines the envelope.  The
          -- Hull  of f  on the  interval  between two  lines has  the
          -- recursive structure:

data Hull a = Hull { left  :: Line a           ,
                     down  :: (Hull a, Hull a) ,
                     right :: Line a           }

          -- where left and right are the Lines at x1 and x2, and down
          -- consists of  the pair of Hulls  on the intervals x1  < x'
          -- and x' < x2 for some point x'.
          --
          -- [we DO NOT want to derive Show for this thing!]
          --
          -- This construction is fairly indifferent to how exactly we
          -- pick x'.   We do as  follows. A 'hull'  naturally defines
          -- two points on its interval:  the midpoint of the interval
          -- itself  and the  intersection of  the endpoint  tangents.
          -- Take the midpoint of these points.

buildHull :: Floating a => (a -> Line a) -> a -> a -> Hull a
buildHull line x1 x2 =

          -- note that line to be  provided here takes one parameter -
          -- intercept  and scale  are  already  composed in.   Should
          -- rename this to make things slightly less confusing.
  let
    buildHull' x1 x2 =
      Hull left (buildHull' x1 x', buildHull' x' x2) right
         where left @(Line (_, fx1) dfx1)  = line x1
               right@(Line (_, fx2) dfx2)  = line x2
               x'    = x1 + ((fx2 - fx1) - (x2 - x1) * dfx2) / (dfx1 - dfx2)
  in
    buildHull' x1 x2

          -- Now we simply need to project out a set of lines that fit
          -- 'well  enough' for  our purpose.   We start  by observing
          -- that at each  point in the Hull the curve,  in going from
          -- (x1, fx1) to (x2, fx2),  passes through triangle T = ((x,
          -- fx1), (x', fx'), (x, fx2)), so the maximum absolute error
          -- is  bounded by  the _vertical_  distance from  x' to  the
          -- base.  As  a _proportion_ of  the function value  at that
          -- point we have:

hullErrorBound :: Floating a => Hull a -> a
hullErrorBound (Hull (Line (x1, y1) s1)
                     (Hull _ _ (Line (x', y') _), _)
                     (Line (x2, y2) _)) =
  (s1 - (b/a)) * c / ((y1 * c / a) + (y2 * (1 - (c / a))))
    where a = x2 - x1
          b = y2 - y1
          c = x' - x1

          -- then  we  can extract  a  sequence  of lines  defining  a
          -- piecewise linear approximation, with maximum proportional
          -- error maxErr, to the function defining a hull, with:

piecewiseLinearApprox :: (Ord p, Floating p) => Hull p -> p -> [Line p]
piecewiseLinearApprox hull maxErr =
  let
    p h@(Hull _ (dl,dr) r)
      | maxErr > (hullErrorBound h) = [r]
      | otherwise                   = (p dl) ++ (p dr)
  in
    (left hull) : p hull

          -- Now  we  need  a  function that  can  translate  a  value
          -- function (for  one type  into linear constraints  that we
          -- can put in  the model. We have to normalise  the iine (x,
          -- fx, dfx) ->  (0, c m), then the  constraint expectation <
          -- (w .  x)m +  c, is the same as expectation - (m  w) . x <
          -- c.)
          --
          -- Note  that  - like  everything  else  - the  weights  are
          -- exponentiated, in the reference  function, and we have to
          -- take account  of that (since  the weights aren't  part of
          -- the construction of the 'Line's.)
          --
          -- We define what we need for one type, then 'app' it across
          -- the complete thing, collapsing to a single list as we go.
          -- This is kinda dense code - should restructure!
          --
          -- NOTE  HARDWIRED  PRECISION  -  WE EXTRACT  A  CURVE  WITH
          -- MAXIMUM  ERROR   0.01  OF   THE  ACTUAL  VALUE   AT  THAT
          -- POINT. THIS MGHT  BE A BIT MORE DEMANDING  THAN WE REALLY
          -- NEED. SHOULD MAKE THIS CONFIGURABLE.

          -- NEED AN ADT HERE!

type LinearApprox a = [([(a, Mvar)], a)]

buildOneLinearApproximation :: BloodTypes -> ThetaType Double -> LinearApprox Double
buildOneLinearApproximation bloodtype (ThetaType intercept scale decay) =
  let
      buildOneConstraint (Line (x, fx) dfx) =
        (((1, Val bloodtype) : (coefs decay)), fx - x * dfx)
            where coefs decay =
                    toList $ oneConstraint <$>
                               bucketsTimeBucketVector <*>
                                 decayCoefs decay
                  oneConstraint timebucket coef =
                    (- coef * dfx, Keep bloodtype timebucket)
  in
    fmap buildOneConstraint $
           piecewiseLinearApprox (buildHull (line intercept scale) 0 100) 0.05

buildLinearApproximation :: Theta Double -> LinearApprox Double
buildLinearApproximation (Theta theta) =
  foldMap (\x -> x) $ buildOneLinearApproximation <$> typesBTVector <*> theta
 
          --
          ------------------------------------------------------------
          ------------------------------------------------------------
          --
          -- Now the optimization
          --
          -- We have supplies and demands  coming in, which we attempt
          -- to  administer and  serve optimally.  We have  a bank  in
          -- which we keep supplies that we do not use immediately.
          --
          -- Supplies  go staight  into  the bank  (displacing at  the
          -- other   end  supples   that  have   expired,  and   which
          -- disappear),  leaving us  with  demand  of various  types.
          -- This provides the basic configuration of the optimisation
          -- problem:
          --
          -- Given a store and demands,  what is the optimal drawdown,
          -- given that we need to meet all _urgent_ demands.
          --
          ------------------------------------------------------------
          -- 
          -- Start with the  various assignments we have  to track: we
          -- assign  units  to each  combination  of  original type  /
          -- substitution type /  demand type.  We also  need to track
          -- units bought in to cover  urgent demand for which we have
          -- no in-house supplies available.
          --
          -- NOTE ORDER OF FIELDS:
          --
          --    Assign _OriginalType_ _ReplacementType_ Demand Age
          --

data Mvar = Assign BloodTypes BloodTypes DemandTypes AgeBucket
           | BuyIn BloodTypes
           | Keep  BloodTypes AgeBucket
           | Val  BloodTypes
          deriving (Eq, Show, Ord)

allVars :: [Mvar]
allVars = foldr (++) (   [BuyIn t   | t <- range            ]
                      ++ [Keep  t a | t <- range, a <- range]
                      ++ [Val   t   | t <- range            ])
                     (f <$> typesBTVector <*> canBeSubedWith)
          where f i j = [Assign i j' dt a
                          | j' <- j,
                            dt <- range ,
                            a  <- range ]

          --
          ------------------------------------------------------------
          --
          -- and the mixed-integer program itself
          --

lp :: LinearApprox Double -> Store Unit -> BTVector (DemandTypeVector Unit) -> LP Mvar Double
lp =
  let
          -- note that we  put the function bindings down  in the body
          -- of  the let  instead of  on  the left  of the  definition
          -- because we want the compiler to be free to expand as much
          -- as possible all the declarations up front.

          -- so here are the variables and their types and ranges

    termDeclarations =
      sequence_ [f i >> varGeq i 0 | i <- allVars]
        where f i@(Val _) = setVarKind i ContVar
              f i         = setVarKind i IntVar

          -- We  want -  goes without  saying! -  to make  as much  as
          -- possible.  Buyins  cost 200 per unit,  internal drawdowns
          -- priced as defined by g.  The optimization term is thus

    optimisationTerm =
      setObjective $
        linCombination $
          foldr (++)
            (   [(-200, BuyIn t) | t <- range]
             ++ [(1, Val t)      | t <- range]
            )
            (f <$> typesBTVector <*> canBeSubedWith)
              where f i j = [ (g i k dt, Assign i k dt a) |
                                k  <- j,
                                dt <- range,
                                a  <- range]
                    g _  On Elective = 45
                    g _  On Urgent   = 25
                    g t1 t2 Elective | t1 == t2  = 40
                                     | otherwise = 45
                    g t1 t2 Urgent   | t1 == t2  = 20
                                     | otherwise = 25

          -- For each type,  the available resources must  be equal to
          -- sum of assigned and kept resources

    direction = setDirection Max

          -- Note: extra level of  structure in availabilityTerm (also
          -- see remark below about FlexibleContexts).

    availabilityTerms =
      f <$> typesBTVector <*> canBeUsedFor
      where f t tl = fmap g bucketsTimeBucketVector
              where g tb = (linCombination  $
                      (1, Keep t tb) : [ (1, Assign t' t dt tb) |
                                             t' <- tl,
                                             dt <- range])


          -- Once  we  are  past  the  setup,  there  are  four  basic
          -- constraints:

          -- 1. The    unsubstituted    units    assigned    to    the
          --    unsubstitutable urgent demand, together with bought in
          --    units,   should   be   _sufficient_   to   cover   the
          --    unsubstitutable urgent demand.

    urgentNoSubTerm =
      f <$> typesBTVector
      where f t = linCombination ((1, BuyIn t) : [ (1, Assign t t Urgent a)
                                                   | a <- range])

          -- 2. The  units  assigned  to the  urgent  demand in  toto,
          --    together with  bought in  units, should be  _equal_ to
          --    the urgent demand in toto

    urgentTerm =
      f <$> typesBTVector <*> canBeSubedWith
      where f t1 lt2 = linCombination  $ (1, BuyIn t1) : [(1, Assign t1 t2 Urgent a)
                                                           | t2 <- lt2,
                                                             a  <- range]

    urgent x = (urgentNoSub x) + (urgentCanSub x)

          -- 3. the elective demand is _at most_ covered

    electiveTerm =
      f <$> typesBTVector <*> canBeSubedWith
      where f t1 lt2 = linCombination  [(1, Assign t1 t2 Elective a)
                                         | t2 <- lt2, a <- range]

    elective x = (electiveNoSub x) + (electiveCanSub x)

          -- 4. substitutions  can obviously  cover only that  part of
          --    the elective demand that actually is substitutable

    electiveSubTerm =
      f <$> typesBTVector <*> canBeSubedWith 
      where f t1 lt2 = linCombination [(1, Assign t1 t2 Elective a)
                                        | t2 <- lt2,
                                          t1 /= t2,
                                          a <- range]

  in
    \linFuncApprox ->
      \store ->
        \demand ->
          execLPM $ do
            direction
            termDeclarations
            optimisationTerm

            sequence_ $ fmap (\(left, right) -> (leqTo (linCombination left) right)) linFuncApprox

            -- Need  the FlexibleContexts  extension to  get next  two
            -- lines to  typecheck [for  some reason embedded  deep in
            -- the GLPK  bindings they generate complex  type coercion
            -- issues in vanilla Haskell  when we introduce a function
            -- definition  (f)]   (a workaround exists).

            -- THIS IS BAD AND SHOULD BE FIXED - SHOULD SET DEMAND AND
            -- SUPPLY TO  BE FLOATING  AT SOURCE  - SHOULD  NOT COERCE
            -- THEM HERE

            let f at st = (equalTo <$> at <*> st)
            sequence_ . (fmap sequence_) $ f <$> availabilityTerms <*> (fmap (fmap fromIntegral) store)

            sequence_ $ geqTo   <$> urgentNoSubTerm  <*> (fmap (fromIntegral . urgentNoSub)    demand)
            sequence_ $ equalTo <$> urgentTerm       <*> (fmap (fromIntegral . urgent)         demand)
            sequence_ $ leqTo   <$> electiveTerm     <*> (fmap (fromIntegral . elective)       demand)
            sequence_ $ leqTo   <$> electiveSubTerm  <*> (fmap (fromIntegral . electiveCanSub) demand)

          --     
          ------------------------------------------------------------
          ------------------------------------------------------------
          --     
          -- Misc stuff

dot x y = sum ((*) <$> x <*> y)

range :: (Enum a, Ord a, Bounded a) => [a]
range = [minBound .. maxBound]

resetNaN a | a == a    = a
           | otherwise = 0 :: Double

isOK :: RealFloat a => a -> Bool
isOK x = not (isNaN x || isInfinite x || isDenormalized x)

          --
          ------------------------------------------------------------
          ------------------------------------------------------------
          -- 
          -- Running the model
          --
          ------------------------------------------------------------
          --
          -- First, we need a set of scenario data.  We will train the
          -- model with  a lookahead of  n data points, so  we deliver
          -- scenarios in  sequences of  that length, together  with a
          -- random inital bank.

type Scenario  = (Store Unit, EventSequence)

scenarioSeq :: Int -> [ Scenario ]
scenarioSeq n =
  evalState (sequence $ iterate (const m) m) (mkStdGen 1)
    where m = do
            store  <- sampleStore
            stages <- sequence (replicate n sampleOnePeriod)
            return (store, stages)
       
          --
          ------------------------------------------------------------
          --
          -- Training  regime.  In  each   cycle,  given  a  chunk  of
          -- scenarios (and the stuff in the state)
     
type RealisedValue = Double

trainModel :: [Scenario] -> IO ((), Theta Double)
trainModel scenarioSeq =
  runStateT (sequence_ $ fmap trainModelOneIteration scenarioSeq) zeroTheta

rescale store theta grad =
  let
      analyticResult theta = expectation ((fmap (fmap fromIntegral)) store) theta
      f d | (abs (log arTheta - log (analyticResult $ newTheta d))) > 0.5 = f $ d / 2
          | otherwise                                                     = d
         where newTheta d  = (+) <$> theta <*> (fmap (* d) grad)
               arTheta     = analyticResult theta
  in
     f

type OneCycle     a = Store Unit -> EventSequence -> IO ([a])
type OneIteration a = OnePeriod  -> StateT (Store Unit) IO a

trainModelOneIteration :: Scenario -> StateT (Theta Double) IO RealisedValue
trainModelOneIteration (store, periods) = do

  theta <- get

  let valueFunctionEstimate = expectation (fmap (fmap fromIntegral) store) theta
  let linearApproximation = buildLinearApproximation theta

  realisedValue <- liftM sum $ lift $ runOneCycle (runOneIteration linearApproximation) store periods

  let difference = realisedValue - valueFunctionEstimate

  let gradient   = dExpectation ((fmap $ fmap fromIntegral) store) theta
  let magnitude  = (sqrt . sum) $ fmap (^2) gradient

  let delta      = rescale store theta gradient $ difference / magnitude / 500
  let newTheta   = (+) <$> theta <*> (fmap (* delta) gradient)

  put newTheta

  return realisedValue

runOneCycle :: (OneIteration Double) -> OneCycle RealisedValue
runOneCycle runOneIteration =
  \ store ->
    \periods ->
      evalStateT (sequence $ fmap runOneIteration periods) store


runOneIteration :: LinearApprox Double -> OneIteration RealisedValue
runOneIteration linearApproximation =
  let
    assignsTo t ((Assign _ t' _ _),_) = t == t'
    assignsTo _ _                     = False

    isVal ((Val _), _) = True
    isVal _            = False

    aggregateForType a t = round . sum . fmap snd . filter (assignsTo t) $ Data.Map.Internal.toList a
  in
    \(OnePeriod supply demand) ->
      do
        store <- liftM (updateStore supply) get

        (_, Just (v, a)) <-lift $ glpSolveVars mipDefaults (lp linearApproximation store demand)

        let drawdowns   = fmap (aggregateForType a) typesBTVector
        let estimatedFutureValue = sum . fmap snd . filter isVal $ Data.Map.Internal.toList a

        put (drawdownStore drawdowns store)

        return (v - estimatedFutureValue)

runOneIterationBuyIns :: LinearApprox Double -> OneIteration RealisedValue
runOneIterationBuyIns linearApproximation (OnePeriod supply demand) =
  let
    assignsTo t ((Assign _ t' _ _),_) = t == t'
    assignsTo _ _                     = False

    isBuyIn ((BuyIn _), _) = True
    isBuyIn _              = False

    aggregateForType a t = round . sum . fmap snd . filter (assignsTo t) $ Data.Map.Internal.toList a
  in
    do
      store <- liftM (updateStore supply) get

      (_, Just (v, a)) <-lift $ glpSolveVars mipDefaults (lp linearApproximation store demand)

      let drawdowns = fmap (aggregateForType a) typesBTVector
      let buyIns    = sum . fmap snd . filter isBuyIn $ Data.Map.Internal.toList a

      put (drawdownStore drawdowns store)

      return buyIns

          --
          ------------------------------------------------------------
          ------------------------------------------------------------
          --

main = do

          -- Train  model on  500  iterations of  a  scenario with  10
          -- events (and random initial  states) extract the resulting
          -- calibrated  expectation function,  and build  a piecewise
          -- linear approximation.

  valuationFunction <- (trainModel $ take 500 $ scenarioSeq 10) >>= return . buildLinearApproximation . snd

          -- generate a sequence of 1000  events for the test, run the
          -- trained  model  (i.e.  with  the  calibrated  expectation
          -- function) on this, and print the results.

  sample <- evalStateT (sequence (replicate 1000 sampleOnePeriod)) (mkStdGen 0)
  result <- evalStateT (sequence (fmap (runOneIterationBuyIns valuationFunction) sample)) emptyBank
  sequence_ (fmap print result)

          --
          --
          --
          -- END
          --
          ------------------------------------------------------------
          ------------------------------------------------------------
