{-# LANGUAGE NoMonomorphismRestriction #-}
module AltPrelude ( module AltPrelude
                  , module Prelude
                  , module Control.Arrow 
                  , module Control.Applicative 
                  , module Control.Monad
                  , module Data.Functor
                  , module Data.Function
                  , module Data.Maybe
                  , module Data.List
                  , module Data.Monoid
                  , module Control.Category
                  , module Data.Foldable
                  , module Data.Traversable
                  ) where

import qualified Prelude as P

-- generalized functions
import Control.Category

-- generalized lists
import Data.Foldable hiding (foldl1, foldr1)
import Data.Traversable

-- import the rest, hiding stuff we're replacing
import Prelude hiding ( (.), id, const
                      , foldr, foldr1, foldl, foldl1
                      , elem, notElem
                      , sum, product, and, or, all, any, maximum, minimum
                      , concat, concatMap
                      , mapM, mapM_, sequence, sequence_
                      
                      -- also hide terrible functions I don't want to look at
                      -- see below for corrected versions
                      , head, tail, last, init, (!!)
                      , scanl1, scanr1, cycle
                      , read
                      )

-- reexport some misc. stuff
import Control.Arrow ( first, second, (***), (&&&)
                     , left, right, (+++), (|||)
                     )
import Control.Applicative (Applicative(..), liftA2, liftA3)
import Control.Monad ( (=<<), (<=<), (>=>), join
                     , liftM, liftM2, liftM3, liftM4
                     , guard, unless, when
                     , replicateM, void
                     , MonadPlus(..)
                     )
import Data.Functor ((<$>))
import Data.Function (fix, on)
import Data.Maybe (maybe, fromMaybe, catMaybes, mapMaybe)
import Data.List (unfoldr, intersperse, intercalate)
import Data.Monoid (Monoid(..))

-- stuff that's not exported
import Text.ParserCombinators.ReadPrec
import Text.ParserCombinators.ReadP as P
import Text.Read

-- improved list functions
head []    = Nothing
head (x:_) = Just x

tail []     = Nothing
tail (_:xs) = Just xs

last []     = Nothing
last [x]    = Just x
last (x:xs) = last xs

init []     = Nothing
init [x]    = Just []
init (x:xs) = (x:) <$> init xs

cycle :: [a] -> Maybe [a]
cycle [] = Nothing
cycle xs = Just $ P.cycle xs

atIndex :: [a] -> Int -> Maybe a
atIndex xs n = head $ drop n xs


-- improved "read" functions
readEither :: (Read a) => String -> Either String a
readEither s =
  case [ x | (x,"") <- readPrec_to_S read' minPrec s ] of
    [x] -> Right x
    []  -> Left "no parse"
    _   -> Left "ambiguous parse"
 where
  read' = readPrec >>= ignore (lift P.skipSpaces)

read :: (Read a) => String -> Maybe a
read s = case readEither s of 
    Left  _ -> Nothing
    Right x -> Just x

-- generalizing lists to foldable
size :: (Integral z, Foldable f) => f a -> z
size = fromIntegral . length . toList


-- better names
infixr 6 <>
(<>) :: (Monoid a) => a -> a -> a
(<>) = mappend

unit :: (Monoid a) => a
unit = mempty

-- monad utility functions
ignore :: (Monad m) => m b -> a -> m a
ignore m x = m >> return x

whenJust :: (Monad m) => (a -> m b) -> Maybe a -> m ()
whenJust _ Nothing = return ()
whenJust f (Just x) = f x >> return ()

untilM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a
untilM p f x | p x       = return x
             | otherwise = untilM p f =<< f x

unfoldM :: (Monad m) => (b -> m (Maybe (a, b))) -> b -> m [a]
unfoldM f b = do x <- f b
                 case x of Just (a, b') -> liftM (a:) (unfoldM f b)
                           Nothing      -> return []

iterM :: (Monad m) => m (Maybe a) -> m [a]
iterM mx = step =<< mx
  where step (Just x) = liftM (x:) (iterM mx)
        step Nothing  = return []

iterWhileM :: (Monad m) => (a -> Bool) -> m a -> m [a]
iterWhileM p mx = iterM $ liftM (justIf p) mx

iterM_ :: (Monad m) => m Bool -> m ()
iterM_ mb = step =<< mb
  where step True  = iterM_ mb
        step False = return ()

iterWhileM_ :: (Monad m) => (a -> Bool) -> m a -> m ()
iterWhileM_ p mx = iterM_ (liftM p mx)

ensure :: (MonadPlus m) => (a -> Bool) -> a -> m a
ensure p x = ignore (guard $ p x) x

-- other assorted utility functions
enumerate :: (Enum a, Bounded a) => [a]
enumerate = [minBound .. maxBound]

infixr 8 ??
(??) :: Maybe a -> a -> a
(??) = flip fromMaybe

justIf :: (a -> Bool) -> a -> Maybe a
justIf p x | p x 	   = Just x
           | otherwise = Nothing

takeUntilDiff :: (a -> a -> Bool) -> [a] -> [a]
takeUntilDiff f (x1:x2:xs) | f x1 x2   = [x1]
                           | otherwise = x1:takeUntilDiff f (x2:xs)
takeUntilDiff _ xs = xs

between :: (Ord a) => a -> a -> a -> Bool
between a z = (&&) <$> (> a) <*> (< z)

eitherF :: (Functor f) => (a -> f c) -> (b -> f d) -> Either a b -> f (Either c d)
eitherF f g = either (fmap Left . f) (fmap Right . g)


