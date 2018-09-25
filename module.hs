import Data.List --All list functions
import Data.List (sort) --Only imports sort
import Data.List hiding (sort) --Imports all functions exept sort
import Data.List qualified --Data.List.sort to invoke function
import Data.List qualified as L

module test (
	a,
	b,
	c) where

a :: Int -> Int
a n = n+1