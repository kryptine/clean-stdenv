definition module Rational

import	ExtendedArithBasics
from	StdOverloaded import ==, <, +, -, zero, *, /, one, abs, sign, ~, toReal
from	BigInt import BigInt, toBigInt

:: Rational

// comparison

instance ==			Rational
instance <  		Rational

// arithmetic

instance +			Rational
instance -  		Rational
instance zero		Rational

instance *  		Rational
instance /			Rational
instance one		Rational

instance abs		Rational
instance sign		Rational
instance ~			Rational

// specialised versions for operations with 32 bit numbers

instance EqInt		Rational
instance LessInt	Rational
instance GreaterInt	Rational
instance +%			Rational
instance -%			Rational
instance %-			Rational
instance *%			Rational
instance ^%			Rational

// creating rationals

class (/:) infix 7 a :: !a !a -> Rational	// second argument must not be zero
class toRational   a ::    !a -> Rational

instance /: 		Int
instance /: 		BigInt
instance toRational Int
instance toRational BigInt

// conversion

instance toReal		Rational

numerator 	:: !Rational -> BigInt			//	gcd (numerator x) (denominator x) == one
denominator :: !Rational -> BigInt			//	(numerator x)==zero -> (denominator x) == one
											//	sign x == sign (numerator x)

