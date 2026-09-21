> module Happy.Indentation (
>       composeIndentRel,
>       unionIndentRel,
>       composeLookaheadRel,
>       unionLookaheadRel,
>       IndentRel(..),
>       LookaheadRel(..)
>       ) where

I will split these into different files later on

There are more relations, but these will do for now

An IndentRel is attached to every symbol on the RHS of a grammar rule.

> data IndentRel
>       = Eq
>       | Geq -- TODO add a (Geq n)
>       | Gt Int
>       | Splash
>       deriving (Eq)

> instance Show IndentRel where
>   show Eq = "="
>   show Geq = ">="
>   show (Gt n) = concat (replicate n ">")
>   show Splash = "*"

> composeIndentRel :: IndentRel -> IndentRel -> IndentRel
> composeIndentRel Splash _ = Splash
> composeIndentRel _ Splash = Splash
> composeIndentRel Eq r = r
> composeIndentRel r Eq = r
> composeIndentRel Geq r = r
> composeIndentRel r Geq = r
> composeIndentRel (Gt n) (Gt m) = Gt (n + m)

> unionIndentRel :: IndentRel -> IndentRel -> IndentRel
> unionIndentRel Splash _ = Splash
> unionIndentRel _ Splash = Splash
> unionIndentRel Eq Eq = Eq
> unionIndentRel Eq Geq = Geq
> unionIndentRel Eq (Gt _) = Geq -- TODO factor in Gt n
> unionIndentRel Geq Eq = Geq
> unionIndentRel Geq Geq = Geq
> unionIndentRel Geq (Gt n) = Gt n
> unionIndentRel (Gt _) Eq = Geq -- TODO factor in Gt n
> unionIndentRel (Gt n) Geq = Gt n
> unionIndentRel (Gt n) (Gt m) = Gt (min n m)

A LookaheadRel consists of two relations.

> data LookaheadRel = LookaheadRel IndentRel IndentRel 
>                   deriving (Eq)

> instance Show LookaheadRel where
>   show (LookaheadRel parentRel childRel) = "<" ++ show parentRel ++ " " ++ show childRel ++ ">"

> composeLookaheadRel :: LookaheadRel -> LookaheadRel -> LookaheadRel
> composeLookaheadRel (LookaheadRel p1 c1) (LookaheadRel p2 c2) =
>   LookaheadRel (composeIndentRel p1 p2) (composeIndentRel c1 c2)

> unionLookaheadRel :: LookaheadRel -> LookaheadRel -> LookaheadRel
> unionLookaheadRel (LookaheadRel p1 c1) (LookaheadRel p2 c2) =
>   LookaheadRel (unionIndentRel p1 p2) (unionIndentRel c1 c2)