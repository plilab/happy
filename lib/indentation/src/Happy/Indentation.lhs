> module Happy.Indentation (
>       composeIndentRel,
>       unionIndentRel,
>       composeLookaheadRel,
>       composeLookaheadParentRel,
>       composeLookaheadChildRel,
>       unionLookaheadRel,
>       IndentRel(..),
>       LookaheadRel(..)
>       ) where

An IndentRel is attached to every symbol on the RHS of a grammar rule.

For example, this is a (unannotated) grammar for balanced braces

    expr -> expr expr
    expr -> '[' expr ']'
    expr -> epsilon

To make all braces vertically-aligned, we add the annotations

    expr -> (expr, Any) (expr, Any)
    epxr -> ('[', =) (expr, Any) (']', =)
    expr -> epsilon

> data IndentRel
>       = Eq
>       | Geq
>       | Gt Int
>       | Any
>       deriving (Eq, Show)

> composeIndentRel :: IndentRel -> IndentRel -> IndentRel
> composeIndentRel Any _ = Any
> composeIndentRel _ Any = Any
> composeIndentRel Eq r = r
> composeIndentRel r Eq = r
> composeIndentRel Geq r = r
> composeIndentRel r Geq = r
> composeIndentRel (Gt n) (Gt m) = Gt (n + m)

> unionIndentRel :: IndentRel -> IndentRel -> IndentRel
> unionIndentRel Any _ = Any
> unionIndentRel _ Any = Any
> unionIndentRel Eq Eq = Eq
> unionIndentRel Eq Geq = Geq
> unionIndentRel Eq (Gt 1) = Geq
> unionIndentRel Geq Eq = Geq
> unionIndentRel Geq Geq = Geq
> unionIndentRel Geq (Gt _) = Geq
> unionIndentRel (Gt 1) Eq = Geq
> unionIndentRel (Gt _) Geq = Geq
> unionIndentRel (Gt n) (Gt m) = Gt (min n m)

A LookaheadRel consists of two relations.

> data LookaheadRel = LookaheadRel IndentRel IndentRel 
>                   deriving (Eq, Show)

> composeLookaheadRel :: LookaheadRel -> LookaheadRel -> LookaheadRel
> composeLookaheadRel (LookaheadRel p1 c1) (LookaheadRel p2 c2) =
>   LookaheadRel (composeIndentRel p1 p2) (composeIndentRel c1 c2)

> composeLookaheadParentRel :: IndentRel -> LookaheadRel -> LookaheadRel
> composeLookaheadParentRel r (LookaheadRel p c) =
>   LookaheadRel (composeIndentRel p r) c

> composeLookaheadChildRel :: IndentRel -> LookaheadRel -> LookaheadRel
> composeLookaheadChildRel r (LookaheadRel p c) =
>   LookaheadRel p (composeIndentRel c r)

> unionLookaheadRel :: LookaheadRel -> LookaheadRel -> LookaheadRel
> unionLookaheadRel (LookaheadRel p1 c1) (LookaheadRel p2 c2) =
>   LookaheadRel (unionIndentRel p1 p2) (unionIndentRel c1 c2)