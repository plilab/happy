-----------------------------------------------------------------------------
Implementation of FIRST

(c) 1993-2001 Andy Gill, Simon Marlow
-----------------------------------------------------------------------------

> module Happy.Tabular.First ( mkFirst, mkClosure ) where

> import Happy.Tabular.NameSet ( NameSet )
> import qualified Happy.Tabular.NameSet as Set
> import Happy.Grammar
> import Data.Maybe (fromMaybe)

> import Data.Map ( Map )
> import qualified Data.Map as Map hiding ( Map )
> import Happy.Indentation

\subsection{Utilities}

joinSymSets with specified default element and union function

> joinSymSets :: b -> (b -> b -> b) -> (a -> Map Name b) -> [a] -> Map Name b
> joinSymSets def union f = foldr go (Map.singleton epsilonTok def) . map f
>    where
>       go h b
>           | Map.member epsilonTok h = Map.unionWith union (Map.delete epsilonTok h) b
>           | otherwise = h

@mkClosure@ makes a closure, when given a comparison and iteration loop.
It's a fixed point computation, we keep applying the function over the
input until it does not change.
Be careful, because if the functional always makes the object different,
This will never terminate.

> mkClosure :: (a -> a -> Bool) -> (a -> a) -> a -> a
> mkClosure eq f = until (\x -> eq x (f x)) f

\subsection{Implementation of FIRST}

The env variable is a mapping from symbols to indented FIRST sets.

For example, if the initial item set is
    S' -> . S>=
    S -> . A>
    A -> . a= B>
    A -> . B> c>
    B -> . b>
    B -> . eps
Then FIRST(A) = { a=, b>>, c> }.

When a lookahead is attached to the symbol, then the FIRST set needs to consider this indentation

FIRST( (>=, >, A) ) = { (>=, >, a), (>=, >>>, b), (>=, >>, c) }.

The child relation gets "shifted down" to account for the relationship of A with its ancestor node (>).

> mkFirst :: Grammar e -> [(Name, LookaheadRel)] -> Map Name LookaheadRel
> mkFirst (Grammar { first_term = fst_term
>                  , lookupProdNo = prodNo
>                  , lookupProdsOfName = prodsOfName
>                  , non_terminals = nts
>                  })
>       = joinSymSets (LookaheadRel Eq Eq) 
>                     unionLookaheadRel 
>                     (\(name, r@(LookaheadRel p c)) -> maybe (Map.singleton name r) 
>                                                             (Map.map (\rel -> LookaheadRel p (rel `composeIndentRel` c))) 
>                                                             (lookup name env))
>   where
>       env :: [(Name, Map Name IndentRel)]
>       env = mkClosure (==) (updateFirstSets fst_term prodNo prodsOfName) [(name, Map.empty) | name <- nts]

> updateFirstSets :: Name -> (a -> Production e) -> (Name -> [a]) -> [(Name, Map Name IndentRel)]
>                 -> [(Name, Map Name IndentRel)]
> updateFirstSets fst_term prodNo prodsOfName env = [ (nm, nextFstSet nm)
>                                                   | (nm,_) <- env ]
>    where
>       terminalP :: Name -> Bool
>       terminalP s = s >= fst_term

>       currFstSet :: (Name, IndentRel) -> Map Name IndentRel
>       currFstSet i@(s, rel) | s == errorTok || s == catchTok || terminalP s = Map.singleton s rel
>                             | otherwise = maybe (error "attempted FIRST(e) :-(")
>                                                 (Map.map id) 
>                                                 (lookup s env)

>       nextFstSet :: Name -> Map Name IndentRel
>       nextFstSet s | terminalP s = Map.singleton s Eq
>                    | otherwise   = foldr (Map.unionWith unionIndentRel) Map.empty
>                                          [ joinSymSets Eq unionIndentRel currFstSet rhs
>                                          | rl <- prodsOfName s
>                                          , let Production _ rhs _ _ = prodNo rl] -- a list of FIRST sets, for all possible rules
