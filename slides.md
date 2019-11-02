# Lazy Evaluation

Got.λ - Gothenburg Functional Programming Meetup

Johan Lodin, 2019-10-30

github.com/jolod

## JavaScript example

```javascript
function foo(x, y) {
  return;
}

foo(console.log("Hello"), console.log("World"))
```

--

```text
Hello
World
```

## R example

```r
foo <- function (x, y) {
  #x
  #y
  #x
  return;
}

foo(print("Hello"), print("World"))
```

--

```text
```

--

```text
Hello
```

--

```text
Hello
World
```

--

```text
Hello
World
```

`print` returns the argument.

## Haskell example

```haskell
foo x y = do
  --x
  --y
  --x
  return ()

main = do
  foo (putStrLn "Hello") (putStrLn "World")
```

--

```text
```

--

```text
Hello
```

--

```text
Hello
World
```

--

```text
Hello
World
Hello
```

But Haskell is lazy?!

## Explanation of the Haskell code

```haskell
foo x y = do
  x
  y
  x
  return ()
```

is equivalent to

```haskell
foo x y = do
  _ <- x
  _ <- y
  _ <- x
  return ()
```

and `<-` is *not* `=`:

```haskell
foo x y =
  x >>= (\_ -> y >>= (\_ -> x >>= (\_ -> return ())))
```

`putStrLn` is not an action in itself. `IO a` is essentially `Runtime -> a` and only the Haskell runtime holds a `Runtime` value.

--

**_In a pure language you don't directly observe the laziness._**

Lazy evaluation ⇒ side effects!

## Elaborate R example

```r
f <- function(x, y) {
  if (x) {
    y
  }
}

f(z <- TRUE, z <- FALSE)
print(z)

f(z <- FALSE, z <- TRUE)
print(z)
```

--

```text
[1] FALSE
[1] FALSE
```

```r
g <- function(x) {
  f(x, x)
}

a <- g(print(TRUE)) # print(TRUE) returns TRUE
```

--

```text
[1] TRUE
```

```r
f <- function(x, y) {
  return
}

a <- g(print(TRUE)) # g references the *new* f.
```

## Explicitly delaying execution (1/2)

```javascript
// Define a *thunk*.
x = function() {
  console.log("Computing!");
  return 3;
};
console.log(x());
console.log(x());
```

```text
Computing!
3
Computing!
3
```

## Explicitly delaying execution (2/2)

```javascript
function memoize(f) {
    return function() {
        if (this.has_value) {
            return this.value;
        }
        else {
            this.value = f();
            this.has_value = true;
            return this.value;
        }
    };
}

y = memoize(x);
console.log(y());
console.log(y());
```

```text
Computing!
3
3
```

## Thunks

Thunks (without memoization) were first described for the implementation of ALGOL 60.

Published in 1961: *Thunks: a way of compiling procedure statements with some comments on procedure declarations*

A naïve way to implement lazy evaluation: thunk and memoize every expression, and invoke where used.

AFAIU, memoized thunks are used in GHC's implementation of lazy evaluation.

# In Practice

Modularity through lazy evaluation

## Streams

**Streams invert control flow**

Typical for lazy data.

The *consumer* decides how much of the stream that needs to be realized.

--

However, streams are *not* lazy evaluation.

### Streams - Python

```python
from itertools import islice

def take(n, xs):
  return list(islice(xs, n))

def factorials():
  (n, p) = (0, 1);
  while True:
    yield (n, p)
    m = n + 1
    (n, p) = (m, m * p)

fs = factorials()

print(take(6, fs))
print(take(6, fs))
```

--

```text
[(0, 1), (1, 1), (2, 2), (3, 6), (4, 24), (5, 120)]
[(6, 720), (7, 5040), (8, 40320), (9, 362880), (10, 3628800), (11, 39916800)]
```

:-(

Same in e.g. Java.

Comment: `yield` effectively turns the return value of `factorial` into an object (of type `generator`) which mutates every time the `__next__` method is called, which is usually hidden behind syntax such as `for ... in ...`.

### Streams - Clojure

```clojure
(defn factorials []
  (iterate (fn [[n p]]
             (let [m (inc n)]
               [m (* m p)]))
           [0 1]))

(let [xs (factorials)]
  (println (take 6 xs))
  (println (take 6 xs)))
```

```text
([0 1] [1 1] [2 2] [3 6] [4 24] [5 120])
([0 1] [1 1] [2 2] [3 6] [4 24] [5 120])
```

:-)

## (Corecursion)

```haskell
import Data.Foldable

main = do
  for_ (take 6 $ factorials ()) $ \n ->
    putStrLn $ show n

factorials () = iterate f (0, 1)
  where
  f (n, p) =
    let m = n + 1
    in (m, m * p)
```

```text
(0,1)
(1,1)
(2,2)
(3,6)
(4,24)
(5,120)
```

Accumulator arguments + tail calls ≈ iteration.

Corecursion ≈ iteration.

Codata ≈ the loop variables at every iteration.

Accumulator necessarily terminates. Codata can be infinite.

(Why do I use `()`?)

## Algorithms ("real world" example)

We will look at solving boolean expressions, because I wrote such code, and it showcases laziness well.

* `P && (P || Q)`
* `P && (!P || Q)`
* `P && (Q || R)`

etc.

Similar to deciding the next move in e.g. board games.

## Conjunctive normal form (CNF)

```text
CNF = DISJ && DISJ && ...
```

```text
DISJ = LIT || LIT || ...
```

```text
LIT = VAR
LIT = !VAR
```

--

```haskell
data Literal a = Positive a | Negative a
type CNF a = Set (Set (Lit a)) -- a.k.a. clausal normal form.
```

--

Any boolean expression can be rewritten to CNF.

## CNF-SAT solver (DPLL)

**Reduction**

* `{P} && { P, ...} && ...` ⇒ `P` and `...`
* `{P} && {!P, ...} && ...` ⇒ `P` and `{...} && ...`
* `{} && ...` ⇒ Contradiction
* (`!!P` ⇒ `P`)

--

**Branching**

Given `{P, Q, ...} && ...`

* `{ P} && {P, Q, ...} && ...` ⇒ `P` and `...`
* `{!P} && {P, Q, ...} && ...` ⇒ `!P` and `{Q, ...} && ...`

--

**Done**

* Contradiction, or
* no clauses left.

The literals extracted from the unit clauses (singleton sets) is the solution.

## Algorithms (1/2)

See the repository at *github.com/jolod* for the complete code.

```haskell
data Reduction a = Contradiction
                 | NoUnits
                 | Unit a (CNF a)

reduce :: Inv a => CNF a -> Reduction a

branch :: Inv a => CNF a -> Maybe (CNF a, CNF a)
```

--

```haskell
data Solver a = Solver
  { units :: [a]
  , cnf :: CNF a }
  deriving Show

solve :: Inv a => Solver a -> [[a]]
solve solver =
  case reduce (cnf solver) of
    Contradiction -> -- Contradiction.
      mempty
    Unit x cnf -> -- Reduction performed, repeat.
      solve Solver { units = x : units solver
                   , cnf = cnf }
    NoUnits -> -- No reduction performed, attempt to branch.
      case branch (cnf solver) of
        Nothing -> -- Nothing to branch on; solution found.
          pure $ units solver
        Just (left, right) -> -- Explore both branches.
          solve solver {cnf = left} <> solve solver {cnf = right}
```

--

```haskell
aSolution :: Inv a => Solver a -> Maybe [a]
aSolution = head' . solve
```

## Algorithms (2/2)

PureScript is like Haskell, but not lazy.

To write `aSolution` efficiently in PureScript you need to reimplement `solve` but change

```haskell
solve solver {cnf = left} <> solve solver {cnf = right}
```

to

```haskell
case aSolution' solver {cnf = left} of
  Just solution ->
    Just solution
  Nothing ->
    aSolution' solver {cnf = right}
```

## Algorithms - bonus

PureScript's `Set` implementation is taken from Haskell:

```haskell
-- PureScript
instance ordSet :: Ord a => Ord (Set a) where
  compare s1 s2 = compare (toList s1) (toList s2)
```

`toList` is *not* lazy in PureScript.

What happens when you have a set of sets?

```haskell
type CNF a = Set (Set (Lit a))
```

## Control structures (1/4)

```haskell
if' True  t _ = t
if' False _ e = e
```

"Just a neat trick?"

--

```haskell
if (a || b || c)
  then ...
  else ...
```

```haskell
case (a || b || c) of
  True -> ...
  False -> ...
```

--

**`Alternative`: "A monoid on applicative functors"**

```haskell
instance Alternative Maybe where
  empty = Nothing
  (<|>) (Just t) _ = Just t
  (<|>) Nothing  e = e
```

--

```haskell
case (a <|> b <|> c) of
  Just x -> ...  -- then
  Nothing -> ... -- else
```

## Control structures (2/4)

`Alternative` generalizes `if` in two ways:

* Works on rich values rather than booleans.
* Need not choose - can do both.

--

```haskell
instance Alternative [] where
  empty = []
  (<|>) = (++)
```

--

Could have used `Alternative` in `solve`. *Usage* dictates how solutions are collected.

```haskell
solve' :: (Inv a, Alternative f) => Solver a -> f [a]
solve' solver =
  case reduce (cnf solver) of
    Contradiction -> -- Contradiction.
      Alternative.empty
    Unit x cnf -> -- Reduction performed, repeat.
      solve' Solver { units = x : units solver
                    , cnf = cnf }
    NoUnits -> -- No reduction performed, attempt to branch.
      case branch (cnf solver) of
        Nothing -> -- Nothing to branch on; solution found.
          pure $ units solver
        Just (left, right) -> -- Explore both branches.
          solve' solver {cnf = left} <|> solve' solver {cnf = right}
```

Remember how we replaced `<>` with a `case` (i.e. an `if`) to get just one solution?

## Control structures (3/4)

Short-circuiting `or` with nil punning is a special case of `Alternative` or `Monoid`.

Example use cases:
* Parsing JSON with different versions.
* Reading files from different locations (e.g. user vs system).

## Control structures (4/4)
```haskell
data Result a b = Failure a | Success b
  deriving Show

instance Semigroup a => Semigroup (Result a b) where
  Failure xs <> Failure ys = Failure (xs <> ys)
  Success x <> _ = Success x
  _ <> Success y = Success y

instance Monoid a => Monoid (Result a b) where
  mempty = Failure mempty
```

--

```haskell
noop = Failure []
failure x = Failure [x]
success = Success

result1 :: Result [String] Integer
result1 = noop <> failure "Hello" <> failure "World"

result2 = result1 <> success 42 <> failure "Ignored"

main = do
  putStrLn $ show result1
  putStrLn $ show result2
```

```text
Failure ["Hello","World"]
Success 42
```

# Tracing the Roots

λ-calculus ahead

## λ-calculus terms

A term in λ-calculus is either

* `x`, a variable (placeholder)
* `λx. t`, an abstraction ("binds" `x` in `t`)
* `(a b)`, an application ("inserts" `b` into `a`)

For "programs", all terms are ultimately functions!

## Reduction

An abstraction in an application can be *reduced*:

```text
((λx. ??? x ???) y)

??? y ???
```

Inner abstractions *shadow* variables.

```text
((λx. (λx. x)) y)

λx. x
```

Show [interactive example](https://www.cl.cam.ac.uk/~rmk35/lambda_calculus/lambda_calculus.html).

## Reduction

Interestingly, the reduced term need not be smaller.

```text
((λx. (x x)) (λx. (x x)))
```

--

It can in fact be *larger*:

```text
((λx. ((x x) x)) (λx. ((x x) x)))
```

## The K combinator

```text
K = λa. (λb. a)
```

Always returns `a`.

```text
((K a) ((λx. (x x)) (λx. (x x))))
```

Should reduce to just `a`.

Or does it?

## Reduction strategy

* Normal (leads to a *normal* form if it exists)
* Applicative order (might never terminate)

Lazy evaluation is not a reduction strategy, but kind of.

## Implementations

```javascript
// JavaScript - applicative order

K = a => b => a
K(3)(5) // 3

omega = x => x(x)
K(3)(omega(omega)) // stack overflow
```

--

```R
# R - lazy

K <- function(a) function(b) a;
print(K(3)(omega(omega))); # 3

omega <- function(x) x(x);
print(K(3)(omega(omega))); # 3
```

--

```haskell
-- haskell - lazy

K = \a -> \b -> a
K 3 5 -- 3

omega = \x -> x x -- cannot construct the infinite type: t ~ t -> t1
```

## Applicative order ⇒ no recursion

In λ-calculus, the Y combinator never terminates for applicative order reduction.

By definition:

```text
Y f = f (Y f)
```

so

```text
Y f x
f (Y f) x
f (f (Y f)) x
f (f (f (Y f))) x
...
```
--

* No recursion ⇒ not Turing complete.
* Simply typed lambda calculus is *not* Turing complete.
* The Y combinator does not type check in Haskell.
* Exploited by the functional configuration language *Dhall*.

Forced to use normal order ...

## ... but normal order is inefficient

```text
dupFirst x y = (x, x)
```

Applicative order:

```text
dupFirst (2 + 3, factorial 40)
dupFirst (5, factorial 40)
dupFirst (5, 815915283247897734345611269596115894272000000000)
(5, 5)
```

--

Normal order:

```text
dupFirst (2 + 3, factorial 40)
(2 + 3, 2 + 3)
(5, 2 + 3)
(5, 5)
```

--

Lazy evaluation:

```text
dupFirst (2 + 3, factorial 40)
(2 + 3, 2 + 3)
(5, 5)                               [magic!]
```

--

And that's why we have lazy evaluation! ;-)

# THE END

Thank you!

github.com/jolod

[lc-evaluator]: https://www.cl.cam.ac.uk/~rmk35/lambda_calculus/lambda_calculus.html
