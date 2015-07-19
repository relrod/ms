# ms - metric spaces

[![Build Status](https://travis-ci.org/relrod/ms.svg?branch=master)](https://travis-ci.org/relrod/ms)

A metric space is a set together with some notion of distance function which
obeys four laws. See
[the haddock](https://relrod.github.io/ms/Math-MetricSpace.html) for more
information.

We implement these not using a typeclass, but using a simple datatype. This
avoids needing to create newtypes for every metric implementation over a given
type, meaning that we gain more code re-use and an easier-to-use API.

**WARNING**: The triangle-inequality law does not necessarily hold in the
presence of `Floating a => a`, which we use. This is because `Double` and
`Float` both don't have arbitrary precision. However, some of our metrics, such
as that of `euclidean` distance, require the use of the `sqrt` function, which
requires a `Floating` constraint.

## Usage

```haskell
dist levenshtein "foo" "bar"   -- => 3.0
dist euclidean (V.fromList [3,2]) (V.fromList [2,3])  -- => 1.4142135623730951
```

# License

[BSD-2](LICENSE).
