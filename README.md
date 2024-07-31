A simple random test case generator for
[MessageFormat 2.0](https://github.com/unicode-org/message-format-wg/),
written in Haskell.

The generator is based on [the ABNF grammar](https://github.com/unicode-org/message-format-wg/blob/main/spec/message.abnf) as of [this commit](https://github.com/unicode-org/message-format-wg/commit/fdfed1b393c4d0a91b7c62bf7f3d5613a6230306).
Any future grammar changes will require updating this generator
to provide test coverage.

Some grammar productions are simplified to produce more readable test cases.
Those have been noted throughout.

== Usage ==

This is meant to be run as a script, but could be made
into a proper Cabal package in the future.

Requires any reasonably modern GHC version.

Install the `random` package if necessary:

$ cabal install --lib random

Then run the script:

$ ghc --run Main.hs
{ "src" : "\\|{#T:l u-.z:w = $z A-.7=n.-8 x.39:O=$L-.8 @w7-d = || @E:q @H.Yk = $V }{#t:c.-c @t-3M @dc4-=K8H4 @B:Q..-}{:H:O0.V E = $s q---:L =|A #| S = |\\\\\\\\{| @w:p-.Z =|{{\\|| @m1U.:cP02 =$s @q }" },
[...]

Adjust `maxLen`, `weight`, and `numTests` in the code as desired
(these could be made into command-line flags in the future).
