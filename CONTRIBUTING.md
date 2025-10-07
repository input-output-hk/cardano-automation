## Updating dependencies

### ... from Hackage

Updating package dependencies from Hackage should work like normal in a
Haskell project. The most important thing to note is that we pin the
`index-state` of the Hackage package index in `cabal.project`. This
means that cabal will always see Hackage “as if” it was that time,
ensuring reproducibility. But it also means that if you need a package
version that was released *after* that time, you need to bump the
`index-state` (and to run `cabal update` locally).

Because of how we use Nix to manage our Haskell build, whenever you do this you
will also need to pull in the Nix equivalent of the newer `index-state`. You can
do this by running `nix flake update hackageNix`.

