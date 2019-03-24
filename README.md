# lnforum-yesod

***LN Project: Major work in progress***

## what?

This is the backend for ln.

## important

requires cabal 1.22.8.x. cabal 1.24.x.x causes problems with yesod-devel.

## building

```
stack build
```

## running

```
make exec-dev-sudo
```

## stack

```
stack install yesod-bin cabal-install --install-ghc
^ check ~/.local/bin

stack exec -- yesod init --bare && stack init

stack build && stack exec -- yesod devel

stack build

Launch devel server: stack exec -- yesod devel
View your Yesod site at http://localhost:3000/
```
