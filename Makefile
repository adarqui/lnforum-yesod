build:
	stack build --fast

clean:
	stack clean

build-watch:
	stack build --fast --file-watch

docs:
	cabal haddock --hyperlink-source

exec_prod:
	./bin/ln-yesod config/settings.yml

exec_prod_sudo:
	sudo ./bin/ln-yesod config/settings/production.yml

exec_dev_sudo:
	sudo stack exec --allow-different-user -- yesod devel -p 80 --tls-port 443

ghci:
	sudo stack ghci ln-yesod --main-is none --fast --allow-different-user

install:
	stack install --local-bin-path=./bin

install-yesod:
	stack install yesod-bin cabal-install --install-ghc
