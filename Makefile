build:
	stack build --fast

clean:
	stack clean

build-watch:
	stack build --fast --file-watch

docs:
	cabal haddock --hyperlink-source

exec-prod:
	~/.local/bin/ln-yesod ./config/settings/production.yml ./config/settings/role/web.yml ./private/settings/production.yml

exec-prod-sudo:
	sudo ./bin/ln-yesod ./config/settings/production.yml ./config/settings/role/web.yml ./private/settings/production.yml

exec-dev-sudo:
	sudo stack exec --allow-different-user -- yesod devel -p 80 --tls-port 443

exec-dev-sudo-exe:
	sudo ~/.local/bin/ln-yesod ./config/settings/dev.yml ./config/settings/role/web.yml ./private/settings/dev.yml

exec-dev-workers-exe:
	~/.local/bin/ln-bg ./config/settings/dev.yml ./config/settings/role/worker.yml ./private/settings/dev.yml

exec-prod-workers-exe:
	~/.local/bin/ln-bg ./config/settings/production.yml ./config/settings/role/worker.yml ./private/settings/production.yml

ghci:
	sudo stack ghci ln-yesod --main-is none --fast --allow-different-user

install:
	stack install

install-yesod:
	stack install yesod-bin cabal-install --install-ghc
