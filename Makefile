
hakyll-build:
	nix-shell -A website-shell --run 'site build'

hakyll-watch:
	nix-shell -A website-shell --run 'site watch --port 5555'

clean:
	nix-shell -A website-shell --run 'site clean'

download-agdalib:
	wget https://github.com/agda/agda-stdlib/archive/v0.17.tar.gz
	tar xzvf v0.17.tar.gz
	rm -f v0.17.tar.gz
	mv agda-stdlib-0.17 agdalib

yarn-install:
	nix-shell -A yarn-shell --run 'yarn install'

webpack-build:
	nix-shell -A yarn-shell --run 'yarn build'

webpack-build-prod:
	nix-shell -A yarn-shell --run 'yarn build-prod'

webpack-watch:
	nix-shell -A yarn-shell --run 'yarn watch'
