
hakyll-build:
	nix-shell -A website-shell --run 'site build'

hakyll-watch:
	nix-shell -A website-shell --run 'site watch --port 5555'

clean:
	nix-shell -A website-shell --run 'site clean'

yarn-install:
	nix-shell -A yarn-shell --run 'yarn install'

webpack-build:
	nix-shell -A yarn-shell --run 'yarn build'

webpack-build-prod:
	nix-shell -A yarn-shell --run 'yarn build-prod'

webpack-watch:
	nix-shell -A yarn-shell --run 'yarn watch'
