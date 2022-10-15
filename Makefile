
hakyll-build:
	nix-shell -A website-shell --run 'site build'

hakyll-watch:
	nix-shell -A website-shell --run 'site watch --port 5001'

clean:
	nix-shell -A website-shell --run 'site clean'

yarn-install:
	nix-shell -A yarn-shell --run 'yarn install'

webpack-build:
	nix-shell -A yarn-shell --run 'yarn build'

webpack-build-prod:
	nix-shell -A yarn-shell --run 'yarn build-prod'

webpack-dev-server:
	nix-shell -A yarn-shell --run 'yarn dev-server'

comments:
	nix-shell -A website-shell --run "gunicorn -b 127.0.0.1:5002 --reload \"comments:App(posts_dir='./_site/posts', comments_dir='./_site/assets/comments', password='test')\""