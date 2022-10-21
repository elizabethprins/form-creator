# Form Creator

Work in progress, based on the Online Form Creator of Google Forms.
Add, edit, and remove questions using  [List.Zipper](https://package.elm-lang.org/packages/wernerdegroot/listzipper/latest/), and reorder them using [DnDList](https://package.elm-lang.org/packages/annaghi/dnd-list/latest/).

## Includes

1. [Livereload](https://github.com/napcs/node-livereload)
2. [Serve](https://github.com/zeit/serve/)
3. [Sass](https://sass-lang.com/install)
4. [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/2.6.1/)

## Getting started

* `make deps` - Install dependencies
* `make` - Compile all Elm and Scss files
* `make watch` - Start livereload and serve app, makes use of [entr](https://formulae.brew.sh/formula-linux/entr)
* `make build` - Build for production
