# hase

Hase is a Haskell binding for the [Senna NLP toolkit](http://ronan.collobert.com/senna),
supporting

  * Part of Speech tagging (POS)
  * Chunking (CHK)
  * Name Entity Recognition (NER)
  * Semantic Role Labeling (SRL)

### Getting Started

At first, please download [Senna](http://ronan.collobert.com/senna) and extract the
archive content into `foreign/senna`.

Then run

    cabal configure
		cabal build

to build hase and the example program. You can execute the example program
by running

   ./dist/build/example/example

If you want to install this package, run

    cabal install

after build.

### License

hase is released under MIT license.
You can find a copy of the MIT License in the [LICENSE](./LICENSE) file.