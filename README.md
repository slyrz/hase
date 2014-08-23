# hase

Hase provides Haskell bindings for the [Senna NLP toolkit](http://ronan.collobert.com/senna),
supporting

  * Part of Speech tagging (POS)
  * Chunking (CHK)
  * Name Entity Recognition (NER)
  * Semantic Role Labeling (SRL)

### Getting Started

At first, please download [Senna](http://ronan.collobert.com/senna) and
extract the Senna source files into `foreign/senna/`. The `tar` command below
does just that.

    tar -xzf senna-v3.0.tgz -C foreign/

Then run

    cabal configure
    cabal build

to build hase. You can run the example program by executing

    cabal run example

Finally, if you want to install this package, run

    cabal install

### Usage

Here's a simple example to tokenize and process a sentence read from stdin.

```haskell
module Main where
import NLP.Senna

main =
  withContext $ \ctx -> do
    tokenize ctx =<< getLine
    print =<< (process ctx :: IO [Token])
    print =<< (process ctx :: IO [Maybe POS])
    print =<< (process ctx :: IO [(CHK, Phrase)])
    print =<< (process ctx :: IO [(NER, Phrase)])
    print =<< (process ctx :: IO [[(SRL, Phrase)]])
```

The *Context* type holds the internal C library state.
You can either manage *Context* manually by calling *createContext*
and *freeContext*, or you can use *withContext*, which automatically
creates and frees the *Context* for you.

Once the *Context* is created, you pass a sentence to the *tokenize* function.
The *tokenize* function sets up *Context* to work with your sentence.
Then you can call *process* to perform various NLP tasks on the previously
tokenized sentence.

The *Context* is memory hungry (200 MB) and should only be setup once.
Processing multiple sentences works by alternately calling *tokenize*
and *process* on the same *Context*.

### License

hase is released under MIT license.
You can find a copy of the MIT License in the [LICENSE](./LICENSE) file.
