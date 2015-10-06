


# Astview - Documentation 

Astview is a little desktop program to be used by people that want
to investigate syntax trees, e.g. students and lecturers in compiler
construction courses. 
The program evolved as a case study in datatype-generic programming and
building graphical user interfaces in Haskell.

Astview is under continuous development. The sources can be found at [Github](https://github.com/pascalh/Astview).

## 1) User guide

### 1.1)Installing astview

First of all download the sources of astview, which can be easily achieved by  `git clone https://github.com/pascalh/Astview`. 
To install astview, just run `cabal install` in the astview-directory.

### 1.2)Using Astview

You can open astview directly with a file by giving the filename at the command line:

```Bash
astview path/to/mysource.hs
```
or simply start `astview` without arguments and open a file directly via the file menu. 


#### Basic text editor functionality 

The menu file offers the functionality to work with files.
Saving a edited file works as usual: Ctrl-S saves, Save-As has to be done via the menu. 
After changing a file in astview's source editor, a star appears in the title bar next to the
filename to indicate that the file has been changed.

Cut-and-Paste functionality works as usual (Ctrl-C/V/X), allowing to copy-paste source code around. 
The correspondent menu items can be found in menu `Edit`. You can use Ctrl-P to reparse the source code and refresh the tree (the shortcut is not displayed in the menu but works anyway).

#### Source location specific functionality

If the current language supports source locations, one can jump from a selected text position in the source editor to the associated position in the abtract syntax tree by clicking on `Edit/>>>`.

The menu entry `Edit/<<<` highlights the corresponding interval in the source editor for the recently selected subtree.

#### Flattening lists

One note on the representation of Haskells lists in the tree: By default lists in the abstract syntax tree are flattened, which means that they are displayed as a `n`-ary tuple where `n` is the length of the list. If you like to see the exact Haskell term displayed in the tree view with all of its nested applications of the cons operator, you can disable flattening in menu `Edit`.

## 2)Developer guide 

The rest of this Readme is aimed to developers and describes how to add support for your own languages to astview.

### 2.1)Adding custom parsers

The module `Language.Astview.Languages` contains a list of all languages (and thus parsers) which are known to astview. 
You can append new languages right here. 
See now how to define a new language.

First of all we introduce the data type for parse errors. 
Since parsers return different amount of error information, we distinguish between three different types of parsers:
```Haskell
data Error
  = Err -- ^ no specific error information
  | ErrMessage String -- ^ plain error message
  | ErrLocation SrcLocation String -- ^ error message with position information
```

In order to extend astview with your own language you need to know the structure of the data type `Language`, which we use to represent languages and their parsers.

```Haskell
data Language = Language
  { name :: String 
  , syntax :: String 
  , exts :: [String] 
  , parse :: String -> Either Error Ast 
  } 
```
The `name` is just a string for gui-issues, whereas the second attribute `syntax` is the name of the syntax highlighter, which should be associated with that language. If no syntax highlighting is desired `[]` works for you here.
Astview uses the same syntax highlighting as gedit, so you might find the name of your language there. 

The attribute `exts` defines a list of file extentions which should be associated with that language. When opening a file astview automatically selects a language based on the file extention. Thus it is reasonable that the file extentions of all languages known to astview may not overlap. A future release of astview should support manual language selection and thus making the restriction obsolete.

The `parse` function maps the input string either to an error value or to an abstract syntax tree. 
After an input string has been parsed, one has to transform the parsed tree into our internal representation type `Ast` (see documentation of `Language.Astview.Language` for details on `Ast`).
The module `Language.Astview.DataTree` offers different type-generic functions for that purpose.
The very basic one is the function `dataToAstSimpl :: Data t => t -> Ast` transforming an arbitary value whose type implements class `Data` into our internal type `Ast` by just printing the constructors and storing them in a tree.
In order to simplify the tree `dataToAstSimpl` represents `String`s not as a list of `Char`, but as a single node in the tree. 

#### Example: Adding Haskell support to astview

In this section we will introduce you to adding Haskell support to astview. 
We use the abstract syntax and parser from package [haskell-src-exts](http://hackage.haskell.org/packages/archive/haskell-src-exts/latest/doc/html/Language-Haskell-Exts.html). 
The name and the syntax highlighter are both the string `"Haskell"`. 
Although we associate both classical haskell files `".hs"` and literate haskell files `".lhs"` with this language. 
The following code applies the parser to our file content and transforms the parsed value in the right context to fit with our data type `Ast` using `dataToAstSimpl`:

```Haskell
parsehs :: String -> Either Error Ast 
parsehs s =
  case parse s :: ParseResult (Module SrcSpan) of
    ParseOk t                    -> Right $ data2AstSimpl t
    ParseFailed (SrcLoc _ l c) m -> 
      Left $ ErrLocation (position l c) m
```

If the parse fails, the parser returns information about the incorrect source.
We reuse this data to help the user of astview finding the faulty source positions.

Putting it all together we can now define a value of type `Language` in order to support Haskell sources in astview:

```Haskell
haskellexts :: Language
haskellexts = Language "Haskell" "Haskell" [".hs",".lhs"] parsehs 
```
After appending `haskellexts` to the list of known languages `languages` in module `Language.Astview.Languages` and a reinstallation, astview can now display the abstract synax tree of Haskell files.

### 2.2)Adding custom parsers with source location support

In order to get astview to work this source locations, a bit more work has to be done. 
We now assume that the parser builds an abstract syntax tree annotated with source locations. 
The function `dataToAstSimpl` doesn't know which values in the tree are source locations.

Our type for source locations is defined in module `Language.Astview.Language`:
```Haskell
data SrcLocation  =  SrcSpan Int Int Int Int
```

One can use the constructor functions `position` and `linear` to create special cases of source locations.

Instead of the function `dataToAstSimpl` which does not support creation of source locations, we use 

```Haskell
dataToAst :: (Data t) => (forall srcloc.Data srcloc => srcloc -> Maybe SrcLocation)
                      -> (forall st . Typeable st => st -> Bool)
                      -> t -> Ast
```

which gets a source location selector as first argument.
The given function will be automatically applied to all nodes of the tree to extract their source location. 
The target type is wrapped in `Maybe` since not every node of a tree has a associated source location. The second argument is a predicate for subtrees, which should not be displayed. After annotating the subtrees with their respective source location, one sometimes does not want the subtrees representing source locations to occur in the displayed tree. For that purpose one can hand over a predicate to `dataToAst` and all subtrees satisfying the predicate will not be displayed by astview.

In most of the cases one wants values of exactly one type to be removed from the tree. The function 

```Haskell
 dataToAstIgnoreByExample :: (Data t,Typeable t,Typeable b,Data b)
         => (forall a . (Data a,Typeable a)  => a -> Maybe SrcLocation)
         -> b -> t -> Ast
```
works like `dataToAst`, but instead of a predicate one can define a value of an arbitary type `b` and all values of type `b` will be removed from the displayed tree.

#### Example: Adding source location support for Haskell 

We only need to change the function `parsehs` from our example above in order to add source location support to Haskell.
Since we have to care with the type for source locations from `haskell-src-exts` and our internal type, we import the Haskell source locations in a qualified manner:
```Haskell
import qualifed Language.Haskell.Exts.SrcLoc as HsSrcLoc
```
First of all we need to define a function, which returns the associated source location for an arbitrary node in the abstract syntax.
Thank to the structure of the abstract syntax in `hasskell-src-exts` this can be done completely type-generic.
The source location is always of type `SrcSpan` and can be found as the left-most subtree of a tree if existing.
We use a zipper from package `syz` to go the left-most subtree and extract the source location information: 
```Haskell
getSrcLoc :: Data t => t -> Maybe SrcLocation
getSrcLoc t = down' (toZipper t) >>= query (def `extQ` atSpan) where

  def :: a -> Maybe SrcLocation
  def _ = Nothing

  atSpan :: HsSrcLoc.SrcSpan -> Maybe SrcLocation 
  atSpan (HsSrcLoc.SrcSpan _ c1 c2 c3 c4) = Just $ SrcSpan c1 c2 c3 c4 
```

To add the source location support, we now need to give `getSrcLoc` as an argument to the function `dataToAst` as a selector for source locations:
```Haskell
parsehs :: String -> Either Error Ast
parsehs s = case parse s :: ParseResult (Module HsSrcLoc.SrcSpan) of
  ParseOk t                             -> Right $ dataToAst getSrcLoc (const False) t
  ParseFailed (HsSrcLoc.SrcLoc _ l c) m ->  Left $ ErrLocation (position l c) m
```
Using this version of `parsehs` as a parse function causes astview to support juming between associated positions in source text and abstract syntax tree. 

The resulting trees will now contain all the source location information as subtrees, which are already internally stored in the `Ast`. Since source locations are only metainformation to the subtrees and one can jump from subtrees to their respective position in the sources, it is not required to display source locations as nodes in the abstract syntax tree. One can simply remove source locations from the tree by using the function `dataToAstIgnoreByExample`, which causes all values of type `SrcSpan` to be discarded from the tree:

```Haskell
parsehs :: String -> Either Error Ast
parsehs s = case parse s :: ParseResult (Module HsSrcLoc.SrcSpan) of
  ParseOk t  -> Right $ dataToAstIgnoreByExample getSrcLoc
                                                 (undefined::HsSrcLoc.SrcSpan)
                                                 t
  ParseFailed (HsSrcLoc.SrcLoc _ l c) m -> Left $ ErrLocation (position l c) m

```

