


# Astview - Documentation 

Astview is a little desktop program to be used by people that want
to investigate syntax trees, e.g. students and lecturers in compiler
construction courses. Given a parse function `p :: String -> a`, where `a` is a member of Haskell's `Data`
type class, astview can show syntax trees in a standard tree
widget.

The program evolved as a case study in generic programming and
building graphical user interfaces in Haskell.


## User guide

### Opening astview
Astview will be installed in our local cabal directory. Thus you will
find the executable `astview` in `~/.cabal/bin`.


### Working with source files

We tried to make the user interface as common as possible by
following the [
GNOME human interface guidelines](http://library.gnome.org/devel/hig-book/stable/) closely. You can open a file by
giving the filename at the command line:

```Bash
astview path/to/mysource.hs
```
or simply open it via the file menu. 

The file's extension will
determine the parser automatically. If there are multiple parsers for
one extension, the first one will be taken. Launching astview without
any files will not enable any parser. Saving works as
expected: Ctrl-S saves, Save-As has to be done via the menu. After changing
a file in astview's text editor, the usual star appears in the title bar, next to the
filename.

Cut-and-Paste functionality works as usual (Ctrl-C/P/X), allowing
to copy-paste source code to or from other programs.

Astview uses the same syntax-higlighting sourceview widget as
GNOME's standard editor gedit, so any language recognized there will
be highlighted by astview. 

If the current language supports source locations, one can jump from a position
in the source editor to the associated position in the tree by clicking on `Edit/-->(Jump to position in tree)`.

As noted above, the parser is chosen automatically when opening a
file. When editing source code, one can change the parser using the
parser menu issuing an immediate reparse. Ctrl-P reparses the source
at any time.

## Adding custom parsers

The module `Language.Astview.Languages` contains
a list of all languages (and thus parsers) which are known. You can 
append new languages right here. In this section we show how to add custom parsers
to astview.

### The data type for languages
First of all we introduce the following data type for parse errors. Since parsers return different amount of error information, we distinguish
between three different types of parsers:
```Haskell
data Error
  = Err -- ^ no error information
  | ErrMessage String -- ^ simple error message
  | ErrLocation SrcLocation String -- ^ error message and src loc
```

In order to extend astview with your own language you need to know the structure
of the data type `Language`, which we use to represent languages
and their parsers.

```Haskell
data Language = forall a . Language
  { name :: String -- ^ language name
  , syntax :: String -- ^ syntax highlighter name
  , exts :: [String] 
   -- ^ file extentions which should be associated with this language
  , parse :: String -> Either Error a -- ^ parse function
  , toTree :: a -> Tree String -- ^ how to get a Tree String?
  , srcLoc :: Tree String -> Maybe SrcLocation
    -- ^ selector function for source locations (if supported)
  } 
```
The name is just a string for gui-issues, whereas the second attribute is the name
of the syntax highlighter, which should be associated with that language. As
described above, we use the same syntax highlighting as gedit. 

The parse function maps the input string either to an error value or to an 
abstract syntax tree. Up to now we do not work on the abstract syntax tree, but
on a tree with string-labeled nodes. In module `Language.Astview.DataTree` you
find the type-generic function `data2tree` transforming an arbitary value
whose type implements class `Data` to a tree with string nodes.
After a file's parse we directly apply the given `toTree`
function throwing away all type informations. Thus we internally represent abstract syntax trees as a tree of strings.

The last component of the data type is about source locations. Our implementation of source locations can be found in module `Language.Astview.SourceLocation`. Since we want to
associate positions in the source text with tree positions, we need to reason about
source locations in the syntax tree. Languages respectively parsers not supporting source 
location use the constant `Nothing` function here. For the others we map the given
function over the tree of strings in order to get all source locations being represented in the tree. Note that you do not have to reason about the whole tree. The function `srcLoc` will be automatically applied to all nodes of the tree.

### Example: Adding your own language

In this section we will introduce you to extending astview with our own languages. As
a running example we will use the language Haskell based on the abstract syntax and parser from package [haskell-src-exts](http://hackage.haskell.org/packages/archive/haskell-src-exts/latest/doc/html/Language-Haskell-Exts.html). The name and the syntax highlighter are both the string `"Haskell"`. Although we associate both classical haskell files `".hs"` and literate haskell files `".lhs"` with this language. The following code applies the parser to our file content and puts the result in the right context to fit with our resulting data type:


```Haskell
parHaskell :: String -> Either Error (Module SrcSpan)
parHaskell s =
  case parse s of
    ParseOk t                    -> Right t
    ParseFailed (SrcLoc _ l c) m -> 
      Left $ ErrLocation (SrcPosition l c) m
```

If the parse fails, the parser returns information about the incorrect source. We reuse this data to help the user of astview finding the faulty sources. Have a look at module 
`Language.Astview.SourceLocation` to get an impression of our source location types. 

The last missing component of our language definition is the extraction of source locations. Currently this is done in an unsafe way, since the algorithm assumes that every constructor named `"SrcSpan"` is the beginning of a source location and we simply extract the source span information. 
```Haskell
toSrcLoc :: Tree String -> Maybe SrcLocation
toSrcLoc (Node "SrcSpan" [_,c1,c2,c3,c4]) = 
  Just $ SrcSpan (to c1) (to c2) (to c3) (to c4)
   where 
     to :: Tree String -> Int
     to = read . rootLabel 
toSrcLoc _        = Nothing 
```
Applying the constructor `Language` to all those functions, we get the value representing the language Haskell in astview:
```Haskell
haskellexts :: Language
haskellexts = Language 
  "Haskell" 
  "Haskell" 
  [".hs",".lhs"] 
  parHaskell
  (data2tree::Module SrcSpan ->Tree String)
  toSrcLoc
```
In the end one has to append this value to the list `knownLanguages` in module `Language.Astview.Languages` to introduce your new language to astview.


