


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
following the [http://library.gnome.org/devel/hig-book/stable/](
GNOME human interface guidelines) closely. You can open a file by
giving the filename at the command line:

```Bash
astview path/to/mysource.hs
```
or simply open it via the file menu. The file's extension will
determine the parser automatically. If there are multiple parsers for
one extension, the first one will be taken. Launching astview without
any files will not enable any parser. Saving works as
expected: Ctrl-S saves, Save-As has to be done via the menu. After changing
a file in astview's text editor, the usual star appears in the title bar, next to the
filename.


