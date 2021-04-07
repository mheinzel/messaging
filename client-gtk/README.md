# messaging-client-gtk

A basic cross-platform GTK client.

### Build instructions

```
stack build messaging-client-gtk
stack run messaging-client-gtk-exe -- --help
```

But first you need to set up the GTK dependencies on your system.

#### Windows GTK setup

Install Stack if not done already.
We will need MSYS2, install by following the steps given at https://msys2.org.
Install the packages needed for the GTK client by running the follwing in the msys2 console:
`pacman -S -q --noconfirm mingw64/mingw-w64-x86_64-pkg-config mingw64/mingw-w64-x86_64-gobject-introspection mingw64/mingw-w64-x86_64-gtksourceview3`

Then in the cmd console call the following commands before building.
These commands effect only this instance of the cmd console, so run it again when opening a new console.

```
SET PATH=C:\msys64\mingw64\bin;C:\msys64\usr\bin;%PATH%
SET PKG_CONFIG_PATH=C:\msys64\mingw64\lib\pkgconfig
SET XDG_DATA_DIRS=C:\msys64\mingw64\share
```

Build using a resolver. I used lts-17.4, other versions will result in different ghc versions. Lts-17.4 uses ghc version 8.10.4. 

`stack build --resolver lts-17.4`

This will take a while.

There is a good chance you will run into any of the following errors:
* inflateValidate cannot be resolved when loading libpng16-16.dll
* cairo_gobject_context_get_type cannot be resolved when loading `libcairo-gobject-2.dll`
* Could not resolve symbol "cairo_gobject_context_get_type" in namespace "cairo-1.0"

In this case go to the folder C:\msys64\mingw64\bin and copy "zlib1.dll".
We need to place it in the directory where stack put ghc.
So do "stack path --programs" and go to the given folder and then to ghc-8.10.4\mingw\bin, or another version if using a different resolver, and paste it here.
Now building should hopefully succeed.

After building, if you want to run the GTK version call the following commands.

```
SET PATH=C:\msys64\mingw64\bin;C:\msys64\usr\bin;%PATH%
SET PKG_CONFIG_PATH=C:\msys64\mingw64\lib\pkgconfig
SET XDG_DATA_DIRS=C:\msys64\mingw64\share
stack run messaging-client-gtk-exe -- --help
```

The first three commands need to be run once each time you start a new cmd console, when dealing with GTK.

P.S. Make sure to have a server running before attempting to run it else it will not work.

`stack run messaging-server-exe`

Most info on the installation of GTK on windows is from the following github page, note that some data on the page is outdated like the resolver version: 
https://github.com/haskell-gi/haskell-gi/wiki/Using-haskell-gi-in-Windows
If the above does not work this github page might have a solution or message us directly if you have any problems.
