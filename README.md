# GeoMeTry

A Wolfram Language package for Mathematica doing Riemann geometry.  
Little helper for general relativity calculations.

[Wiki pages on Github](https://github.com/yzerd/geometry/wiki)

---

## Installation

### Simple setup

In Mathematica, using `File` > `Install...` to open the installation dialog.

see official web support: [How do I install packages?](http://support.wolfram.com/kb/5648)

### Manual setup

Put (or link) `GeoMeTry.wl` into one of the places Mathematica prepares for
external applications. You can find the actual paths in your Mathematica
installation in the variables $BaseDirectory and $UserBaseDirectory.

#### Linux:

- system-wide installation (requires root priviledges):

    `/usr/share/Mathematica/Applications/`

- single-user installation:

    `$HOME/.Mathematica/Applications/`

#### Mac OS:

- system-wide installation (requires root priviledges):

    `/Library/Mathematica/Applications/`

- single-user installation:

    `/Users/<user>/Library/Mathematica/Applications/`

#### MSWindows:

- system-wide installation:

    `C:\Documents and settings\All Users\Application
    data\Mathematica\Applications\`

- single-user installation:

    `C:\Documents and settings\<user>\Application
    Data\Mathematica\Applications\`

*In Windows these directories might be hidden.*

### Load package

In mathematica, evaluate
``` Mathematica
<<GeoMeTry`
```

---

## Change Log

### 1015-11-09

+ Fix some bugs. Now `Metric` can handle submanifold.
+ Add `EinsteinTensor`.

### 2015-11-05

+ Now can do curvature!
+ Given metric, and `GmtAll` will take care all the other stuff.
+ `GmtShow` helps to visuallize 2~4 order tensors.

### 2015-11-01

+ Initial commit
