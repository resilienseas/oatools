# oatools

Tools for analyzing gaps in ocean acidification data


```R
library(devtools)
devtools::setup()
```

```bash
git config --global user.email clthomas07@gmail.com
git commit --amend --reset-author
```

## Install and load oatools package

One time install:

```R
devtools::install_github("resilienseas/oatools")
```

Load package:

```R
library(oatools)
```

## Developer Tweaks

### Package dependencies

```R
devtools::use_package("rgdal", type="Imports")
devtools::use_package("sp", type="Imports")
devtools::use_package("raster", type="Imports")
devtools::use_package("sdmpredictors", type="Imports")
devtools::use_package("leaflet", type="Suggests")
```

### Documentation

Update documentation after adding or modifying a function in `tools.R`. Note: Placing cursor in functino and using menu "Code" -> "Insert Roxygen Skeleton" is very handy for automatically populating the function documentation, including any new arguments added.

```R
devtools::document()
```

