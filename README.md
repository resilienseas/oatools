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

### Tips for Debugging

- `traceback()`: get details on error message
- `system.file(package="oatools")`: find out path for installed R package
- `devtools::load_all(here("../oatools"))`, then `browser()` to debug inside function

### Updating website

To update the website for the R package, update documentation and regenerate the website outputs into the `docs/` folder:

```R
devtools::document()
pkgdown::build_site()
```

#### Errors with `pkgdown::build_site()`

You may get error like this...

```
Reading 'man/find_gaps.Rd'
Error in rep(TRUE, length(x) - 1) : invalid 'times' argument
```

To fix this, be sure that all arguments in your functions are given a definition, ie next to `#' @param some-argument-name`. Then in RStudio, place your cursor inside the offending function (eg `find_gaps()` based on error message example), Code > Insert Roxygen Skeleton. This assures all arguments are listed in the documentation of the function. Then rerun:

```R
devtools::document()
pkgdown::build_site()
```
