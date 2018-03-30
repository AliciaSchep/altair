htmlwidgets lib files
================

The purpose of this document is to download the javascript files for Vega and Vega-Lite.

``` r
library("httr")
library("rprojroot")
library("tibble")
library("purrr")
library("readr")
```

Let's establish the root directory to which the files will be downloaded.

``` r
lib_dir <- file.path(find_package_root_file(), "inst", "htmlwidgets", "lib")
```

Let's establish the files we will download, and where we will download them to.

``` r
downloads <-
  tribble(
    ~path_local,                    ~path_remote,
    "vega/vega-min.js",             "https://cdn.jsdelivr.net/npm/vega@3.2.1",
    "vega/LICENSE",                 "https://raw.githubusercontent.com/vega/vega/master/LICENSE",
    "vega-lite/vega-lite-min.js",   "https://cdn.jsdelivr.net/npm/vega-lite@2.3.1",
    "vega-lite/LICENSE",            "https://raw.githubusercontent.com/vega/vega-lite/master/LICENSE",
    "vega-embed/vega-embed-min.js", "https://cdn.jsdelivr.net/npm/vega-embed@3.2.0",
    "vega-embed/LICENSE",           "https://raw.githubusercontent.com/vega/vega-embed/master/LICENSE"
  )
```

Let's write a function that will download such a file.

``` r
get_file <- function(path_local, path_remote, lib_dir) {
  
  path_local <- file.path(lib_dir, path_local)
  
  resp <- httr::GET(path_remote)
  
  text <- httr::content(resp, type = "text", encoding = "UTF-8")
  
  readr::write_file(text, path_local)
  
  invisible(NULL)
}
```

``` r
pwalk(downloads, get_file, lib_dir = lib_dir)
```

We also have to write out a yaml manifest. It might be an idea to automate this, as well.
