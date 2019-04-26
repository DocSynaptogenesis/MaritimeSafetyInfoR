# COREmaritime

## Installation

  1. Install `devtools` if you haven't already.

``` r
install.packages("devtools")
```

  2. Install the package using `devtools`.
  
``` r
devtools::install_github("NPSCORELAB/COREmisc", upgrade="never")
```

## Using Functions Locally

``` r
scrape2018 <- COREmaritime::scrape_NtM("2018", "pdf")
pacific_NtM_2018 <- COREmaritime::scrape_cleaner(scrape2018, "pacific")
```

## Launching Apps Locally

``` r
COREmaritime::launch_stnmApp()
```  
