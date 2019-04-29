# COREmaritime

## Installation

  1. Install `devtools` if you haven't already.

``` r
install.packages("devtools")
```

  2. Install the package using `devtools`.
  
``` r
devtools::install_github("NPSCORELAB/COREmaritime", upgrade="never")
```

## Using Functions Locally

Scrape yearly notices to marines from the National Geospatial-Intelligence Agency [site](https://msi.nga.mil/NGAPortal/MSI.portal?_nfpb=true&_st=&_pageLabel=msi_portal_page_61). Then filter those by AOR names.

``` r
scrape2018 <- COREmaritime::scrape_NtM("2018", "pdf")
pacific_NtM_2018 <- COREmaritime::scrape_cleaner(scrape2018, "pacific")
```

## Launching Apps Locally

``` r
COREmaritime::launch_stnmApp()
```  
