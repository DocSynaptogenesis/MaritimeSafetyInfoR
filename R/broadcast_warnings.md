PDF Inferno
================
Brendan Knapp
November 30, 2017

-   [Notice to Mariners](#notice-to-mariners)
-   [Dependencies](#dependencies)
-   [Most Recently Published Issue](#most-recently-published-issue)
-   [URLs](#urls)
-   [Setup for Data Extraction](#setup-for-data-extraction)
    -   [Useful Regular Expressions](#useful-regular-expressions)
    -   [`list_to_text()` Modification](#list_to_text-modification)
    -   [Empty `tibble` data frame](#empty-tibble-data-frame)
-   [Data Wrangling](#data-wrangling)
    -   [Extraction and Initial Cleaning](#extraction-and-initial-cleaning)
    -   [The Heavy Lifting](#the-heavy-lifting)
-   [Results](#results)
-   [The Resulting Table](#the-resulting-table)
-   [Save](#save)
-   [`sessionInfo()`](#sessioninfo)

<!-- <base target="_blank"/> -->
The data obtained in this code is available on GitHub [here](https://github.com/DocSynaptogenesis/MaritimeSafetyInfoR).

Notice to Mariners
==================

-   The U.S. Notice to Mariners data is available in multiple formats, but they all leave something to be desired in terms of data quality and completeness.

-   Unfortunately, there are many Broadcast Warnings included in the PDF publications that are missing in other available formats, making the PDF versions the most complete source of data.

> **To overcome this, we must step into the seventh circle of data mining hell: poorly structured PDF files.**

Dependencies
============

``` r
library(dplyr)
library(tidyr)
library(stringr)
library(rvest)
library(pdftools)
```

Most Recently Published Issue
=============================

We can tap into the NGA site's HTML and scrape the latest publication's issue number like so:

``` r
notice_to_mariners_url <- "https://msi.nga.mil/NGAPortal/MSI.portal?_nfpb=true&_st=&_pageLabel=msi_portal_page_61"

most_recent_issue <- notice_to_mariners_url %>%
  read_html() %>%
  html_nodes("tr:nth-child(2) .dec-inv") %>%
  html_text()

paste(cat("The most recent issue is:\n"), most_recent_issue)
```

    ## The most recent issue is:

    ## [1] " 48/2017"

``` r
most_recent_issue %<>%
  str_extract("\\d{2}") %>%
  as.numeric

paste(cat("The week number we will use for our URL range is:\n"), most_recent_issue)
```

    ## The week number we will use for our URL range is:

    ## [1] " 48"

For demonstration, you can see the \* table of contents of the most recent Notice to Mariners (48/2017) + [here](https://msi.nga.mil/NGAPortal/MSI.portal?_nfpb=true&_st=&_pageLabel=msi_ntm_pubview_page&CCD_itemID=201748) \* the Broadcast Warnings/MARAD Advisories/Special Warnings PDF + [here](https://msi.nga.mil/MSISiteContent/StaticFiles/NAV_PUBS/UNTM/201748/Broadcast_Warn.pdf.)

URLs
====

The URLs for 2017's PDFs follow a template containing the four digit year followed by the two digit week.

Here we replace the two digit week with `%s` so that we can format a the template for each week of the year.

``` r
url_template <-
  "https://msi.nga.mil/MSISiteContent/StaticFiles/NAV_PUBS/UNTM/201701/Broadcast_Warn.pdf" %>%
  str_replace("(\\d{4})\\d{2}", "\\1%s")

paste(cat("Our formatted URL template is:\n"), url_template)
```

    ## Our formatted URL template is:

    ## [1] " https://msi.nga.mil/MSISiteContent/StaticFiles/NAV_PUBS/UNTM/2017%s/Broadcast_Warn.pdf"

Using `sprintf()` with `url_template`, we can build a `vector` of `urls` with each two digit week up until `most_recent_issue`.

``` r
urls <- sprintf(url_template,
                str_pad(seq(1:most_recent_issue),
                        width = 2,
                        pad = "0"))

paste(cat("Our URLs follow this pattern:\n"), urls[1:5])
```

    ## Our URLs follow this pattern:

    ## [1] " https://msi.nga.mil/MSISiteContent/StaticFiles/NAV_PUBS/UNTM/201701/Broadcast_Warn.pdf"
    ## [2] " https://msi.nga.mil/MSISiteContent/StaticFiles/NAV_PUBS/UNTM/201702/Broadcast_Warn.pdf"
    ## [3] " https://msi.nga.mil/MSISiteContent/StaticFiles/NAV_PUBS/UNTM/201703/Broadcast_Warn.pdf"
    ## [4] " https://msi.nga.mil/MSISiteContent/StaticFiles/NAV_PUBS/UNTM/201704/Broadcast_Warn.pdf"
    ## [5] " https://msi.nga.mil/MSISiteContent/StaticFiles/NAV_PUBS/UNTM/201705/Broadcast_Warn.pdf"

We will eventually iterate through `urls` to extract and parse the desired information.

Setup for Data Extraction
=========================

Useful Regular Expressions
--------------------------

Let's prepare a handy regex variable of months in the format that we see is used for dates in the PDF documents.

``` r
months_regex <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                  "JUL", "AUG", "SEP", "OCT", "NOV", "DEC") %>%
  str_c(collapse = "|")

paste(cat("The resulting regex looks like so:\n"), months_regex)
```

    ## The resulting regex looks like so:

    ## [1] " JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC"

We'll also prepare a regex variable containing terms of interest to extract from the messages to facilitate filtering for research.

``` r
relevant_terms_regex <- c("ROCKET", "MISSILE", "HAZARDOUS",
                          "GUNNERY", "LAUNCHING") %>%
  str_c(collapse = "|")

paste(cat("The resulting regex looks like so:\n"), relevant_terms_regex)
```

    ## The resulting regex looks like so:

    ## [1] " ROCKET|MISSILE|HAZARDOUS|GUNNERY|LAUNCHING"

`list_to_text()` Modification
-----------------------------

This is an incredibly handy `list_to_text()` function that is slightly modified from the that in the [`exploratory` package](https://github.com/exploratory-io/exploratory_func).

The difference is that the `character` returned contains only `unique()` elements from the original `list`, which I have found to be useful when you have a set of key terms with which you'd like to annotate a text dataset.

``` r
list_to_text <- function(column, sep = ":"){
  loadNamespace("stringr")
  ret <- sapply(column, function(x){
    ret <- stringr::str_c(unique(x), collapse = sep)
    if(identical(ret, character(0))){
      # if it's character(0)
      NA
    } else {
      ret}})
  as.character(ret)
}
```

Empty `tibble` data frame
-------------------------

We'll create an empty `tibble` which we will use to store our information.

``` r
raw_df <- tibble()
```

Data Wrangling
==============

Extraction and Initial Cleaning
-------------------------------

-   pull the PDF text: `pdf_text()`
-   remove document's header and footer clutter: `str_replace_all()`
-   annotate the positions to
    -   split desired text from body clutter into a nested `list` : `str_replace_all()`
    -   split desired entries into separate fields : `str_replace_all()`
-   unlist top layer: `unlist()`
-   keep only those elements in the `list` with our annotations: `str_subset()`
-   remove inner excess white space: `str_replace_all()`
-   remove outer excess white space: `str_trim()`
-   convert the `list` to a data frame: `as_tibble()`
-   bind data to `raw_df`: `bind_rows()`
-   rinse, recycle, repeat

``` r
for(URL in urls){
  temp_df <- URL %>%
    pdf_text() %>%
    str_replace_all("([I]+-\\d\\.\\d\\s)|(NM\\s\\d+/\\d+)|(SECTION\\s[I]+)", "") %>%
    str_replace_all("(\\d+/\\d+\\(.*?\\)\\.)", "~_~~\\1") %>% 
    str_split("~_") %>%
    unlist(., recursive = FALSE) %>%
    str_subset("~~") %>%
    str_replace_all("~~", "") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim %>%
    as_tibble
    
  raw_df <- bind_rows(raw_df, temp_df)
}
```

The Heavy Lifting
-----------------

> There are more steps than required here as the format for which that will be used is not yet determined. Modification from this point into whatever structure is required for future analysis will be simple.

``` r
total_df <- raw_df %>%
  mutate(value = str_replace(value, "(\\)\\.)", "\\)\\.~")) %>%
    separate(value, c("ID", "message"), sep =  "~") %>%
    mutate(coords_1 = str_extract_all(message,
                                      "\\d+-\\d+\\.\\d+[NS]{1}\\s\\d+-\\d+\\.\\d+[EW]"),
           coords_2 = str_extract_all(message,
                                      "\\d+-\\d+[NS]\\s\\d+-\\d+[EW]")) %>%
    mutate(coords_1 = list_to_text(coords_1),
           coords_2 = list_to_text(coords_2)) %>%
    separate(coords_1, paste0("fine_", c(1:40)), sep = ":") %>%
    separate(coords_2, paste0("coarse_", c(1:40)), sep = ":") %>%
    gather(coord_precision, coords, contains("_")) %>%
    drop_na(coords) %>%
    separate(coords, c("lat", "long"), sep = " ") %>%
    mutate(lat = str_replace(lat, "-", "d"),
           lat = str_replace(lat, "-", "'"),
           lat = str_replace(lat, "\\.", "'"),
           lat = if_else(str_detect(coord_precision, "fine"),
                         str_replace_all(lat, str_sub(lat, -1L, -1L),
                                         paste0("\\\" ", str_sub(lat, -1L, -1L))),
                         str_replace_all(lat, str_sub(lat, -1L, -1L),
                                         paste0(" ", str_sub(lat, -1L, -1L)))),
           lat = as.numeric(sp::char2dms(lat))) %>%
    mutate(long = str_replace(long, "-", "d"),
           long = str_replace(long, "-", "'"),
           long = str_replace(long, "\\.", "'"),
           long = if_else(str_detect(coord_precision, "fine"),
                          str_replace_all(long, str_sub(long, -1L, -1L),
                                          paste0("\\\" ", str_sub(long, -1L, -1L))),
                          str_replace_all(long, str_sub(long, -1L, -1L),
                                          paste0(" ", str_sub(long, -1L, -1L)))),
           long = as.numeric(sp::char2dms(long))) %>%
    mutate(relevant_terms = str_extract_all(message, relevant_terms_regex),
           relevant_terms = list_to_text(relevant_terms)) %>%
    mutate(time_date = str_extract(message, "\\(\\d{6}Z.*?\\)"),
           time_date = str_replace_all(time_date, "(\\()|(\\))", ""),
           message_mday = str_sub(time_date, 1L, 2L),
           message_month = str_extract(time_date, months_regex),
           message_year = str_extract(time_date, "\\s\\d{4}"),
           message_date = as.Date(paste0(message_year,
                                         message_month,
                                         message_mday),
                                  format = "%Y%b%d"),
           message_time = str_sub(time_date, 3L, 6L),
           message_time = str_replace(message_time, "(\\d{2})", "\\1:"),
           message_date_time = as.POSIXct(paste(message_date,
                                                message_time),
                                          "%Y-%m-%d %H:%M", tz = "GMT")) %>%
    rename(zulu_time_date = time_date) %>%
    select(-message_mday, -message_month, -message_year) %>%
    mutate(coord_precision = if_else(str_detect(coord_precision, "fine"),
                                     "fine", "coarse")) %>%
    select(message_date_time, relevant_terms, long, lat, 
           coord_precision, message, zulu_time_date, ID) %>%
    distinct() %>%
    arrange(desc(message_date_time))
```

Results
=======

``` r
head(total_df)
```

    ## # A tibble: 6 x 8
    ##     message_date_time relevant_terms      long       lat coord_precision
    ##                <dttm>          <chr>     <dbl>     <dbl>           <chr>
    ## 1 2017-11-21 09:38:00      HAZARDOUS 146.05972  16.01833            fine
    ## 2 2017-11-21 08:47:00           <NA> -68.53528  63.71889            fine
    ## 3 2017-11-21 08:37:00           <NA> -68.53528  63.71889            fine
    ## 4 2017-11-21 08:15:00           <NA> 130.00000 -10.00000          coarse
    ## 5 2017-11-21 08:09:00           <NA>  12.00000  33.00000          coarse
    ## 6 2017-11-21 08:03:00      HAZARDOUS 144.00000  13.00000          coarse
    ## # ... with 3 more variables: message <chr>, zulu_time_date <chr>, ID <chr>

``` r
glimpse(total_df)
```

    ## Observations: 12,813
    ## Variables: 8
    ## $ message_date_time <dttm> 2017-11-21 09:38:00, 2017-11-21 08:47:00, 2...
    ## $ relevant_terms    <chr> "HAZARDOUS", NA, NA, NA, NA, "HAZARDOUS", "H...
    ## $ long              <dbl> 146.05972, -68.53528, -68.53528, 130.00000, ...
    ## $ lat               <dbl> 16.018333, 63.718889, 63.718889, -10.000000,...
    ## $ coord_precision   <chr> "fine", "fine", "fine", "coarse", "coarse", ...
    ## $ message           <chr> " WESTERN NORTH PACIFIC. MARIANA ISLANDS. DN...
    ## $ zulu_time_date    <chr> "210938Z NOV 2017", "210847Z NOV 2017", "210...
    ## $ ID                <chr> "4055/17(81).", "393/17(15).", "1093/17(15)....

The Resulting Table
===================

``` r
pander::panderOptions('table.split.table', Inf)
pander::pander(head(total_df, n = 5))
```

<table style="width:100%;">
<colgroup>
<col width="15%" />
<col width="12%" />
<col width="6%" />
<col width="5%" />
<col width="12%" />
<col width="23%" />
<col width="13%" />
<col width="10%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">message_date_time</th>
<th align="center">relevant_terms</th>
<th align="center">long</th>
<th align="center">lat</th>
<th align="center">coord_precision</th>
<th align="center">message</th>
<th align="center">zulu_time_date</th>
<th align="center">ID</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">2017-11-21 09:38:00</td>
<td align="center">HAZARDOUS</td>
<td align="center">146.1</td>
<td align="center">16.02</td>
<td align="center">fine</td>
<td align="center">WESTERN NORTH PACIFIC. MARIANA ISLANDS. DNC 12. 1. HAZARDOUS OPERATIONS 212230Z TO 220230Z NOV IN AREA WITHIN 12 MILES OF 16-01.06N 146-03.35E. 2. CANCEL THIS MSG 220330Z NOV 17. (210938Z NOV 2017) III-1.20</td>
<td align="center">210938Z NOV 2017</td>
<td align="center">4055/17(81).</td>
</tr>
<tr class="even">
<td align="center">2017-11-21 08:47:00</td>
<td align="center">NA</td>
<td align="center">-68.54</td>
<td align="center">63.72</td>
<td align="center">fine</td>
<td align="center">DAVIS STRAIT. CANADA-NORTHEAST COAST. DNC 28. MCTS IQALUIT CENTRE 63-43.8N 068-32.7W NBDP AND DSC SERVICES INOPERATIVE. (210847Z NOV 2017) III-1.22</td>
<td align="center">210847Z NOV 2017</td>
<td align="center">393/17(15).</td>
</tr>
<tr class="odd">
<td align="center">2017-11-21 08:37:00</td>
<td align="center">NA</td>
<td align="center">-68.54</td>
<td align="center">63.72</td>
<td align="center">fine</td>
<td align="center">DAVIS STRAIT. CANADA-NORTHEAST COAST. MCTS IQALUIT CENTRE 63-43.8N 068-32.7W NBDP AND DSC SERVICES INOPERATIVE. (210837Z NOV 2017)</td>
<td align="center">210837Z NOV 2017</td>
<td align="center">1093/17(15).</td>
</tr>
<tr class="even">
<td align="center">2017-11-21 08:15:00</td>
<td align="center">NA</td>
<td align="center">130</td>
<td align="center">-10</td>
<td align="center">coarse</td>
<td align="center">TIMOR SEA. AUSTRALIA-NORTH COAST. DNC 04. 1. FISHING NET ADRIFT IN 10-40S 130-40E AT 210444Z NOV. 2. CANCEL HYDROPAC 4039/17. 3. CANCEL THIS MSG 240815Z NOV 17. (210815Z NOV 2017)</td>
<td align="center">210815Z NOV 2017</td>
<td align="center">4054/17(74).</td>
</tr>
<tr class="odd">
<td align="center">2017-11-21 08:09:00</td>
<td align="center">NA</td>
<td align="center">12</td>
<td align="center">33</td>
<td align="center">coarse</td>
<td align="center">EASTERN MEDITERRANEAN SEA. DNC 08, DNC 09. VESSEL, NUMEROUS PERSONS ON BOARD, IN NEED OF ASSISTANCE IN 33-42N 012-40E AT 210400Z NOV. VESSELS IN VICINITY REQUESTED TO KEEP A SHARP LOOKOUT, ASSIST IF POSSIBLE. REPORTS TO MRCC ROME, INMARSAT-C: 424744220, PHONE: 3906 5908 4527, 3906 5908 4409, FAX: 390 6592 2737, 3906 5908 4793, E-MAIL: <a href="mailto:ITMRCC@MIT.GOV.IT">ITMRCC@MIT.GOV.IT</a>. (210809Z NOV 2017)</td>
<td align="center">210809Z NOV 2017</td>
<td align="center">3747/17(56).</td>
</tr>
</tbody>
</table>

Save
====

As the format to be used is not yet determined, I save a few different versions that are easy to distribute, including an `.rda` file that is simple to load for future use and include in a package.

``` r
readr::write_excel_csv(total_df, "data_raw/total_df_excel.csv")
readr::write_csv(total_df, "data_raw/total_df.csv")
save(total_df, file = "data/total_df.rda")
```

`sessionInfo()`
===============

``` r
sessionInfo()
```

    ## R version 3.4.2 (2017-09-28)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 15063)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United States.1252 
    ## [2] LC_CTYPE=English_United States.1252   
    ## [3] LC_MONETARY=English_United States.1252
    ## [4] LC_NUMERIC=C                          
    ## [5] LC_TIME=English_United States.1252    
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] bindrcpp_0.2     pdftools_1.4     rvest_0.3.2.9000 xml2_1.1.1      
    ## [5] stringr_1.2.0    tidyr_0.7.2      dplyr_0.7.4     
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_0.12.13     knitr_1.17       bindr_0.1        magrittr_1.5    
    ##  [5] hms_0.3          tidyselect_0.2.2 lattice_0.20-35  R6_2.2.2        
    ##  [9] rlang_0.1.4      httr_1.3.1       tools_3.4.2      grid_3.4.2      
    ## [13] selectr_0.3-1    htmltools_0.3.6  yaml_2.1.14      assertthat_0.2.0
    ## [17] rprojroot_1.2    digest_0.6.12    tibble_1.3.4     readr_1.1.1     
    ## [21] purrr_0.2.4      curl_3.0         glue_1.1.1       evaluate_0.10.1 
    ## [25] rmarkdown_1.6    sp_1.2-5         stringi_1.1.5    pander_0.6.1    
    ## [29] compiler_3.4.2   backports_1.1.1  XML_3.98-1.9     pkgconfig_2.0.1
