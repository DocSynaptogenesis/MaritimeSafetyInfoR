#' @title `list_to_text`
#'
#' @author Brendan Knapp, \email{brendan.knapp@@nps.edu}
#' 
#' @importFrom stringr str_c
list_to_text <- function(column, sep = ":"){
  ret <- sapply(column, function(x){
    ret <- str_c(unique(x), collapse = sep)
    if(identical(ret, character(0))){
      # if it's character(0)
      NA
    } else {
      ret}})
  as.character(ret)
}


#' @title Yearly 
#'
#' @author Brendan Knapp, \email{brendan.knapp@@nps.edu}
#' @author Christopher Callaghan, \email{cjcallag@@nps.edu}
#'
#' @importFrom dplyr as_tibble if_else mutate group_by arrange desc select
#' @importFrom magrittr %>%
#' @importFrom pdftools pdf_text
#' @importFrom purrr map_dfr
#' @importFrom tidyr drop_na separate gather
#' @importFrom tidyselect contains
#' @importFrom rvest html_nodes html_text
#' @importFrom stringr str_replace str_replace_all str_split str_subset str_pad str_trim str_extract str_extract_all str_sub str_detect
#' @importFrom sp char2dms
#' @importFrom xml2 read_html
#'
#' @export
scrape_NtM <- function(year, format) {
  if(is.null(year)){
    year <- format(Sys.time(), "%Y")
  }
  if(!is.character(year)){
    stop("Year value provided is not a character.", call. = FALSE)
  }
  if(is.null(format)){
    stop("Please provide a format (e.g. pdf).", call. = FALSE)
  }
  if(!is.character(format)){
    stop("Format value provided is not a character.", call. = FALSE)
  }
  if(format=="pdf"){
    if(year == format(Sys.time(), "%Y")){
      notice_to_mariners_url <- "https://msi.nga.mil/NGAPortal/MSI.portal?_nfpb=true&_st=&_pageLabel=msi_portal_page_61"
      
      most_recent_issue <- notice_to_mariners_url %>%
        read_html() %>%
        html_nodes("tr:nth-child(2) .dec-inv") %>%
        html_text()
      most_recent_week <- str_split(most_recent_issue, "/")[[1]][1]
      #print(most_recent_week)
    }
    if(year != format(Sys.time(), "%Y")){
      most_recent_week <- 52
      #print(most_recent_week)
    }

    url_template <- paste("https://msi.nga.mil/MSISiteContent/StaticFiles/NAV_PUBS/UNTM/", year, "%s/Broadcast_Warn.pdf", sep="")
    
    urls <- sprintf(url_template,
                    str_pad(seq(1:most_recent_week),
                            width = 2,
                            pad = "0"))
    
    months_regex <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                      "JUL", "AUG", "SEP", "OCT", "NOV", "DEC") %>%
      str_c(collapse = "|")
    
    raw_df <- map_dfr(urls, ~
                        .x %>%
                        pdf_text() %>%
                        str_replace_all("([I]+-\\d\\.\\d\\s)|(NM\\s\\d+/\\d+)|(SECTION\\s[I]+)", "") %>%
                        str_replace_all("(\\d+/\\d+\\(.*?\\)\\.)", "~_~~\\1") %>% 
                        str_split("~_") %>%
                        unlist(., recursive = FALSE) %>%
                        str_subset("~~") %>%
                        str_replace_all("~~", "") %>%
                        str_replace_all("\\s+", " ") %>%
                        str_trim() %>%
                        as_tibble())
    
    total_df <- raw_df %>%
      mutate(value = str_replace(value, "(\\)\\.)", "\\)\\.~")) %>%
      separate(value, c("ID", "message"), sep =  "~") %>%
      group_by(ID) %>%
      mutate(coords_1 = str_extract_all(message,
                                        "\\d+-\\d+\\.\\d+[NS]{1}\\s\\d+-\\d+\\.\\d+[EW]"),
             coords_2 = str_extract_all(message,
                                        "\\d+-\\d+[NS]\\s\\d+-\\d+[EW]")) %>%
      mutate(coords_1 = COREmaritime:::list_to_text(coords_1),
             coords_2 = COREmaritime:::list_to_text(coords_2)) %>%
      separate(coords_1, paste0("fine_", c(1:length(.)), sep = ":")) %>%
      separate(coords_2, paste0("coarse_", c(1:length(.)), sep = ":")) %>%
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
             lat = as.numeric(char2dms(lat))) %>%
      mutate(long = str_replace(long, "-", "d"),
             long = str_replace(long, "-", "'"),
             long = str_replace(long, "\\.", "'"),
             long = if_else(str_detect(coord_precision, "fine"),
                            str_replace_all(long, str_sub(long, -1L, -1L),
                                            paste0("\\\" ", str_sub(long, -1L, -1L))),
                            str_replace_all(long, str_sub(long, -1L, -1L),
                                            paste0(" ", str_sub(long, -1L, -1L)))),
             long = as.numeric(char2dms(long))) %>%
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
      select(message_date_time, long, lat, 
             coord_precision, message, zulu_time_date, ID) %>%
      distinct() %>%
      arrange(desc(message_date_time))
    
    #print(urls)
  }
}

