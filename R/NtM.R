#' @title Yearly 
#'
#' @author Brendan Knapp, \email{brendan.knapp@@nps.edu}
#' @author Christopher Callaghan, \email{cjcallag@@nps.edu}
#' 
#' @importFrom magrittr %>%
#' @importFrom rvest html_nodes html_text
#' @importFrom stringr str_split str_pad
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
    #print(urls)
  }
}
