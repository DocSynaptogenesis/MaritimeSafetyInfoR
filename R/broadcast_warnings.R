library(tidyverse)
library(rvest)

scrape_url <- "https://msi.nga.mil/NGAPortal/msi/query_results.jsp?MSI_queryType=BroadcastWarning&MSI_generalFilterType=All&MSI_generalFilterValue=-999&MSI_additionalFilterType1=DateRange&MSI_additionalFilterType2=-999&MSI_additionalFilterValue1=20150101%3A20171201&MSI_additionalFilterValue2=-999&MSI_outputOptionType1=SortBy&MSI_outputOptionType2=-999&MSI_outputOptionValue1=Number_DESC&MSI_outputOptionValue2=-999"

data <- "tr~ tr+ tr .nga-resultsCourier , .nga-resultsMainTable .nga-resultsData , pre , .nga-resultsMainTable tr:nth-child(1) .nga-resultsCourier"

test <- scrape_url %>%
  read_html() %>%
  html_nodes(data) %>%
  html_text()

test[1:4]

n <- length(test)
k <- 4 ## your LEN
test_split <- split(test, rep(1:ceiling(n/k), each=k)[1:n])

months <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN",
            "JUL", "AUG", "SEP", "OCT", "NOV", "DEC") %>%
  str_c(collapse = "|")

group <- function(..., na.rm = FALSE){
  df <- data.frame(list(...))
  if (na.rm){
    out <- rep(NA, nrow(df))
    complete <- complete.cases(df)
    indices <- df %>% filter(complete) %>% group_indices_(.dots = names(df))
    out[complete] <- indices
  } else{
    out <- group_indices_(df, .dots = names(df))
  }
  out
}

test_df <- test_split %>%
  as_tibble %>% 
  t %>% 
  as_tibble %>% 
  select(-V3) %>%
  rename(header = V1,
         message = V2,
         footer = V4) %>%
  mutate(new_message = message) %>%
  select(-message) %>%
  rename(message = new_message) %>%
  group_by(message) %>%
  mutate(coords_1 = str_extract_all(message,
                                    "\\d+-\\d+\\.\\d+[NS]{1}\\s\\d+-\\d+\\.\\d+[EW]"),
         coords_2 = str_extract_all(message,
                                    "\\d+-\\d+[NS]\\s\\d+-\\d+[EW]")) %>%
  mutate(coords_1 = list_to_text(coords_1),
         coords_2 = list_to_text(coords_2)) %>%
  # mutate(message_id = row_number()) %>%
  separate(coords_1, paste0("fine_", c(1:30)), sep = ":") %>%
  separate(coords_2, paste0("coarse_", c(1:30)), sep = ":") %>%
  
  gather(coord_precision, coords, contains("_")) %>%
  drop_na(coords) %>%
  separate(coords, c("lat", "long"), sep = " ") %>%
  mutate(header = str_trim(header, side = "both"),
         header = str_replace_all(header, "\\(|\\)", ""),
         header = str_replace_all(header, "(,)|(\\s+)", "_"),
         new_header = str_trim(header)) %>%
  select(-header) %>%
  rename(header = new_header) %>%
  mutate(footer = str_trim(footer, side = "both"),
         footer = str_replace_all(footer, "\\(|\\)", ""),
         new_footer = str_trim(footer)) %>%
  select(-footer) %>%
  rename(footer = new_footer) %>%
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
  select(header, footer, message, coord_precision, lat, long) %>%
  mutate(coord_precision = if_else(str_detect(coord_precision, "fine"),
                                   "fine", "coarse")) %>%
  mutate(message_time = str_extract(footer, "\\d{4}Z"),
         message_time_zone = str_extract(message_time, "[A-Z]"),
         message_time_zone = if_else(message_time_zone == "Z",
                                     "Z/UTC", message_time_zone),
         message_time = str_replace(message_time, "[A-Z]", ""),
         message_mday = str_sub(footer, 1L, 2L),
         message_month = str_extract(footer, months),
         message_year = str_extract(footer, "\\s\\d{4}"),
         message_date = as.Date(paste0(message_year,
                                       message_month,
                                       message_mday),
                                format = "%Y%b%d")) %>%
  mutate(relevant_terms = str_extract_all(message,
                                          "(ROCKET)|(MISSILE)|(HAZARDOUS)|(GUNNERY)|(LAUNCHING)"),
         relevant_terms = list_to_text(relevant_terms)) %>%
  drop_na() %>%
  # ungroup() %>%
  # group_by(message) %>%
  # select(message_date, relevant_terms, message, lat, long,
  # coord_precision, message_time, message_time_zone) %>%
  # mutate(message = str_trim(message),
  # message = str_replace_all(message, "[:punct:]", " "),
  # message = str_replace_all(message, "\\s+", " ")) %>%
  # group_by(message_date, message_time, message, relevant_terms) %>%
  # mutate(group) %>%
  add_tally() %>%
  mutate(shape = if_else(n == 1,"point",
                         if_else(n == 2, "line", "polygon"))) %>%
  ungroup() %>%
  mutate(message_id = group(message_date, message)) %>%
  select(-n) %>%
  arrange(desc(message_date), message) %>%
  select(message_id, message_date, relevant_terms, message, lat, long,
         coord_precision, message_time, message_time_zone)

write_excel_csv(test_df, path = "~/broadcast_warnings.csv")


library(pdftools)

notice_to_mariners_url <- "https://msi.nga.mil/NGAPortal/MSI.portal?_nfpb=true&_st=&_pageLabel=msi_portal_page_61"

most_recent_issue <- notice_to_mariners_url %>%
  read_html() %>%
  html_nodes("tr:nth-child(2) .dec-inv") %>%
  html_text() %>%
  str_extract("\\d{2}") %>%
  as.numeric

# url_template <- "https://msi.nga.mil/MSISiteContent/StaticFiles/NAV_PUBS/UNTM/2017%s/Broadcast_Warn.pdf"

# weeks <- c(1:most_recent_issue)

# urls <- sprintf(url_template, str_pad(seq(1:most_recent_issue), width = 2, pad = "0"))

pdf_url <- "https://msi.nga.mil/MSISiteContent/StaticFiles/NAV_PUBS/UNTM/201719/Broadcast_Warn.pdf"
pdf_url_2 <- "https://msi.nga.mil/MSISiteContent/StaticFiles/NAV_PUBS/UNTM/201701/Broadcast_Warn.pdf"


pdf_test <- pdf_text(pdf_url)
pdf_test_2 <- pdf_text(pdf_url_2)

pdf_test_2 %>% str_detect("(\\(35,36\\))")
# pdf_test %>% 
test <- pdf_test_2 %>%
  str_replace_all("([I]+-\\d\\.\\d\\s)|(NM\\s\\d+/\\d+)|(SECTION\\s[I]+)", "") %>%
  str_replace_all("(\\d+/\\d+\\(.*?\\)\\.)", "~_~~\\1") %>% 
  str_split("~_") %>% unlist(., recursive = FALSE) %>%
  str_subset("~~") %>%
  str_replace_all("~~", "") %>%
  str_replace_all("\\s+", " ") %>%
  str_trim

test %>%
  as_tibble %>%
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
  mutate(relevant_terms = str_extract_all(message,
                                          "(ROCKET)|(MISSILE)|(HAZARDOUS)|(GUNNERY)|(LAUNCHING)"),
         relevant_terms = list_to_text(relevant_terms)) %>%
  mutate(time_date = str_extract(message, "\\(\\d{6}Z.*?\\)"),
         time_date = str_replace_all(time_date, "(\\()|(\\))", ""),
         message_mday = str_sub(time_date, 1L, 2L),
         message_month = str_extract(time_date, months),
         message_year = str_extract(time_date, "\\s\\d{4}"),
         message_date = as.Date(paste0(message_year,
                                       message_month,
                                       message_mday),
                                format = "%Y%b%d"),
         message_time = str_sub(time_date, 3L, 6L),
         message_time = str_replace(message_time, "(\\d{2})", "\\1:"),
         message_date_time = as.POSIXct(paste(message_date,
                                              message_time),
                                        "%Y-%m-%d %H:%M", tz = "GMT")
  ) %>%
  rename(zulu_time_date = time_date) %>%
  select(-message_mday, -message_month, -message_year) %>%
  mutate(coord_precision = if_else(str_detect(coord_precision, "fine"),
                                   "fine", "coarse")) %>%
  select(ID, message_date_time, message, relevant_terms, 
         long, lat, coord_precision, relevant_terms, zulu_time_date) %>%
  arrange(desc(message_date_time)) %>%
  View



