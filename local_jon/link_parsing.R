# Example of how to extract RAWS ids

url <- "https://raws.dri.edu/orlst.html"

orIDs <- 
  MazamaCoreUtils::html_getLinkUrls(url) %>%     # get links 
  stringr::str_subset("rawMAIN") %>%             # only keep those with "rawMAIN"
  stringr::str_match(".+MAIN.pl\\?(.+)") %>%     # pull out everything after "MAIN.pl?"
  magrittr::extract(, 2)                         # keep the second column of the matrix


