library(readr)
library(dplyr)
library(MazamaCoreUtils)

# Format description: https://fam.nwcg.gov/fam-web/weatherfirecd/13.htm

example <- "
        10         20       30        40        50        60        70        80
12345678901234567890123456789012345678901234567890123456789012345678901234567890
W13500742200507131500R  72 36261  4   72 48100 34 1   20     213 713200 10
W13500742200507131600R  71 36280  4   72 48100 34 1   20     213 596163  9
W13500742200507131700R  71 34249  4   72 48100 34 1   20     213 426285  9
W13500742200507131800R  74 31258  4   74 48100 31 1   20     213 495287  7
W13500742200507131900R  73 29196  2   74 48100 29 1   20     213 300225  4
W13500742200507132000R  71 40228  2   74 48100 29 1   20     213 234277  6
12345678901234567890123456789012345678901234567890123456789012345678901234567890
        10         20       30        40        50        60        70        80
"

# Get column widths and types from the format description
url <- "https://fam.nwcg.gov/fam-web/weatherfirecd/13.htm"

typeInfo <- 
  MazamaCoreUtils::html_getTable(url, 2) %>%
  dplyr::slice(-(1:3)) %>%
  dplyr::pull("X3")

widths <- 
  stringr::str_sub(typeInfo, 1, 1) %>% 
  as.numeric()

col_types <-
  stringr::str_sub(typeInfo, 2, 2) %>% 
  stringr::str_replace("A","c") %>%
  stringr::str_replace("N","n") %>%
  paste0(collapse = "")

col_names = c(letters,"aa")

col_positions <- readr::fwf_widths(
  widths = widths,
  col_names = col_names
)

# Now read in the file

df <- readr::read_fwf(
  file = "~/Downloads/500742.fw13", 
  col_positions = col_positions, 
  col_types = col_types
)

datestamp <- paste0(df$c, df$d)

df$datetime <- MazamaCoreUtils::parseDatetime(datestamp, timezone = "UTC")

retainedColumns <- c("datetime") # TODO:  Add others

# Now dplyr::select(all_of(retainedColumns))
