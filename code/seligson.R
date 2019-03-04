library(dplyr)
library(readr)
library(rvest)

html <- read_html('https://www.seligson.fi/sco/suomi/rahastot/rahastojen-arvojen-historia/')

hrefs <- html_nodes(html, 'a') %>% html_attr('href')

csvs <- hrefs[grepl('\\.csv$', hrefs)]

download <- function(url) {
  prc <- read_delim(url,
                    locale = locale(date_format = '%d.%m.%Y', decimal_mark = ','),
                    col_names = c('date', 'prc'),
                    col_types = cols(date = col_date(), prc = col_double()),
                    delim = ';') %>%
    mutate(fund = tools::file_path_sans_ext(basename(url)))
  prc
}

prices <- bind_rows(lapply(csvs, download))

dir.create('static/data', showWarnings = FALSE, recursive = TRUE)

write_rds(prices, 'static/data/seligson.rds')
