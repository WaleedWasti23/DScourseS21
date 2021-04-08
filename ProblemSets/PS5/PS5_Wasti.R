library(tidyverse)
library(rscorecard)
library(rvest)
library(httr)
library(listviewer)
library(lubridate)

# Q3

eplrec = read_html("https://en.wikipedia.org/wiki/Premier_League_records_and_statistics")

eplrec

mgoals <- 
  eplrec %>%
  html_nodes("#mw-content-text > div.mw-parser-output > table:nth-child(34)") %>% ## select table element
  `[[`(1) %>%
  html_table()

mgoals


# Q4


endpoint = "series/observations"
params = list(
  api_key= "#put in your API key#", ## Change to your own key
  file_type="json", 
  series_id="UNRATE"
)


# library(httr) ## Already loaded above

fred = 
  httr::GET(
    url = "https://api.stlouisfed.org/", ## Base URL
    path = paste0("fred/", endpoint),    ## The API endpoint
    query = params                       ## Our parameter list
  )



fred = 
  fred %>% 
  httr::content("text") %>% ## Extract the reponse content (i.e. text)
  jsonlite::fromJSON()      ## Convert from JSON to R object

## What type of object did we get?
typeof(fred)

# View(fred) ## What I'd use in an interactive R session

## library(listviewer)        ## Already loaded
jsonedit(fred, mode = "view") ## Better for RMarkdown documents

fred =
  fred %>% 
  purrr::pluck("observations") %>% ## Extract the "$observations" list element
  # .$observations %>% ## I could also have used this
  # magrittr::extract("observations") %>% ## Or this
  as_tibble() ## Just for nice formatting
fred


# library(lubridate) ## Already loaded above

fred =
  fred %>%
  mutate(across(realtime_start:date, ymd)) %>%
  mutate(value = as.numeric(value))

fred %>%
  ggplot(aes(date, value)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x="Date", y="Percent",
    title="Unemployment Rate", caption="Source: FRED"
  )

