

make_search_urls <- function(df){
  
  #browser()

  df <- df %>%
    mutate(term = term %>% 
             as.character() %>%        # just in case it’s a factor
             str_squish() %>%          # collapse & trim extra whitespace
             str_replace_all("\\s+", "+"),
           urls = str_c("https://www.onemine.org/search?Keywords=", term, "&SearchField=All"))
  
  urls <- df$urls
  urls <- tibble(urls)
  
  return(urls)
  
}


name_adjuster <- function(url) {
  # if there's no "&", grab everything after "="
  if (!grepl("&", url, fixed = TRUE)) {
    keyword <- sub(".*?=(.*)$", "\\1", url)
  } else {
    keyword <- sub(".*?=(.*?)&.*", "\\1", url)
  }
  # replace URL-encoded "+" with spaces
  gsub("\\+", " ", keyword)
}