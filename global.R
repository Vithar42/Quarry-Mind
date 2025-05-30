

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


extract_ris_mla <- function(ris_url, searchname, article_url) {
  # dependencies
  if (!requireNamespace("httr", quietly=TRUE)) install.packages("httr")
  if (!requireNamespace("tibble", quietly=TRUE)) install.packages("tibble")
  library(httr); library(tibble)
  
  # 1. Fetch
  res <- httr::GET(ris_url)
  if (httr::status_code(res) != 200) {
    stop("Failed to retrieve RIS (HTTP ", httr::status_code(res), ").")
  }
  
  # 2. Split into lines
  txt   <- httr::content(res, "text", encoding="UTF-8")
  lines <- strsplit(txt, "\r?\n")[[1]]
  
  # 3. Simple tag grabber
  get_tag <- function(tag) {
    ln <- grep(paste0("^", tag, "  -"), lines, value = TRUE)
    if (length(ln)) sub(paste0("^", tag, "  -\\s*"), "", ln[1]) else NA
  }
  
  # 4. Pull metadata
  title       <- get_tag("TI")
  year        <- get_tag("PY")
  publication <- get_tag("PB")
  au_raw      <- grep("^AU  -", lines, value = TRUE)
  authors     <- sub("^AU  -\\s*", "", au_raw)
  
  # 5. Build MLA author string
  auth_str <- switch(
    as.character(length(authors)),
    "0" = "",
    "1" = authors,
    "2" = {
      a1 <- authors[1]
      p2 <- strsplit(authors[2], ",\\s*")[[1]]
      a2 <- paste(p2[2], p2[1])
      paste0(a1, ", and ", a2)
    },
    paste0(authors[1], ", et al.")
  )
  
  # 6. Robust abstract extraction
  ab_idx <- grep("^AB  -", lines)
  if (length(ab_idx)) {
    idxs <- ab_idx
    for (i in seq(ab_idx + 1, length(lines))) {
      # stop as soon as we hit another RIS tag (like TI, AU, ER, etc)
      if (grepl("^[A-Z0-9]{2}  -", lines[i])) break
      idxs <- c(idxs, i)
    }
    segs <- lines[idxs]
    # strip only the prefix on the first line, and leading whitespace on the rest
    segs[1]   <- sub("^AB  -\\s*", "", segs[1])
    if (length(segs) > 1) segs[-1] <- sub("^\\s+", "", segs[-1])
    abstract <- paste(segs, collapse = " ")
  } else {
    abstract <- NA
  }
  
  # 7. Assemble MLA citation
  citation <- paste0(
    auth_str, if (nzchar(auth_str)) ". " else "",
    title, ". ",
    publication, ", ",
    year, "."
  )
  
  # 8. Return
  tibble::tibble(
    search = searchname,
    title    = title,
    abstract = abstract,
    citation = citation,
    url = article_url,
  )
}

extract_pdf <- function(page_url, username, password) {
  # 0. Install / load dependencies
  needed <- c("httr","rvest","xml2","pdftools","tibble")
  for (pkg in needed) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
  
  
  # assume you’ve exported Firefox cookies for the target domain to cookies.txt
  cookie_file <- "firefox-cookies.txt"
  
  # 1) POST your credentials to the login form
  login <- httr::POST(
    "https://login.personifygo.com/prodsmemi/Account/Login",
    body = list(username = username, password = password),
    encode = "form",
    httr::config(cookiejar = cookie_file)   # capture the resulting session cookie
  )

  # 1. Fetch the HTML page
  res <- httr::GET(page_url,
                   httr::config(cookiefile = cookie_file, cookiejar = cookie_file))
  if (httr::status_code(res) != 200) {
    stop("Failed to retrieve page (HTTP ", httr::status_code(res), ").")
  }
  html_txt <- httr::content(res, "text", encoding = "UTF-8")
  doc      <- xml2::read_html(html_txt)
  
  # 2. Find all <a rel="noopener" href="*.pdf">
  anchors  <- rvest::html_nodes(doc, "a[rel='noopener']")
  hrefs    <- rvest::html_attr(anchors, "href")
  pdf_refs <- hrefs[grepl("\\.pdf$", hrefs, ignore.case = TRUE)]
  if (length(pdf_refs) == 0) {
    stop("No PDF links found on the page.")
  }
  
  # 3. Turn them into absolute URLs
  pdf_urls <- xml2::url_absolute(pdf_refs, base = page_url)
  
  # 4. Download each PDF & extract text
  texts <- vapply(pdf_urls, function(pdf_url) {
    tmp <- tempfile(fileext = ".pdf")
    r2  <- httr::GET(pdf_url,
                     httr::config(cookiefile = cookie_file, cookiejar = cookie_file),
                     httr::write_disk(tmp, overwrite = TRUE))
    if (httr::status_code(r2) != 200) {
      stop("Failed to download PDF: ", pdf_url)
    }
    pages <- pdftools::pdf_text(tmp)
    unlink(tmp)
    paste(pages, collapse = "\n\n")
  }, FUN.VALUE = character(1))
  
  # 5. Return as a tibble
  tibble::tibble(
    pdf_url = pdf_urls,
    text    = texts
  )
}
