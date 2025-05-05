#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinyalert)
library(ellmer)
library(tidyverse)
library(jsonlite)
library(rvest)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Declare the reactive value early
  api_key <- reactiveVal(NULL)
  vals <- reactiveValues(thoughts = NULL,
                         searchterms = tibble(),
                         urls =  NULL,
                         scraped_links = NULL)

  # React to Check_api button click
  observeEvent(input$check_api, {
    tryCatch({
      key <- Sys.getenv("OPENAI_API_KEY")
      if (key == "") stop("API key not set")
      
      shinyalert(
        title = "API Key Found",
        text = "Your API key is available.",
        type = "success"
      )
    }, error = function(e) {
      shinyalert(
        title = "API Key Missing",
        text = "You don't have an API key set in your environment.",
        type = "error"
      )
    })
  })
  
  # React to set_api button click
  observeEvent(input$set_api, {
    shinyalert(
      title = "Enter your API Key",
      text = "Paste your OpenAI API key below:",
      type = "input",
      inputType = "text",
      showCancelButton = TRUE,
      inputPlaceholder = "sk-...",
      callbackJS = "function(value) {
        if (value) {
          Shiny.setInputValue('api_key_entered', value, {priority: 'event'});
        }
      }"
    )
  })
  
  # Store the entered key
  observeEvent(input$api_key_entered, {
    api_key(input$api_key_entered)
    # Optional: persist it using Sys.setenv()
    Sys.setenv(OPENAI_API_KEY = input$api_key_entered)
    
    shinyalert(
      title = "API Key Saved",
      text = "Your API key has been stored for this session.",
      type = "success"
    )
  })
  
  output$current_key <- renderText({
    paste("Current API Key:", api_key() %||% "[none]")
  })

  # React to search button click
  observeEvent(input$search_btn, {
    query <- input$search_query
    
    # Construct the prompt for Ellmer
    prompt <- paste0("Search OneMine for: ", query)
    
    chat_Search <- chat_openai(
      model = "gpt-4o-mini",
      system_prompt = "You are a specilized search term generator for the OneMine.org database.  The user will give you a description of a problem.  Determine the best way to search OneMine.org's database to find journal articles that will help the user.  If there is a single search term that will work return it, if it would be best to do multiple searches then make a set of serch terms.  Format as jason, with the first tag being 'Thoughts' where you explain the resoning behind the suggested search terms.  The next tag is 'SearchTerms' where you put the search terms.  Make sure to use the correct format for JSON, and make sure to escape any double quotes.  Do not include any other text."
    )
    
    # Call your function to handle the search (replace with actual call to Ellmer)
    result <- chat_Search$chat(prompt)  

    #browser()
    
    data <- fromJSON(result)
    vals$thoughts <- data$Thoughts
    vals$searchterms <- tibble(term = data$SearchTerms)

    
    print("------------")
    print(query)
    print(vals$searchterms)

  })
  
  observeEvent(vals$searchterms, {
    output$search_results <- renderUI({
      # If no terms, show a placeholder
      if (is.null(vals$searchterms) || nrow(vals$searchterms) == 0) {
        return(div("No search terms found.", class = "text-muted"))
      }
      
      # Generate the URLs
      urls <- make_search_urls(vals$searchterms)$urls

      vals$urls <- urls
      
      # Build a <ul> of clickable links
      links_ul <- tags$ul(
        lapply(urls, function(u) {
          tags$li(a(href = u, target = "_blank", u))
        })
      )
      tagList(
        card(
          card_header("Thoughts"),
          card_body(vals$thoughts),
          class = "mb-3"
        ),
        card(
          card_header("Search URLs"),
          card_body(links_ul)
        )
      )
    })
  })
  
  observeEvent(vals$urls, {
    req(vals$urls)
    
    test_url <- vals$urls
    
    
    if (TRUE){
      #vals$scraped_links <- "https://www.onemine.org/documents/open-pit-mine-dewatering-knob-lake"
      load("myname5.RData")
      vals$scraped_links <- results
      return()
    }
    
    results <- list()
    for (u in test_url){
      print(u)
      
      # for each URL, read it and grab the anchors
      all_hrefs <- map(u, function(single_url) {
        page <- read_html(single_url)
        page %>%
          html_nodes("a.item-list__title") %>%
          html_attr("href") %>%
          # turn any relative paths into absolute URLs
          url_absolute(single_url)
      }) %>%
        flatten_chr() %>%
        unique()
      
      temp <- list(search_link = u,
                   results = all_hrefs)
      
      results[[u]] <- temp
    }
    
    save(results, file = "myname5.RData")
    vals$scraped_links <- results
    
  })

  
  observeEvent(vals$scraped_links, {
    output$scraped_results <- renderUI({
      # placeholder if no results yet
      if (is.null(vals$searchterms) || nrow(vals$searchterms) == 0) {
        return(div("No Results Yet.", class = "text-muted"))
      }
      
      # grab the names; fall back to “Set 1”, “Set 2”, … if unnamed
      panel_names <- names(vals$scraped_links)
      if (is.null(panel_names)) {
        panel_names <- paste("Set", seq_along(vals$scraped_links))
      }
      
      # build one accordion_panel per element
      panels <- lapply(seq_along(vals$scraped_links), function(i) {
        this_name <- name_adjuster(panel_names[i])
        this_urls <- vals$scraped_links[[i]]$results
        
        accordion_panel(
          title = this_name,
          # the body: a simple <ul> of links
          tags$ul(
            lapply(this_urls, function(u) {
              tags$li(a(href = u, target = "_blank", u))
            })
          ),
          open = FALSE   # collapsed by default
        )
      })
      
      # stitch them into one accordion
      do.call(accordion, c(panels, list(id = "scrapedAccordion")))
    })
    })
    
}
