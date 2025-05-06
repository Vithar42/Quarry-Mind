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

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Reactive Values ----
  # Declare the reactive value early
  api_key <- reactiveVal(NULL)
  vals <- reactiveValues(thoughts = NULL,
                         searchterms = tibble(),
                         urls =  NULL,
                         scraped_links = NULL,
                         abs_data = tibble(),
                         json= NULL)

  # Set API id needed Values ----
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

  # Items for the Search Tab ----
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
        ),
        card(
          card_header("Scrape Search Results"),
          card_body(actionButton("scrape_onemine", "Scrape Search Results"))
        )
      )
    })
  })
  
  observeEvent({vals$urls
                input$scrape_onemine}, {
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

  # Items for the Scrape Tab ----
  observeEvent(vals$scraped_links, {
    
    updateTabsetPanel(session, "mytabs", selected = "Scrape")
    
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
      
      # make a little “overview” panel that stays open
      intro_panel <- accordion_panel(
        title = "Overview",
        tags$p("Below are your search‐result sets.  Click each one to expand and see the links."),
        open = TRUE
      )
      
      # build one accordion_panel per element
      result_panels <- lapply(seq_along(vals$scraped_links), function(i) {
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
      tagList(
        do.call(
          accordion,
          c(list(intro_panel), result_panels, list(id = "scrapedAccordion"))
        ),
        card(
          card_header("Summarize Search Results"),
          card_body(actionButton("sum_scrape_onemine", "Summarize Search Results"))
          )
        )
      
      })
    })
    
  observeEvent(vals$scraped_links, {
    
    urls <- vals$scraped_links
    
    df <- tibble(search = character(),
                 title = character(),
                 abstract = character(),
                 citation = character(),
                 url = character())
    
    for (i in 1:length(urls)){
      results <- urls[[i]]$results
      print(names(urls)[i])
      for (k in results){
        
        url_ris <- paste0(k, "/citation/ris")
        searchname <- names(urls)[i]
        temp <- extract_ris_mla(url_ris, searchname, k)
        
        df <- df %>%
          add_row(temp)
        
      }
    }

    print(df)
    vals$abs_data <- df
  })
  
  # Items for the Results Tab ----
  observeEvent({vals$abs_data
                input$sum_scrape_onemine}, {
    
    query <- input$search_query
    abs_data <- vals$abs_data %>% select(-search)
    
    # 1) A stricter system prompt
    new_system_prompt <- paste0(
      "You are an expert researcher in Mining and Civil Engineering. You specialize in ",
      "synthesizing abstracts and summarizing collections of papers. When given titles, ",
      "abstracts, MLA citations, and URLs, you must output **only** one JSON object with ",
      "exactly three keys: \"thoughts\", \"summary\", and \"useful_articles\".\n\n",
      "- \"thoughts\": your private reasoning for why each article was selected (string).\n",
      "- \"summary\": a detailed, multi-paragraph overview (at least 150 words) that ",
      "answers the original query, explains how each chosen article contributes to that ",
      "answer, and justifies their selection.\n",
      "- \"useful_articles\": an **array of 4–6 objects**. Each object **must** have exactly ",
      "four keys:\n",
      "    • \"title\" (string)\n",
      "    • \"author\" (string; empty string if unknown)\n",
      "    • \"year\" (integer; 0 if unknown)\n",
      "    • \"url\" (string)\n\n",
      "Do **not** wrap your output in markdown or backticks, and do not emit any extra keys ",
      "or text—only raw JSON."
    )
    
    # 2) A matching user prompt
    prompt_summary <- paste0(
      "Original OneMine search query: ", query, ".\n\n",
      "You have retrieved the following articles and abstracts:\n", abs_data, "\n\n",
      "Please select **4 to 6** articles that best address the query. In your JSON:\n",
      "  • Use \"thoughts\" to show how you decided on each article.\n",
      "  • In \"summary\", write **at least 150 words** that (1) directly answer the query, ",
      "and (2) for each selected article explain why it’s useful and how it relates to the query.\n\n",
      "Follow the system instructions exactly and output only the final JSON object."
    )
    
  # 3) Fire the chat
  chat_summary <- chat_openai(
    model = "gpt-4o-mini",
    system_prompt = new_system_prompt
    )
  
    # Call your function to handle the search (replace with actual call to Ellmer)
    result <- chat_summary$chat(prompt_summary)  
    
    vals$json <- fromJSON(result)
    
    updateTabsetPanel(session, "mytabs", selected = "Results")
    
    output$abs_results <- renderUI({
      # make sure we actually have the parsed JSON
      req(vals$json)  
      data <- vals$json
      
      # placeholder if nothing
      if (is.null(data$useful_articles) || length(data$useful_articles) == 0) {
        return(div("No search terms found.", class = "text-muted"))
      }
      
      # collapse multi-line strings into one
      thoughts_txt <- paste(data$thoughts, collapse = " ")
      summary_txt  <- paste(data$summary,  collapse = " ")
      
      #browser()
      # build the list of article citations
      
      # 1) Pull out the raw useful_articles
      ua <- tibble(data$useful_articles)
      # 2) If it's not a list, turn it into one (e.g. from a character vector)

      # ua is your data.frame of articles
      ua_list <- lapply(seq_len(nrow(ua)), function(i) {
        # `drop = TRUE` makes it a named vector, then as.list()
        as.list(ua[i, , drop = TRUE])
      })
      
      #browser()
      
      articles_ui <- lapply(ua_list, function(cite) {
        div(class = "mb-3",
            strong(cite$title), tags$br(),
            paste0(cite$author, " (", cite$year, ")"), tags$br(),
            tags$a(href = cite$url, target = "_blank", cite$url)
        )
      })
      
      
      # put it all in cards
      tagList(
        card(
          card_header("Thoughts"),
          card_body(thoughts_txt),
          class = "mb-3"
        ),
        card(
          card_header("Summary"),
          card_body(summary_txt),
          class = "mb-3"
        ),
        card(
          card_header("Useful Articles"),
          card_body(tagList(articles_ui)),
          class = "mb-3"
        )
      )
    })
    

  })
}
