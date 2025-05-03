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

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Declare the reactive value early
  api_key <- reactiveVal(NULL)
  vals <- reactiveValues(searchterms = tibble())

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
    
    # Call your function to handle the search (replace with actual call to Ellmer)
    result <- chat_Search$chat(prompt)  

    clean_result <- gsub('["\\\\]', '', result)
    
    # Split by comma and trim whitespace
    values <- str_trim(unlist(strsplit(clean_result, ",")))
    
    # Create tibble
    vals$searchterms <- tibble(result = values)

    print("------------")
    print(query)
    print(vals$searchterms)
  })
  

}
