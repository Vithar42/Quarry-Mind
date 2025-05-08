#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)

ui <- page_fluid(
  accordion(
    accordion_panel("LLM Credentials",
                 actionButton("check_api",
                              "Check if API key is set"),
                 actionButton("set_api",
                              "Set an openAI API key")
                 ), 
    open = FALSE),
  
  # Main card container
  card(
    # Navigation tabs
    navset_tab(
      id = "mytabs",
      nav_panel("Search",
                #textInput("search_query", "Search OneMine", placeholder = "Enter search terms..."),
                textAreaInput("search_query", 
                              "Search OneMine", 
                              placeholder = "Enter search terms...",
                              width = "100%",
                              height = "200px"),
                actionButton("search_btn", "Search", class = "btn-primary"),
                card(
                  title = "Search Results",
                  #verbatimTextOutput("search_results")
                  uiOutput("search_results"),
                )
      ),
      nav_panel("Scrape",
                uiOutput("scraped_results")
      ),
      nav_panel("Results",
               uiOutput("abs_results")
               ),
      nav_panel("Analysis",
                uiOutput("analysis_results")
      )
    )
  )
)