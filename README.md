# Quarry-Mind-
An open-source Shiny app that searches OneMine with your SME login, uses an LLM to summarize abstracts, and highlights the most relevant cited papers. Smarter mining research—faster.

## Project Description

An app to search and summarize abstracts and recomended sources from the OneMine Database of academic papers.

### Required Features

- Enter a verbose search quarry explaining what your looking for and why:
  - Have the quarry broken into one or many individual keyword based searches by an LLM
  - Search the OneMine database and gather all the titles, abstracts, citation, and links from each search.
  - Feed the each search result into a LLM prompt to filter down to the most useful and relivent results.
  - COmbine the results from the multiple searches and feed into the LLM again for a final summary and recomendation of papers to read.
  - Have the final recomendation be presented in a clean and printable fasion
  
### Challenges

- Getting the LLM to give search terms in a stable and consistent manor
- Scraping the abstract data from the website.
  - some (maybe all) abstract pages have a CSV and RIS file.  Checking a dozen or so of them, the CSV is the citation information and the RIS contains the abstract.  So actual scraping might not bee strictly needed if we can get the search result urls in a good list format, as the CSV and RIS have a consistent link format.
- Develop good prompts for the multiple LLM uses.  Collect a users LLM API key information so keys are not in the code.

### Current App Flow

- Search for something mining related, and get a list of suggested search URLS.  They can be clicked on to see search results.
- The user is presented with a button to scrape the search results.  When clicked the user is taken to the Scraped tab, and shown the articles pulled from the scraping of search links.  After reviewing the user can click a "Summarize Search Results" button.
- The Summarize button will bring the user to the results page.  Which gives a summary and final selection of the most useful articles from the search results for the users original search.  It also lists the articles, and includes links directly to them.
  
## Deliverables

**Start date: 2025-05-07**  
**End date: 2025-05-22**

- Day 1 and 2:
  - [X] Initial wireframing. You may use AI tools such as
    [tldraw](https://www.tldraw.com/), [shiny
    assistant](https://gallery.shinyapps.io/assistant/#) or [ploomber AI
    editor](https://editor.ploomber.io/)
  - Framework to use:
    - [X] shiny only
    - [ ] [golem](https://github.com/ThinkR-open/golem)
    - [ ] [rhino](https://github.com/Appsilon/rhino)
    - [ ] shiny + [ambiorix](https://ambiorix.dev/)
- Day 3:
  - [ ] First meeting for discussing UI and features
- Day 10:
  - [ ] Second meeting for discussing deployment
  
## Meetings

Projects start on Wednesdays with meetings on Fridays. We have a shiny
club channel on the [DSLC
Slack](https://dslcio.slack.com/archives/C08A52V98TY) for chatting. Zoom
link appears 10 minutes before meeting on Fridays in the slack channel.
If you’re not a member of DSLC, [join here](https://dslc.io/join).

## How to Contribute

- Fork this repository
- Create a new branch for your feature
- Make your changes
- Submit a pull request
- Wait for review and address any feedback