library(ellmer)

chat_Search <- chat_openai(
  model = "gpt-4o-mini",
  system_prompt = "You are a specilized search term generator for the OneMine.org database.  The user will give you a description of a proble.  Determine the best way to search OneMine.org's database to find journal articles that will help the user.  If there is a single search term that will work return it, if it would be best to do multiple searches then make a set of serch terms.  Format as a csv and do not include any other content or comments."
)