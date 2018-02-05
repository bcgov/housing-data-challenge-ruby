navbarMenu(
  "About",
  tabPanel("Project background", includeMarkdown("about/project_background.Rmd")),
  tabPanel("Data sources", includeMarkdown("about/data_sources.Rmd")),
  tabPanel("Code repository", includeMarkdown("about/code_repository.Rmd")),
  tabPanel("Glossary", includeMarkdown("about/glossary.Rmd")),
  tabPanel("External resources", includeMarkdown("about/external_resources.Rmd"))
)
