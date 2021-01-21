sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem(text = "Search",
                 tabName = "search",
                 icon = icon("search")
        ),
        menuItem(text = "XML",
                 tabName = "xml",
                 icon = icon("code")
        ),
        menuItem(text = "Download",
                 tabName = "download",
                 icon = icon("download"))
    )
    
)

tab_search <- tabItem(tabName = "search",
                      fluidRow( #fluid row 1
                          box(title = "Source Language",
                              pickerInput(
                                  inputId = "src_lang",
                                  #label = "Source",
                                  choices = languages,
                                  options = list(title = "Choose language"))
                          ),
                          box(title = "Target Language",
                              pickerInput(
                                  inputId = "trgt_lang",
                                  #label = "Target",
                                  choices = languages,
                                  options = list(title = "Choose language"))
                          )
                      ),
                      fluidRow(
                          box(
                              title = "Live Search",
                              width = 12,
                              pickerInput(
                                  inputId = "src_search",
                                  #label = "Live search",
                                  choices = dataset$search,
                                  options = list(title = "search...",
                                                 `live-search` = TRUE)
                              )
                          )
                          
                      ),
                      fluidRow(
                          box(title = "Technical (source)",
                              textOutput(
                                  outputId = "choice_src_tech"
                              )
                          ),
                          box(title = "Technical (target)",
                              textOutput(
                                  outputId = "choice_trgt_tech"
                              )
                          )
                          
                      ),
                      fluidRow(
                          box(title = "Popular (source)",
                              textOutput(
                                  outputId = "choice_src_pop"
                              )
                          ),
                          box(title = "Popular (target)",
                              textOutput(
                                  outputId = "choice_trgt_pop"
                              )
                          )
                          
                      )

)

body <- dashboardBody(
    tabItems(
        tab_search,
        tabItem(tabName = "xml",
                h4("Show XML structure of the terminological record"),
                verbatimTextOutput("tbx_to_text")
        ),
        tabItem(tabName = "download",
                h4("Click below to download the TBX file for the chosen record"),
                uiOutput("get_the_item")
        )
    )
    
)


ui <- dashboardPage(
    
    dashboardHeader(title = "Demo EU Glossary"),
    
    sidebar,
    
    body
    
)
