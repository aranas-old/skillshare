library(shinyjs)
library(shinyBS)
library(DT)
library(plotly)
require(visNetwork, quietly = TRUE)

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

css <- ".mandatory_star { color: red; }
        .shiny-output-error { visibility: hidden; }
        .shiny-output-error:before {
          visibility: visible;
          content: 'An error occurred. Please contact the admin.'; }
       "

shinyUI(
  navbarPage(title="Skillshare database",
    tabPanel("Data",
      div(class="outer",
          
             tagList(
               tags$head(
                 tags$link(rel="stylesheet", type="text/css"),#href="style.css"), through css we can edit the text size/font family
                 tags$script("Shiny.addCustomMessageHandler('resetValue', function(variableName) {
                                            Shiny.onInputChange(variableName, null);});"),
                 useShinyjs(),
                 shinyjs::inlineCSS(css)
                 )
             ),
          fluidRow(
             column(6,visNetworkOutput("network")),
            column(6,DT::dataTableOutput("database"), tags$hr())) 
             ))
    ))
    
#   fluidRow(
#     column(5,
#            tagList(
#              tags$head(
#                tags$link(rel="stylesheet", type="text/css")#href="style.css"), through css we can edit the text size/font family
#              )
#            ),
#            visNetworkOutput("network")),
#     column(7,
#            tabsetPanel(
#              tabPanel("Table",
#                       DT::dataTableOutput("database"), tags$hr()),
#              tabPanel("Add data",  #pop-up when "Add Data" is clicked
#                       actionButton("buttonAdd", "Add Data"),
#                       bsModal("modaladd", "Add data", "buttonAdd", size = "small",
#                               HTML("Please fill in this form and press submit"),
#                               textInput("firstName", labelMandatory("First Name"), ""),
#                               textInput("lastName", labelMandatory("Last Name"), ""),
#                               textInput("email", labelMandatory("Email"), ""),
#                               uiOutput("skillsSelector"),
#                               textInput("skillsDetail", "Skill in detail", ""),
#                               uiOutput("needsSelector"),
#                               textInput("needsDetail", "Need in detail", ""),
#                               uiOutput("departmentSelector"),
#                               actionButton("submit", "Submit"))
#                       ),
#              tabPanel("Keyword Statistics",
#                       splitLayout(#width="100%", #cellHeights = c("100%", "100%"),
#                         plotlyOutput("piePlotSkills"),
#                         plotlyOutput("piePlotNeeds")))
#               )
#     ))
#   # TODO: Move this info to an "About" page
#   #, fluidRow(column(4, helpText('Click on one of the dot to get more details about individual: '), htmlOutput("data_individual")))
#   )
# )
