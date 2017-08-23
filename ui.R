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
                 tags$style("#network{height:100vh !important;}"),
                 useShinyjs(),
                 shinyjs::inlineCSS(css)
                 )
             ),
          fluidRow(
             column(6,visNetworkOutput("network"),
                    absolutePanel(id = "welcome", class = "panel panel-default", fixed = TRUE
                    )),
            column(6,DT::dataTableOutput("database"), tags$hr(),
                   absolutePanel(id = "welcome", class = "panel panel-default", fixed = TRUE,
                                 draggable = TRUE, top = 60, left = 20, right = "auto", bottom = "auto", width=300, heigth = "auto",
                                 style = "opacity: 0.80",
                                 fluidPage(
                                   bsCollapse(id = "collapse", open = "How to",
                                   bsCollapsePanel("How to",
                                    div('This database is meant to efficiently connect doctoral students by linking the ones who need help to the ones who can offer help - both in work as well as leisure.
                                          
                                          You can explore our database by interacting with both the network graph or the table. Just click on a circle to learn more about that persons skills & needs! Or explore who???s is helping out by hovering over the arrows!
                                          
                                          Would you like to join? Simply add your data and be part of our network!
                                        
                                        '),
                                    actionButton("More", "Show Less")
                                    )),
                                   h4("Join the community!"),
                                   actionButton("buttonAdd", "Add Data"),
                                   bsModal("modaladd", "Add data", "buttonAdd", size = "small",
                                   HTML("Please fill in this form and press submit"),
                                   textInput("firstName", labelMandatory("First Name"), ""),
                                   textInput("lastName", labelMandatory("Last Name"), ""),
                                   textInput("email", labelMandatory("Email"), ""),
                                   uiOutput("skillsSelector"),
                                   textInput("skillsDetail", "Skill in detail", ""),
                                   uiOutput("needsSelector"),
                                   textInput("needsDetail", "Need in detail", ""),
                                   uiOutput("departmentSelector"),
                                   actionButton("submit", "Submit")),
                                   h4("Details:"),
                                   div('We could also display details in this window this way people could have the details visible while scanning through the table. I am not sure what is better.')
                                 ))
                   ) 
                  )
          )
          ),
    tabPanel("About",
             mainPanel(
               div('Hi, welcome to our little database of skills!

Have you ever needed help during your PhD but were not sure who to ask? Do you miss teamwork as a lone science warrior? Do you want to connect with your peers outside work? With X doctoral students in the Language in Interaction consortium and the International Max Planck Research School there is an immense amount of talent & interests surrounding us! This database is meant to efficiently connect doctoral students by linking the ones who need help to the ones who can offer help - both in work as well as leisure.

You can explore our database by interacting with both the network graph or the table. Just click on a circle to learn more about that persons skills & needs! Or explore who???s is helping out by hovering over the arrows!

Would you like to join? Simply add your data and be part of our network!
                   For Questions concerning the Database please contact skillshare@email.com
                   Credits ...')
             )
      
    ),
    tabPanel("Keyword Statistics",
             splitLayout(#width="100%", #cellHeights = c("100%", "100%"),
             plotlyOutput("piePlotSkills"),
             plotlyOutput("piePlotNeeds"))
    )
    )
)
