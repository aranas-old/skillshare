library(shinyjs)
library(shinyBS)
library(DT)
library(plotly)
require(visNetwork, quietly = TRUE)

css <- "
.shiny-output-error { visibility: hidden; }
.shiny-output-error:before {
visibility: visible;
content: 'An error occurred. Please contact the admin.'; }
}
"

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <-
  ".mandatory_star { color: red; }"


shinyUI(fluidPage(
  tags$script("Shiny.addCustomMessageHandler('resetValue', function(variableName) {
                                            Shiny.onInputChange(variableName, null);});"),
  useShinyjs(),
  shinyjs::inlineCSS(appCSS),
  titlePanel(title=div(img(src="images/combined_logos.png", height= 90), "Skillshare Database")),
  fluidRow(
    column(5,
           tagList(
             tags$head(
               tags$link(rel="stylesheet", type="text/css"),#href="style.css"), through css we can edit the text size/font family
               tags$script(type="text/javascript", src = "md5.js"),
               tags$script(type="text/javascript", src = "passwdInputBinding.js")
             )
           ),
           ## Login module;
           div(class = "login",
               uiOutput("uiLogin"),
               textOutput("pass")
           ),
           visNetworkOutput("network")),
    column(7,
           tabsetPanel(
           tabPanel("Table",
                    DT::dataTableOutput("database"), tags$hr()),
           tabPanel("Statistics",
                    splitLayout(#width="100%", #cellHeights = c("100%", "100%"),
                                plotlyOutput("piePlotSkills"),
                                plotlyOutput("piePlotNeeds"))),
           tabPanel("Form",  #pop-up when "Add Data" is clicked
                    actionButton("buttonAdd", "Add Data"),
                    bsModal("modaladd", "Add data", "buttonAdd", size = "small",
                            HTML("Please fill in this form and press submit"),
                            textInput("firstName", labelMandatory("First Name"), ""),
                            textInput("lastName", labelMandatory("Last Name"), ""),
                            textInput("email", labelMandatory("Email"), ""),
                            textInput("skills", labelMandatory("Skill"), ""),
                            textInput("skillsDetail", "Skill in detail", ""),
                            textInput("needs", "Need", ""),
                            textInput("needsDetail", "Need in detail", ""),
                            textInput("department", "Department", ""),
                            actionButton("submit", "Submit"))
                    )
              )
    ))
  # TODO: Move this info to an "About" page
  #, fluidRow(column(4, helpText('Click on one of the dot to get more details about individual: '), htmlOutput("data_individual")))
  )
)
