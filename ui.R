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
        .navbar-header .navbar-brand {padding:0;margin:-20px 0;padding:1px}
        .modal {overflow-y: scroll;}
        .loadmessage {
               position: fixed;
               top: 0px;
               left: 0px;
               width: 100%;
               padding: 5px 0px 5px 0px;
               text-align: center;
               font-weight: bold;
               font-size: 100%;
               color: #000000;
               background-color: '#CCFF66';
               z-index: 105;
             }
       "

shinyUI(fluidPage(
    titlePanel(title=div(class=".navbar-header .navbar-brand", img(src="images/combined_logos.png", height= 50))), #margin:-20px is an ugly solution but it works for now :S
    fluidRow(
      column(6,
             fluidRow(
             column(6,bsCollapse(id = "collapsehowto", open = "NULL",
                        bsCollapsePanel("How to",
                                        div('This database is meant to efficiently connect doctoral students by linking the ones who need help to the ones who can offer help - both in work as well as leisure.
                                                   
                                                   You can explore our database by interacting with both the network graph or the table. Just click on a circle to learn more about that persons skills & needs! Or explore who???s is helping out by hovering over the arrows!
                                                   
                                                   Would you like to join? Simply add your data and be part of our network!
                                                   
                                                   ')
                        ))),
             column(6,bsCollapse(id = "collapsejoin", open = "NULL",
                                 bsCollapsePanel("Join us!",
                                                 tags$div(
                                                 tags$h4('Thank you for joining the network!'),
                                                 tags$p("By filling in your skills you can give back to our PhD community and open possibilities for more interaction across institutes and disciplines."),
                                                 tags$p("Please also make sure to fill in some of your own needs. It may be that people do not even know they posses a skill that could be helpful to others.")
                                                ),
                                                actionButton("buttonAdd", "Add your Data"),
                                                bsModal("modaladd", "Add data", "buttonAdd", size = "small",
                                                         HTML("Please fill in this form and press submit"),
                                                         textInput("firstName", labelMandatory("First Name"), ""),
                                                         textInput("lastName", labelMandatory("Last Name"), ""),
                                                         textInput("email", labelMandatory("Email"), ""),
                                                         uiOutput("skillsSelector"),
                                                         textInput("newskill","New keyword describing your skill:"),
                                                         textInput("skillsDetail", "Skill in detail", ""),
                                                         uiOutput("needsSelector"),
                                                         textInput("newneed","New keyword describing your need:"),
                                                         textInput("needsDetail", "Need in detail", ""),
                                                         uiOutput("departmentSelector"),
                                                         actionButton("submit", "Submit"))
                                                )))),
             visNetworkOutput("network")),
      column(6,
             fluidRow(
               column(12,bsCollapse(id = "collapseabout", open = "NULL",
                                   bsCollapsePanel("About",
                                                   tags$div(
                                                     tags$h4("Hi, welcome to our little database of skills!"),
                                                     tags$p("Have you ever needed help during your PhD but were not sure who to ask? Do you miss teamwork as a lone science warrior? Do you want to connect with your peers outside work? With 25 doctoral students in the Language in Interaction consortium and XX in the International Max Planck Research School there is an immense amount of talent & interests surrounding us! This database is meant to efficiently connect doctoral students by linking the ones who need help to the ones who can offer help - both in work as well as leisure."),
                                                     tags$p("You can explore our database by interacting with both the network graph or the table. Just click on a circle to learn more about that person's skills & needs! Or explore who is is helping out by hovering over the arrows!"),
                                                     tags$p("Would you like to join? Simply add your data and be part of our network!"),
                                                     tags$p("For Questions concerning the Database please contact skillshare@email.com"),
                                                     tags$p("Credits...")))))),
             DT::dataTableOutput("database"), tags$hr())
    )
 )
)

    