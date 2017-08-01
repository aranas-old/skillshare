#ui.R
# hallo
shinyUI(fluidPage(
  useShinyjs(),  # Include shinyjs
  titlePanel(title=div(img(src="images/MPI_logo.png", height= 100),"Skillshare Database")),
  fluidRow(
    column(6,
           tagList(
             tags$head(
               tags$link(rel="stylesheet", type="text/css"),#,href="style.css"),
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
    column(6,
           tabsetPanel(
           tabPanel("Table",
                    DT::dataTableOutput("database", width = 400), tags$hr()),
           tabPanel("Statistics",
                    splitLayout(cellHeights = c("50%", "50%"),
                    plotlyOutput("piePlot1"),
                    plotlyOutput("piePlot2"))),
           tabPanel("Form",
                    actionButton("BUTadd", "Add Data"),
                    #pop-up when "Add Data" is clicked
                    bsModal("modaladd", "Add data", "BUTadd", size = "small",
                      HTML("Please fill in this form and press submit in order to add your data"),
                      textInput("First_Name", "First Name", ""),
                      textInput("Last_Name", "Last Name", ""),
                      textInput("email", "email", ""),
                      textInput("Skill", "Skill", ""),
                      textInput("Skill_detail", "Skill in detail", ""),
                      textInput("Need", "Need", ""),
                      textInput("Need_detail", "Need in detail", ""),
                      textInput("Department", "Department", ""),
                      actionButton("submit", "Submit")),
                    #pop-up when "Edit Data" is clicked
                    actionButton("BUTedit", "Edit Data"),
                    bsModal("modaledit", "Edit data", "BUTedit", size = "small",
                      HTML("Please fill in your last name:"),
                      textInput("Last_Name2", "Last Name", ""),
                    actionButton("BUTsubmit", "Submit")),
                    bsModal("modaledit2", "Edit data", "BUTsubmit", size = "small",
                              HTML("Please update your information (please be patient, saving data may take some time):"),
                              textInput("Skill2", "New Skill", value = "",placeholder = "enter all your skills"),
                              textInput("Skill_detail2", "Skill in detail", "",placeholder = " "),
                              textInput("Need2", "New Need", value = "",placeholder = "enter all your needs"),
                              textInput("Need_detail2", "Need in detail","",placeholder = " "),
                              actionButton("Editsubmit", "Submit"))
       
                      )
              )
    ))
  ,
  fluidRow(
    column(4,
           helpText('Click on one of the dot to get more details about individual: '), 
           htmlOutput("data_individual"))
  )
  )
)
