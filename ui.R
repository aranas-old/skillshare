library(googlesheets)
#library(RColorBrewer)
library(igraph)
library(plotly)
library(visNetwork)

# data: edge matrix
key <-extract_key_from_url('https://docs.google.com/spreadsheets/d/1zsG-2R8CMXYjUKd4Cx_EzvIelFR7nGHp4ixSuuvdy7g/pubhtml?gid=572166108&single=true')
gap <- key %>% gs_key()
data <- gap %>% gs_read(ws = "examplar")
data <- within(data,  Fullname <- paste(First_Name, Last_Name, sep=" "))  # new var "Fullname" so as to keep First+Last name separate

#find pairs of people where skills match needs and create new table with one row per pair (with repetitions)
df_pairs <- data.frame()
nodes <- data.frame()
skills_sort <- character()
needs_sort <- character()
for (name in sort(unique(data$Fullname))) {
  combined <- 0
  indname <- which(data$Fullname %in% name)
  currentskill <- unlist(strsplit(data$Skills[indname],","))
  currentneed <- unlist(strsplit(data$Needs[indname],","))
  if (length(unique(currentskill))==1) {currentskill <- unique(currentskill)} 
  for (nskill in currentskill){
    skills_sort <- rbind(skills_sort,nskill)
    to_ind <- grepl(paste("^",nskill,"$", sep=""),data$Needs, ignore.case=TRUE)  # no need to lowercase, we can have a case-insensitive match. And ^nskill$ is a regular expression that looks for word boundaries ("R" doesn't match "spoRts" anymore)
    # TODO: W/o lowercasing, the skills contain both Yoga and yoga. Will do grep(nskill, skills_sort, ignore.case = TRUE) to see whether skill already exists and if so increase only frequency w/o adding it to the list
    to <- data$Fullname[to_ind]
    from <- rep(name,length(to))
    title <- nskill
    if (length(to)!=0) {combined <- rbind(from,to,title)
    df_pairs <- rbind(df_pairs,as.data.frame(t(combined)))
    }
    for (need in currentneed){
      needs_sort <- rbind(needs_sort,need)
    }
  }
  nodes <- rbind(nodes,data.frame(id = name, 
                                  Skills = paste(unique(currentskill), collapse = ",") ,
                                  Needs = paste(unique(data$Needs[indname]), collapse = ","),
                                  Department = paste(unique(data$Department[indname]), collapse = ",")))
}

#ui.R

shinyUI(fluidPage(titlePanel(title=div(img(src="images/MPI_logo.png"),"Skillshare Database")),
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
           dataTableOutput("nodes_data_from_shiny")),
           tabPanel("Statistics",
                    splitLayout(cellHeights = c("50%", "50%"),
                    plotlyOutput("piePlot1"),
                    plotlyOutput("piePlot2")))
    ))
  ),
  fluidRow(
    column(4,
           helpText('Click on one of the dot to get more details about individual: '), 
           htmlOutput("data_individual")),
    column(8,
           selectInput(inputId = "selskill", label = "Select person according to Skills", choices = skills_sort, multiple = TRUE),
           selectInput(inputId = "selneed", label = "Select person according to Needs", choices = needs_sort, multiple = TRUE))
    # textInput("text", label = h3("Name query"), value = "Search for name...")
  ),
  fluidRow(
    column(4),
    column(8,
           tags$div(class="header", checked=NA,
                    tags$p("Join us!"),
                    tags$a("Click Here to add your skills & needs to the database and connect with the IMPRS circle!!",     href="https://docs.google.com/forms/d/e/1FAIpQLSfi6awwyMs4gUyxPhEgcAFeZ4cM0MovlwWtzWnQlRowPBrcWw/viewform?usp=sf_link"))
    )
  )
)
)
