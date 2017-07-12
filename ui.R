library(googlesheets)
#library(RColorBrewer)
library(igraph)
library(plotly)
#library(visNetwork)
#rurequire(shiny)
require(visNetwork, quietly = TRUE) # library(visNetwork) 

# data: edge matrix
key <-extract_key_from_url('https://docs.google.com/spreadsheets/d/1zsG-2R8CMXYjUKd4Cx_EzvIelFR7nGHp4ixSuuvdy7g/pubhtml?gid=572166108&single=true')
gap <- key %>% gs_key()
data <- gap %>% gs_read(ws = "examplar")
data <- within(data,  Fullname <- paste(First_Name, Last_Name, sep=" "))  # new var "Fullname" so as to keep First+Last name separate

#find pairs of people where skills match needs and create new table with one row per pair (with repetitions)
df_pairs <- data.frame()
nodes <- data.frame()

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
uppercase_first <- function(x){
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
string_to_list <- function(x){trim(unlist(strsplit(x,",")))}

skills <- c()  # or create unified data frame that contains skills and times seen
skill_frequency <- c()
needs <- c()
needs_frequency <- c()

for (name in sort(unique(data$Fullname))) {
  print(name)
  combined <- 0
  indname <- which(data$Fullname %in% name)
  currentskill <- uppercase_first(string_to_list(data$Skills[indname]))   # we can uppercase the first letter, it's only for aesthetics 
  currentneed <- uppercase_first(string_to_list((data$Needs[indname])))  # @Sophie: if there is an issue with unique() we should resolve it here, not at the node
  for (nskill in currentskill[!is.na(currentskill)]){  # ignore NAs
    if (is.null(skills) || length(grep(nskill, skills, ignore.case=TRUE))==0){
        skills <- c(skills,nskill)
        skill_frequency <- c(skill_frequency,1)
    } else { #  w/o lowercasing, the skills could contain both Yoga and yoga. We are only adding the first encounter and increasing the frequency each time.
        skill_idx = grep(nskill, skills, ignore.case = TRUE)
        skill_frequency[skill_idx] <- skill_frequency[skill_idx] + 1
    }
    to_ind <- grepl(paste("^",nskill,"$", sep=""),data$Needs, ignore.case=TRUE)  # no need to lowercase, we can have a case-insensitive match. ^nskill$ is a regular expression that looks for word boundaries ("R" doesn't match "spoRts" anymore)
    to <- data$Fullname[to_ind]
    from <- rep(name,length(to))
    title <- nskill
    if (length(to)!=0) {  # @Sophie: so this is for the case where there are multiple links?
        combined <- rbind(from,to,title)
        df_pairs <- rbind(df_pairs,as.data.frame(t(combined)))
    }
    # if we leave this loop inside the "for (nskill in currentskill)" one, we won't parse cases that have needs but no skills. Is this what you wanted? I'm moving it out for now.
    #for (need in currentneed){
    #  needs_sort <- rbind(needs_sort,need)
    #}
  }
  
  for (need in currentneed[!is.na(currentneed)]){
    if (is.null(needs) || length(grep(need, needs, ignore.case = TRUE))==0){
      needs <- c(needs,need)
      needs_frequency <- c(needs_frequency,1)
    } else {
      need_idx = grep(need, needs, ignore.case = TRUE)
      needs_frequency[need_idx] <- needs_frequency[need_idx] + 1
    }
    #needs_sort <- rbind(needs_sort,need)
  }
  nodes <- rbind(nodes,data.frame(id = name,
                                  Skills = paste(currentskill, collapse = ", ") ,
                                  Needs = paste(data$Needs[indname], collapse = ", "),
                                  Department = paste(data$Department[indname], collapse = ", ")))
  # nodes <- rbind(nodes,data.frame(id = name, 
  #                                 Skills = paste(unique(currentskill), collapse = ",") ,
  #                                 Needs = paste(unique(data$Needs[indname]), collapse = ","),
  #                                 Department = paste(unique(data$Department[indname]), collapse = ",")))
}
print(skills)
print(skill_frequency)
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
           selectInput(inputId = "selskill", label = "Select person according to Skills", choices = skills, multiple = TRUE),
           selectInput(inputId = "selneed", label = "Select person according to Needs", choices = needs, multiple = TRUE))
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
