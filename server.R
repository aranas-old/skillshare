### Libraries #####
library(RColorBrewer)
library(igraph)
library(plotly)
require(shiny)
require(visNetwork, quietly = TRUE)
library(DT)
require(shiny)
library(shinyjs)
library(shinyBS)
require("RSQLite")
#useShinyjs()

### Set variables #####
fields <- c("timestamp", "firstName","lastName","email","skills","needs","needsDetail","skillsDetail","department")
database_fname <- "db/data"
sql_fname = "db/data.sqlite"

function(input, output, session) {
    observe({
        ### Form #####
        formData <- reactive({
          sapply(fields, function(x) input[[x]])  # Aggregate all form data
        })

        observe({
          # check if all mandatory fields (name, email etc) have a value
          mandatoryFilled <- vapply(c("firstName", "lastName", "skills", "email"),
                                   function(x) {
                                     !is.null(input[[x]]) && input[[x]] != ""
                                   },
                                   logical(1))
          mandatoryFilled <- all(mandatoryFilled)
          # enable/disable the submit button
          shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
        })

        observeEvent(input$submit, { # New data: when the Submit button is clicked, save the form data
          toggleModal(session, "modaladd", toggle = "close") # first close the pop-up window
          saveData(formData())
        })

        observeEvent(input$submitEdit,{
          editData()
          removeModal()  # close pop-up when submit button is clicked
          # # find row that belongs to the last name that was entered by person
          # num <- 1+which(tolower(as.character(database$lastName)) == tolower(as.character(input$Last_Name2)))
          #toggleModal(session, "modaledit", toggle = "close")  # close pop-up when submit button is clicked
        })

        # helper functions to handle text
        trim <- function (x) gsub("^\\s+|\\s+$", "", x) # returns string w/o leading or trailing whitespace
        uppercase_first <- function(x){  # we can uppercase the first letter, it's only for aesthetics
          substr(x, 1, 1) <- toupper(substr(x, 1, 1))
          x
        }
        remove_NA <- function(x){x[!is.na(x)]}
        string_to_list <- function(x){trim(unlist(strsplit(x, ",")))}

        ### Table #####
        # have datatable content as reactive value to be accessed later (i.e. pop-up for details)
        # update with every submit/edit
        dat <- reactive({
          input$submit
          input$submitEdit
          dat = loadData()
          dat
        })
        # Select relevant information to visualize in table
        output$database <- DT::renderDataTable({
          df = dat()
          df <- df[,c("firstName","lastName","skills","needs")]
          ##df <- df[order(df$firstName),]
          df$skills <- as.factor(df$skills) # set columns to factor if search field should be dropdown
          datatable(df, filter = 'top') # put search fields on top of table  # colnames = c('First Name', 'Last Name', 'Skills', 'Needs') ?
          data=data.frame(df,
                          Details = shinyInput(actionButton, length(df$firstName), 'details', label = "Details", onclick = 'Shiny.onInputChange(\"details_button\",  this.id)'))
        },escape=FALSE)

        ### Network graph #######

        # set content of graph (nodes & edges)
        node_pairs <- reactive({
          input$submit # update nodes whenever new data is submitted
          # find pairs of people where skills match needs
          df_pairs <- data.frame()
          nodes <- data.frame()
          datanet <- dat()
          skills <- uppercase_first(remove_NA(string_to_list(datanet$skills))) # reads ALL skills into a list
          needs <- c()  # reads Needs column and processes each line separately
          for (row in 1:nrow(datanet)){
            if (is.na(datanet$needs[row])){
              needs <- rbind(needs, '')  # participant has no needs but we need to keep the row, so rbind it
            } else {
              needs <- rbind(needs, paste(uppercase_first(string_to_list(datanet$needs[row])), collapse = ","))
            }
          }
          # now go through each participant row (we can force 1 line/participant, and remove "unique").
          for (row in 1:nrow(datanet)){
            currentskill <- uppercase_first(remove_NA(string_to_list(datanet$skills[row])))
            currentneed <- uppercase_first(remove_NA(string_to_list((datanet$needs[row]))))
            for (nskill in currentskill) {
              combined <- 0
              to_ind <- grep(paste("(^|,| ,)", nskill, "($|,)", sep = ""), needs, ignore.case=TRUE)
              if (length(to_ind)>0){  # first check if skill is needed
                to <- datanet$fullName[to_ind]
                from <- rep(datanet$fullName[row], length(to))
                title <- nskill
                if (length(to) != 0) {
                  combined <- rbind(from, to, title)
                  df_pairs <- rbind(df_pairs, as.data.frame(t(combined)))
                }
              }
            }
            nodes <- rbind(nodes, data.frame(id = datanet$fullName[row],
                                             skills = paste(currentskill, collapse = ","),
                                             needs = paste(currentneed, collapse = ","),
                                             department = paste(datanet$department[row], collapse = ",")))
          }
          
          df_pairs$title <- as.character(df_pairs$title)
          df_pairs <- df_pairs[order(df_pairs$title),]
          df_pairs$title <- as.factor(df_pairs$title)
          count <- data.frame()
          for (row in 1:nrow(datanet)){
            name <- datanet$fullName[row]
            if (any(df_pairs == name)) {
              count = rbind(count, data.frame(Connections = length(which(df_pairs == name)) * 2))
            } else {
              count = rbind(count, data.frame(Connections = 4))
            }
          }
          nodes <- cbind(nodes, data.frame(Connections = count))
          graphinfo <- list(nodes = nodes, df_pairs = df_pairs, skills = skills, needs = needs) # concat all variables that are accessed elsewhere in code
          graphinfo
        })

        # set visual parameters of graph
        networkgraph <- reactive({
          # access nodes & edges reactive values
          graphinfo <- node_pairs()
          nodes <- graphinfo$nodes
          df_pairs <- graphinfo$df_pairs
          skills <- graphinfo$skills
          # create color palette
          palet = colorRampPalette(brewer.pal(length(unique(skills)), "Paired"))  # Pastel1 has less colors
          colors = data.frame(skills = sort(unique(skills)), colors = c(color = palet(length(unique(skills)))))
          # set node parameters
          nodes$shape <- "dot"
          nodes$shadow <- TRUE # Nodes will drop shadow
          nodes$label <- NULL # Node label
          #nodes$title <- paste0("Name : ", nodes$id, "<br> Email : ", nodes$Email , "<br> Skill : ", nodes$Skills)
          nodes$title <- nodes$id
          nodes$size <- nodes$Connections # Node size
          nodes$borderWidth <- 2 # Node border width
          nodes$font.size <- 0
          #set nodes colors
          nodes$color.background <- "#4bd8c1"
          nodes$color.border <- "#42b2a0"
          nodes$color.highlight.background <- "#4bd8c1"
          nodes$color.highlight.border <- "red"
          #set edges parameters
          df_pairs$color <- colors$colors[match(df_pairs$title, colors$skills)]  # line color
          df_pairs$arrows <- "to" # arrows: 'from', 'to', or 'middle'
          df_pairs$smooth <- TRUE    # should the edges be curved?
          df_pairs$shadow <- FALSE    # edge shadow
          df_pairs$width <- 5    # edge shadow
          
          #output network
          visNetwork(nodes, df_pairs) %>%
            visIgraphLayout(layout = "layout_in_circle") %>%
            visOptions(highlightNearest = FALSE) %>%  #nodesIdSelection = TRUE #selectedBy = list(variable = "Skills")
            visInteraction(hover = TRUE, hoverConnectedEdges = TRUE, dragNodes = FALSE, zoomView = FALSE, tooltipDelay = 150, dragView = FALSE) %>%
            visEvents(click = "function(nodes){ Shiny.onInputChange('current_node_id', nodes.nodes); }")
        })

        output$network <- renderVisNetwork({
          networkgraph()
        })
        # If search function in datatable is used, clear selection)
        observeEvent(input$database_rows_all, {
          proxy = dataTableProxy('database')
          selectRows(proxy, NULL)
        })
        # interaction graph & datatable (both search function updates as well as selection)
          observe({
          indx = input$database_rows_all      # rows on all pages (after being filtered)
          sel = input$database_rows_selected
          if (!is.null(sel)){
            indx = sel
          }
          #access reactive values
          graphinfo <- node_pairs()
          nodes <- graphinfo$nodes
          df_pairs <- graphinfo$df_pairs
          skills <- graphinfo$skills
          #create color palette
          palet = colorRampPalette(brewer.pal(length(unique(skills)), "Paired"))   # Pastel1 has less colors
          colors = data.frame(skills = sort(unique(skills)), colors = c(color = palet(length(unique(skills)))))
          #change color
          nodes$color.background <- "#d3d3d3" #lightgray
          nodes$color.border <- "#d3d3d3"
          nodes$color.background[indx] <- "#4bd8c1"
          nodes$color.border[indx] <- "#42b2a0"
          #color all edges
          df_pairs$color <- colors$colors[match(df_pairs$title, colors$skills)]  # line color
          #gray out the ones belonging to gray nodes
          #FIX:so ugly, there must be a more elegant way?
          gray_out=intersect(which(df_pairs$from%in%setdiff(df_pairs$from,nodes$id[indx])),which(df_pairs$to%in%setdiff(df_pairs$to,nodes$id[indx])))
          df_pairs$color <- as.character(df_pairs$color) # weird interference with the factor level
          df_pairs$color[gray_out] <- "#d3d3d3"
          df_pairs$color <- as.factor(df_pairs$color)
          df_pairs$width <- 5    # edge shadow
          df_pairs$arrows <- "to"
          #FIX: for some reason the tip of the arrows do not change color
          #update network
          visNetworkProxy("network") %>%
          visUpdateNodes(nodes) %>%
          visUpdateEdges(df_pairs)
        })

        ### pie plots #####
        #TODO: plots should update together with graph and table when new data is submitted
        output$piePlotSkills <- renderPlotly({
          graphinfo <- node_pairs()
          frequencies <-as.data.frame(table(graphinfo$skills))
          plot_ly(frequencies, labels = ~Var1, values = ~Freq, type = 'pie',
                  textposition = 'inside',
                  textinfo = 'percent',
                  #insidetextfont = list(color='#FFFFFF'),
                  hoverinfo = 'text',
                  text = frequencies$Var1,
                  marker = list(colors = c(color = brewer.pal(length(frequencies$Var1),"Paired")), # colors do not correspond to colors in network graph
                                line = list(color = '#FFFFFF', width = 5)),
                  showlegend = TRUE
          ) %>%
            layout(title = 'Skills',
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        })

        output$piePlotNeeds <- renderPlotly({
          graphinfo <- node_pairs()
          needs <- unlist(strsplit(graphinfo$needs, ","))
          frequencies <-as.data.frame(table(needs))
          #rownames(frequencies) = frequencies$needs
          plot_ly(frequencies, labels = frequencies$needs, values = ~Freq, type = 'pie',
                  textposition = 'inside',
                  textinfo = 'percent',
                  #insidetextfont = list(color='#FFFFFF'), # leave black font color
                  hoverinfo = 'text',
                  text = frequencies$needs,
                  marker = list(colors = c(color = brewer.pal(length(frequencies$needs),"Paired")),
                                line = list(color = '#FFFFFF', width = 5)),
                  showlegend = TRUE
          ) %>%
          layout(title = 'Needs',
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        })

        ### Detailed view of user#####
        value <- reactiveValues()

        observeEvent(c(input$details_button,input$current_node_id), {
                          df = dat()
                          if (!is.null(input$current_node_id)) {
                            current = which(df$fullName==input$current_node_id)
                          } else {
                            current = as.numeric(input$details_button)
                          }
                          showModal(modalDialog(
                            title= paste(df$fullName[current],", ",df$department[current]),
                            renderUI({  # added na.omit on values that could be non available (we don't need to show NA to the user)
                                if (!is.na(df$needs[current])){
                                  needs <- paste("Needs:", df$needs[current])
                                } else {
                                  needs <- ""
                                }
                                HTML(sprintf("E-mail: %s </br> Skills: %s </br> %s </br> %s </br> %s</br></br>",  #TODO: Really ugly UI, fix
                                             df$email[current], df$skills[current], na.omit(df$skillsDetail[current]), needs, df$needsDetail[current]))
                            }),
                            footer = modalButton("close"),actionButton("buttonEdit","Edit data")
                          ))
                          value$current <- current
                          session$sendCustomMessage(type = "resetValue", message = "current_node_id")
                          session$sendCustomMessage(type = "resetValue", message = "details_button")
        })

        observeEvent(input$buttonEdit, {
            df = dat()
            current = value$current
            showModal(modalDialog(
                title = "Edit Data",
                textInput("firstName", "First Name", value = as.character(df[current, 2]), placeholder = as.character(df[current, 2])),
                textInput("lastName", "Last Name", value = as.character(df[current, 3]), placeholder = as.character(df[current, 3])),
                textInput("email", "Email", value = as.character(df[current, 4]), placeholder = as.character(df[current, 4])),
                selectInput("skill", "Skills", choices = df$skills, selected = as.character(df[current, 5]), multiple = TRUE,
                            selectize = TRUE, width = NULL, size = NULL),
                conditionalPanel(condition = "input.skills == 'Other'",
                                 textInput("new_keyword","New Keyword", value = NULL, placeholder = NULL)),
                textInput("skillDetail", "(Optional) comments on skills", value = as.character(df[current, 8]), placeholder = as.character(df[current, 8])),
                selectInput("needs", "Needs", choices = df$needs, selected = as.character(df[current, 6]), multiple = TRUE,
                            selectize = TRUE, width = NULL, size = NULL),
                textInput("needDetail", "(Optional) comments on needs",value = as.character(df[current, 7]), placeholder = as.character(df[current, 7])),
                textInput("department", "Department", value = as.character(df[current, 9]), placeholder = as.character(df[current, 9])),
                footer = tagList(modalButton("Cancel"), actionButton("submitEdit", "Submit")))
            )
        })

        editData <- function(data){
          print(data)
          #sql_db <- dbConnect(SQLite(), sql_fname)
          #dbDisconnect(sql_db)
        }
        
        ### SQL Lite database
        loadData <- function() {
          con <- dbConnect(SQLite(), sql_fname)
          data <- dbGetQuery(con, "SELECT * FROM skillshare")
          dbDisconnect(con)
          data
        }
        
        saveData <- function(data) {
          # TODO: Clean data (uppercase etc) before saving
          sql_db <- dbConnect(SQLite(), sql_fname)
          query <- sprintf("INSERT INTO skillshare VALUES (CURRENT_TIMESTAMP, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s %s')", 
                           data$firstName, data$lastName, data$email, data$skills, data$skillsDetail, data$needs, data$needsDetail, data$department, data$firstName, data$lastName)
          dbExecute(sql_db, query)
          dbDisconnect(sql_db)
        }

        # "Detail" button
        shinyInput <- function(FUN, len, id, ...) {
          inputs <- character(len)
          for (i in seq_len(len)) {
            inputs[i] <- as.character(FUN(paste0(i), ...))
          }
          inputs
        }
    })
}