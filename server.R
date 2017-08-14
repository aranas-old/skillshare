### Libraries #####
library(googlesheets)
library(RColorBrewer)
library(igraph)
library(plotly)
require(shiny)
require(visNetwork, quietly = TRUE)
library(DT)
require(shiny)
library(shinyjs)
library(shinyBS)
useShinyjs()

### Set variables #####
fields <- c("Timestamp", "First_Name","Last_Name","Email","Skill","Skill_detail","Need","Need_detail","Department")
database_fname <- "data"

### Set password #####
Logged = TRUE; # TEMP, removed password procedure for debugging purposes
PASSWORD <- data.frame(Brukernavn = "imprs", Passord = "6289384392e39fe85938d7bd7b43ff48")
#####

  function(input, output, session) {
    source("www/Login.R",  local = TRUE)
    
    observe({
      if (USER$Logged == TRUE) {
        ### Form #####
        # Whenever a form is filled, aggregate all form data & add timestamp at beginning
        formData <- reactive({
          datain <- sapply(fields, function(x) input[[x]])
          datain$Timestamp <- format(Sys.time(), "%a %b %d %X %Y %Z")
          datain
        })
        # Only activate Submit button when name and email is provided
        observe({
          shinyjs::toggleState("submit", !is.null(input$First_Name) && input$First_Name != "" && !is.null(input$Email) && input$Email != "")
        })
        
        # When the Submit button is clicked, save the form data
        observeEvent(input$submit, {
          #when submit is pressed close pop-up window
          toggleModal(session, "modaladd", toggle = "close")
          saveData(formData()) 
        })
        
        observeEvent(input$BUTsubmit, {
          # find row that belong to the last name that was entered by person
          df = loadData()
          num <- which(tolower(as.character(df$Last_Name)) == tolower(as.character(input$Last_Name2)))
          toggleModal(session, "modaledit", toggle = "close")
          
          if (length(num) < 1){
            # give error message when the entered name does not match any of the names in the dataset 
            # TODO: it takes a long time for this to be evaluated, and the edit window already appears before the error message 
            observeEvent(num, {
              showModal(modalDialog(
                title = "Error",
                "Your name is not yet in the list. Maybe you typed it in wrong?",
                footer = tagList(
                  actionButton("BUTok", "Try again")
                )
              ))
              observeEvent(input$BUTok,{
                removeModal()
                toggleModal(session, "modaledit2", toggle = "close")
                toggleModal(session, "modaledit", toggle = "open")
                # TODO: would be nice if the window to type the name would reappear automatically
              })
              
            })
            } else{
            # do nothing
          }
          
          # TODO: display data that person entered before
          #updatePlaceholder("Skill2", label = "New Skill", value = as.character(df[num, 5]), placeholder = as.character(df[num, 5]))
          #updatePlaceholder("Need2", label = "New Need", value = as.character(df[num, 6]), placeholder = as.character(df[num, 6]))
          #updatePlaceholder("Skill_detail2", label = "Skills in detail", value = as.character(df[num, 8]), placeholder = as.character(df[num, 8]))
          #updatePlaceholder("Need_detail2", label = "Need in detail", value = as.character(df[num, 7]), placeholder = as.character(df[num, 7]))
          })
        
        observeEvent(input$Editsubmit,{
          # find row that belongs to the last name that was entered by person
          database <- loadData()
          num <- 1+which(tolower(as.character(database$Last_Name)) == tolower(as.character(input$Last_Name2)))
          num1 <- paste("E",num, sep="")
          num2 <- paste("H",num, sep="")
          num3 <- paste("F",num, sep="")
          num4 <- paste("G",num, sep="")
          
          # safe newly entered data to the person specific row 
          editData(num1,num2,num3,num4)
          
          # close pop-up when submit button is clicked
          toggleModal(session, "modaledit2", toggle = "close")
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
          input$Editsubmit
          dat = loadData()
          dat <- within(dat, Fullname <- paste(First_Name, Last_Name, sep = " "))  # new var "Fullname" so as to keep First+Last name separate
          dat
        })
        # Select relevant information to visualize in table
        output$database <- DT::renderDataTable({
          df = dat()
          df <- df[,c("First_Name","Last_Name","Skills","Needs")]
          #df <- df[order(df$First_Name),]
          df$Skills <- as.factor(df$Skills) #set columns to factor if search field should be dropdown
          datatable(df, filter = 'top') # put search fields on top of table
          data=data.frame(
          df,
          Details = shinyInput(actionButton, length(df$First_Name), 'details', label = "Details", onclick = 'Shiny.onInputChange(\"details_button\",  this.id)' ))
        },escape=FALSE)
          
        ### Network graph #######  
        
        #set content of graph (nodes & edges)
        nodes_pairs <- reactive({
          input$submit # update nodes whenever new data is submitted
          datanet <- dat()
          #find pairs of people where skills match needs and create new table with one row per pair (with repetitions)
          df_pairs <- data.frame()
          nodes <- data.frame()
          skills <- uppercase_first(remove_NA(string_to_list(datanet$Skills))) # reads ALL skills into a list
          needs <- c()  # reads Needs column and processes each line separately
          for (row in 1:nrow(datanet)){
            if (is.na(datanet$Needs[row])){
              needs <- rbind(needs, '')  # participant has no needs but we need to keep the row, so rbind it
            } else {
              needs <- rbind(needs, paste(uppercase_first(string_to_list(datanet$Needs[row])), collapse = ","))
            }
          }

          # now go through each participant row (we can force 1 line/participant, and remove "unique").
          for (row in 1:nrow(datanet)){
            currentskill <- uppercase_first(remove_NA(string_to_list(datanet$Skills[row])))
            currentneed <- uppercase_first(remove_NA(string_to_list((datanet$Needs[row]))))
            for (nskill in currentskill) {
              combined <- 0
              to_ind <- grep(paste("(^|,| ,)", nskill, "($|,)", sep = ""), needs, ignore.case=TRUE)
              if (length(to_ind)>0){  # first check if skill is needed
                to <- datanet$Fullname[to_ind]
                from <- rep(datanet$Fullname[row], length(to))
                title <- nskill
                if (length(to) != 0) {
                  combined <- rbind(from, to, title)
                  df_pairs <- rbind(df_pairs, as.data.frame(t(combined)))
                }
              }
            }
            if (!is.na(currentskill) || !is.na(currentneed)){  # add node if there are skills or needs --- or if you want we can add it anyway
              nodes <- rbind(nodes, data.frame(id = datanet$Fullname[row], 
                                               Skills = paste(currentskill, collapse = ","),
                                               Needs = paste(currentneed, collapse = ","), 
                                               Department = paste(datanet$Department[row], collapse = ",")))
            }
          }
          
          df_pairs$title <- as.character(df_pairs$title)
          df_pairs <- df_pairs[order(df_pairs$title),]
          df_pairs$title <- as.factor(df_pairs$title)
          info <- nodes
          count <- data.frame()
          for (row in 1:nrow(datanet)){  
            name <- datanet$Fullname[row]
            if (any(df_pairs == name)) {
              count = rbind(count, data.frame(Connections = length(which(df_pairs == name)) * 2))
            } else {
              count = rbind(count, data.frame(Connections = 4))
            }
          }
          nodes <- cbind(nodes, data.frame(Connections = count))
          
          graphinfo <- list(nodes = nodes, df_pairs = df_pairs, skills = skills, needs = needs) # concatenate all variables that should be accessed elsewhere in code
          graphinfo
        })
        
        #set visual parameters of graph
        networkgraph <- reactive({
          #access nodes & edges reactive values
          graphinfo <- nodes_pairs()
          nodes <- graphinfo$nodes
          df_pairs <- graphinfo$df_pairs
          skills <- graphinfo$skills
          #create color palette
          palet = colorRampPalette(brewer.pal(length(unique(skills)), "Pastel1"))
          colors = data.frame(skills = sort(unique(skills)), colors = c(color = palet(length(unique(skills)))))
          #set nodes parameters
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
            visOptions(highlightNearest = FALSE
                       #nodesIdSelection = TRUE
                       #selectedBy = list(variable = "Skills")
            ) %>%
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
          graphinfo <- nodes_pairs()
          nodes <- graphinfo$nodes
          df_pairs <- graphinfo$df_pairs
          skills <- graphinfo$skills
          #create color palette
          palet = colorRampPalette(brewer.pal(length(unique(skills)), "Pastel1"))
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
          graphinfo <- nodes_pairs()
          frequencies <-as.data.frame(table(graphinfo$skills))
          plot_ly(frequencies, labels = ~Var1, values = ~Freq, type = 'pie',
                  textposition = 'inside',
                  textinfo = 'percent',
                  #insidetextfont = list(color='#FFFFFF'),
                  hoverinfo = 'text',
                  text = frequencies$Var1,
                  marker = list(colors = c(color = brewer.pal(length(frequencies$Var1),"Pastel1")), # colors do not correspond to colors in network graph
                                line = list(color = '#FFFFFF', width = 5)),
                  showlegend = TRUE
          ) %>%
            layout(title = 'Skills',
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        })
        
        output$piePlotNeeds <- renderPlotly({
          graphinfo <- nodes_pairs()
          needs <- unlist(strsplit(graphinfo$needs, ","))
          frequencies <-as.data.frame(table(needs))
          #rownames(frequencies) = frequencies$needs
          plot_ly(frequencies, labels = frequencies$needs, values = ~Freq, type = 'pie',
                  textposition = 'inside',
                  textinfo = 'percent',
                  #insidetextfont = list(color='#FFFFFF'), # leave black font color
                  hoverinfo = 'text',
                  text = frequencies$needs,
                  marker = list(colors = c(color = brewer.pal(length(frequencies$needs),"Pastel1")),
                                line = list(color = '#FFFFFF', width = 5)),
                  showlegend = TRUE
          ) %>%
          layout(title = 'Needs',
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        })   
        
        ### Detailed view of user##### 
        value <- reactiveValues()
        
        observeEvent(c(
          input$details_button,
          input$current_node_id
          ), {
          df = dat()
          if (!is.null(input$current_node_id)) {
            current = which(df$Fullname==input$current_node_id)}
          else if (!is.null(input$details_button)){
            current = as.numeric(input$details_button)}
          showModal(modalDialog(
            title= "Details",
            renderUI({  # added na.omit on values that could be non available (we don't need to show NA to the user)
                str1 <- paste(df$Fullname[current],", ",unique(df$Department[current]))
                str2 <- paste(na.omit(unique(df$Email[current])))  # @Sophie: why unique here? - don't know :)
                str3 <- paste("My Skills:   ",unique(df$Skills[current]))
                str4 <- paste(na.omit(unique(data$Skills_details[current])))
                str5 <- paste("My Needs:    ",unique(df$Needs[current]))
                str6 <- paste(na.omit(unique(data$Needs_details[current])))
                HTML(paste(str1,str2," ",str3,str4," ",str5,str6,sep = '<br/>'))
            }),
            footer = modalButton("close"),actionButton("BUTedit","Edit data")
          ))
          value$current <- current
          session$sendCustomMessage(type = "resetValue", message = "current_node_id")
          session$sendCustomMessage(type = "resetValue", message = "details_button")
        })
        

        observeEvent(input$BUTedit, {
          df = dat()
          current = value$current
          showModal(modalDialog(
            title = "Edit Data",
            selectInput("skill", "New Skill", choices = df$Skills, selected = as.character(df[current, 5]), multiple = TRUE,
                        selectize = TRUE, width = NULL, size = NULL),
            conditionalPanel( condition = "input.skill == 'Other'",
                              textInput("new_keyword","New Keyword", value = NULL, placeholder = NULL)),
            textInput("Skill_detail2", "Skill in detail", value = as.character(df[current, 8]), placeholder = as.character(df[current, 8])),
            selectInput("Need","New Need", choices = df$Needs, selected = as.character(df[current, 6]), multiple = TRUE,
                        selectize = TRUE, width = NULL, size = NULL),
            textInput("Need_detail2", "Need in detail",value = as.character(df[current, 7]), placeholder = as.character(df[current, 7])),
            footer = tagList(
              modalButton("Cancel"),
              actionButton("Editsubmit", "Submit")
            )
          ))
      })
        
        ### Read/Write Database ### 
        saveData <- function(data) {
          data <- t(data)
          cat("\n", file=sprintf("db/%s.csv", database_fname), append=TRUE) # append new line to file
          write.table(x = data, file = sprintf("db/%s.csv", database_fname), 
                      quote = c(1,2,3,4,5,6,7,8,9), append = TRUE, sep=",", col.names = F, row.names = F)  # FIXME: quote = T(RUE) didn't add quotes around fields so I resolved to the c(1,2...) solution. 
        }
        
        loadData <- function() {
          data <- read.csv(file=sprintf("db/%s.csv", database_fname), head=TRUE, sep=",", stringsAsFactors=FALSE)
          data
        }
        
        # GeditData <- function(num1,num2,num3,num4) {
        #   #  Grab the Google Sheet
        #   sheet <- gs_title(table)
        #   #  Edit the data
        #   database <- gs_edit_cells(sheet, ws = worksheet, input = input$Skill2, anchor = num1)
        #   database <- gs_edit_cells(sheet, ws = worksheet, input = input$Skill_detail2, anchor = num2)
        #   database <- gs_edit_cells(sheet, ws = worksheet, input = input$Need2, anchor = num3)
        #   database <- gs_edit_cells(sheet, ws = worksheet, input = input$Need_detail2, anchor = num4)
        #   database
        # }
        
        # updatePlaceholder <- function(inputId, label = NULL, value = NULL, placeholder = NULL) {
        #   message <- list(label=label, value=value, placeholder=placeholder)
        #   session$sendInputMessage(inputId, message)
        # }
        
        #for "Detail" button
        shinyInput <- function(FUN, len, id, ...) {
          inputs <- character(len)
          for (i in seq_len(len)) {
            inputs[i] <- as.character(FUN(paste0(i), ...))
          }
          inputs
        }
        
        }
    })
}
