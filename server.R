### Libraries #####
library(googlesheets)
library(RColorBrewer)
library(igraph)
library(plotly)
#require(shiny)
require(visNetwork, quietly = TRUE) # library(visNetwork) 
library(DT)
library(shinyjs)

### Set variables #####
fields <- c("First_Name","Last_Name","email","Skill","Skill_detail","Need","Need_detail","Department")
table <- "reciprocity_database"
worksheet <- "examplar"



### Set password #####
Logged = TRUE; # TEMP, removed password procedure for debugging purposes
PASSWORD <- data.frame(Brukernavn = "imprs", Passord = "6289384392e39fe85938d7bd7b43ff48")
###
  
  function(input, output, session) {
    source("www/Login.R",  local = TRUE)
    
    observe({
      if (USER$Logged == TRUE) {
        
        ### Form #####
        # Whenever a form is filled, aggregate all form data & add timestamp at beginning
        formData <- reactive({
          datain <- sapply(fields, function(x) input[[x]]) #concatenate all input fields
          datain <- c(datain,Timestamp = Sys.time()) #timestamp does not work properly yet, but would be nice to have this info in the google sheets
          datain <- datain[c(9,1,2,3,4,5,6,7,8)] #putting timestamp at the beginning - must be an easier way to do this?
          datain
        })
        # Only activate Submit button when name and email is proided
        observe({
          shinyjs::toggleState("submit", !is.null(input$First_Name) && input$First_Name != "" && !is.null(input$email) && input$email != "")
        })
        
        # When the Submit button is clicked, save the form data
        observeEvent(input$submit, {
          saveData(formData()) 
        })
        
        ### Table #####   
        # Show the database
        # (show updated database when Submit is clicked)
        output$database <- DT::renderDataTable({
          input$submit 
          df = loadData()
          df <- df[,c("First_Name","Last_Name","Skills","Needs")]
          df <- df[order(df$First_Name),]
          df$Skills <- as.factor(df$Skills) #set columns to factor if search field should be dropdown
          datatable(df, filter = 'top') # put search fields on top of table
        })
        
        
        ### Network graph #######  
        
        #set content of graph (nodes & edges)
        nodes_pairs <- reactive({
          input$submit # update nodes whenever new data is submitted
          datanet <- loadData()
          datanet <- within(datanet,  Fullname <- paste(First_Name, Last_Name, sep=" "))  # new var "Fullname" so as to keep First+Last name separate
          
          #find pairs of people where skills match needs and create new table with one row per pair (with repetitions)
          df_pairs <- data.frame()
          nodes <- data.frame()
          
          trim <- function (x) gsub("^\\s+|\\s+$", "", x) # returns string w/o leading or trailing whitespace
          uppercase_first <- function(x){
            substr(x, 1, 1) <- toupper(substr(x, 1, 1))
            x
          }
          string_to_list <- function(x){trim(unlist(strsplit(x,",")))}
          
          skills <- c()  # or create unified data frame that contains skills and times seen
          skill_frequency <- c()
          needs <- c()
          needs_frequency <- c()
          skills_sort <- c()
          for (name in sort(unique(datanet$Fullname))) {
            combined <- 0
            indname <- which(datanet$Fullname %in% name)
            currentskill <- uppercase_first(string_to_list(datanet$Skills[indname]))   # we can uppercase the first letter, it's only for aesthetics 
            currentneed <- uppercase_first(string_to_list((datanet$Needs[indname])))  # @Sophie: if there is an issue with unique() we should resolve it here, not at the node
            for (nskill in currentskill[!is.na(currentskill)]){
              skills_sort <- rbind(skills_sort,nskill)  # only temporarily here, will remove
              if (is.null(skills) || length(grep(nskill, skills, ignore.case = TRUE))==0){
                skills <- c(skills,nskill)
                skill_frequency <- c(skill_frequency,1)
              } else { #  w/o lowercasing, the skills could contain both Yoga and yoga. We are only adding the first encounter and increasing the frequency each time.
                skill_idx = grep(nskill, skills, ignore.case = TRUE)
                skill_frequency[skill_idx] <- skill_frequency[skill_idx] + 1
              }
              to_ind <- grepl(paste("^",nskill,"$", sep=""),datanet$Needs, ignore.case=TRUE)  # no need to lowercase, we can have a case-insensitive match. And ^nskill$ is a regular expression that looks for word boundaries
              to <- datanet$Fullname[to_ind]
              from <- rep(name,length(to))
              title <- nskill
              if (length(to)!=0) {
                combined <- rbind(from,to,title)
                df_pairs <- rbind(df_pairs,as.data.frame(t(combined)))
              }
            }
            for (need in currentneed[!is.na(currentneed)]){
              if (is.null(needs) || length(grep(need, needs, ignore.case=TRUE))==0){
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
                                            Needs = paste(datanet$Needs[indname], collapse = ", "),
                                            Department = paste(datanet$Department[indname], collapse = ", ")))
          }
          
          df_pairs$title <- as.character(df_pairs$title)
          df_pairs      <-  df_pairs[order(df_pairs$title),]
          df_pairs$title <- as.factor(df_pairs$title)
          info <- nodes
          count = data.frame()
          for (name in sort(unique(datanet$Fullname))) {
            if (any(df_pairs == name)){
              count = rbind(count,data.frame(Connections = length(which(df_pairs == name))*2))
            }else {count = rbind(count,data.frame(Connections = 4))}
          }
          nodes <- cbind(nodes,data.frame(Connections = count))
          
          graphinfo <- list(nodes = nodes, df_pairs = df_pairs, skills_sort = skills_sort,needs = needs) # concatenate all variables that should be accessed elsewhere in code
          graphinfo
        })
        #set visual parameters of graph
        networkgraph  <- reactive({
          #access nodes & edges reactive values
          graphinfo <- nodes_pairs()
          nodes <- graphinfo$nodes
          df_pairs <- graphinfo$df_pairs
          skills <- graphinfo$skills
          #create color palette
          palet = colorRampPalette(brewer.pal(length(skills),"Pastel1"))
          colors = data.frame(skills = sort(skills), colors = c(color = palet(length(skills))))
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
          nodes$color.background<- "#4bd8c1"
          nodes$color.border<- "#42b2a0"
          nodes$color.highlight.background <- "#4bd8c1"
          nodes$color.highlight.border <- "red"
          #set edges parameters
          df_pairs$color <-colors$colors[match(df_pairs$title,colors$skills)]  # line color  
          df_pairs$arrows <- "to" # arrows: 'from', 'to', or 'middle'
          df_pairs$smooth <- TRUE    # should the edges be curved?
          df_pairs$shadow <- FALSE    # edge shadow
          df_pairs$width <- 5    # edge shadow
          #output network
          visNetwork(nodes,df_pairs) %>% 
            visIgraphLayout(layout = "layout_in_circle") %>%
            visOptions(highlightNearest = FALSE
                       #nodesIdSelection = TRUE
                       #selectedBy = list(variable = "Skills")
            ) %>%
            visInteraction(hover = T, hoverConnectedEdges = T, dragNodes = FALSE) %>%
            visEvents(click = "function(nodes){
                      Shiny.onInputChange('current_node_id', nodes.nodes);}"
            )
        })
        
        output$network <- renderVisNetwork({
          networkgraph()
        })
        
        # if search function is used (presented rows in database table change) update visuals of graph
        observeEvent(input$database_rows_all,{
          indx = input$database_rows_all      # rows on all pages (after being filtered)
          #access reactive values
          graphinfo <- nodes_pairs()
          nodes <- graphinfo$nodes
          #change color
          nodes$color.background <- "gray"
          nodes$color.border <- "gray"
          nodes$color.background[indx] <- "#4bd8c1"
          nodes$color.border[indx] <- "#42b2a0"
          #update network
          visNetworkProxy("network") %>%
            visUpdateNodes(nodes)
        })
        
        
        
        ### pie plots ##### 
        #TODO: Need to plot frequencies according to skill_frequency now
        #TODO: plots should update together with graph and table when new data is submitted
        output$piePlot1 <- renderPlotly({
          graphinfo <- nodes_pairs()
          skills_sort <- graphinfo$skills_sort
          skills_sort <- sort(skills_sort)
          tmpdata <-as.data.frame(table(skills_sort))
          rownames(tmpdata) = tmpdata$skills_sort
          colors1 = c(color = brewer.pal(length(unique(needs)),"Pastel1")) # colors do not correspond to colors in network graph
          plot_ly(tmpdata, labels = ~skills_sort, values = ~Freq, type = 'pie',
                  textposition = 'inside',
                  textinfo = 'percent',
                  #insidetextfont = list(color='#FFFFFF'),
                  hoverinfo = 'text',
                  mode = 'text',
                  text = tmpdata$skills_sort,
                  marker = list(colors = colors1,
                                line = list(color = '#FFFFFF', width = 5)),
                  showlegend = TRUE
          ) %>%
            layout(title = 'Skills',
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        })
        
        # TODO: Need to plot needs according to needs_frequency
        output$piePlot2 <- renderPlotly({
          graphinfo <- nodes_pairs()
          needs <- graphinfo$needs
          tmpdata <-as.data.frame(table(needs))
          rownames(tmpdata) = tmpdata$needs
          colors2 = c(color = brewer.pal(length(unique(needs)),"YlGnBu"))
          plot_ly(tmpdata, labels = ~needs, values = ~Freq, type = 'pie',
                  textposition = 'inside',
                  textinfo = 'percent',
                  #insidetextfont = list(color='#FFFFFF'),
                  hoverinfo = 'text',
                  text = tmpdata$needs,
                  marker = list(colors = colors2,
                                line = list(color = '#FFFFFF', width = 5)),
                  showlegend = TRUE
          ) %>%
            layout(title = 'Needs',
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        })   
        
        ### Detailed view of user##### 
        #TODO: This should in the future be a pop-up at click not a separate field on the GUI
        output$data_individual <- renderUI({  # added na.omit on values that could be non available (we don't need to show NA to the user)
          if (!is.null(input$current_node_id)) {
            str1 <- paste(input$current_node_id,", ",unique(info$Department[info$id == input$current_node_id]))
            str2 <- paste(na.omit(unique(info$Email[info$id == input$current_node_id])))  # @Sophie: why unique here?
            str3 <- paste("My Skills:   ",unique(info$Skills[info$id == input$current_node_id]))
            str4 <- paste(na.omit(unique(data$Skills_details[data$Fullname == input$current_node_id])))
            str5 <- paste("My Needs:    ",unique(info$Needs[info$id == input$current_node_id]))
            str6 <- paste(na.omit(unique(data$Needs_details[data$Fullname == input$current_node_id])))
            HTML(paste(str1,str2," ",str3,str4," ",str5,str6,sep = '<br/>'))
          }
        })
        
        ### Functions ####
        saveData <- function(data) {
          # Grab the Google Sheet
          sheet <- gs_title(table)
          # Add the data as a new row
          gs_add_row(sheet, ws = worksheet,input = data)
        }
        
        loadData <- function() {
          # Grab the Google Sheet
          sheet <- gs_title(table)
          # Read the data
          database <- gs_read_csv(sheet,ws = worksheet)
          database
        }
        
        editData <- function() {
       # TODO: this function is not used yet but we could implement that users can also update their entry instead of creating a new one altogether
       #  Grab the Google Sheet
         sheet <- gs_title(table)
       #  Read the data
        database <- gs_edit_cell(sheet,ws = worksheet)
        database
        }
        
        }
    })
    }
