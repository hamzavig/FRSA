#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#

# Define server logic
function(input, output, session) {

  
  #---------------------------------------------------
  #------------------Scenario Analysis----------------
  #---------------------------------------------------
  
  # Market -------------------------------------------
  
  # Define reactive data object for yieldCurve data frame
  yieldCurve_df <- reactiveVal(data.frame())
  
  # Define reactive data object for defaultCurve data frame
  defaultCurve_df <- reactiveVal(data.frame())

  # Define reactive list object for yieldCurve data frame
  yieldCurve_ls <- reactiveVal(list())
  
  # Define reactive list object for defaultCurve data frame
  defaultCurve_ls <- reactiveVal(list())
  
  
  # Update data frames with file input (bulk function)
  observeEvent(input$rf_import, {
    
    if(is.null(input$rf_file)){
      output$rf_file_notification <- renderUI({
        tags$div("Please add a file!", style = 'color: red;')
      })
    }else{
      filename <- input$rf_file$datapath
      file_df <- riskFile2dataframe(filename)
      yc_df <- file_df[file_df$rfType == 'YieldCurve',]
      dc_df <- file_df[file_df$rfType == 'DefaultCurve',]
      
      temp_yc_df <- bind_rows(yieldCurve_df(), yc_df)
      temp_dc_df <- bind_rows(defaultCurve_df(), dc_df)
      
      temp_yc_df <- temp_yc_df[!duplicated(temp_yc_df),]
      temp_dc_df <- temp_dc_df[!duplicated(temp_dc_df),]
      
      yc_empty_cols <- apply(temp_yc_df, 2, function(x) all(is.na(x) | x == ""))
      dc_empty_cols <- apply(temp_dc_df, 2, function(x) all(is.na(x) | x == ""))
      
      # Subset the data frame to remove empty columns
      temp_yc_df <- subset(temp_yc_df, select = !yc_empty_cols)
      temp_dc_df <- subset(temp_dc_df, select = !dc_empty_cols)
      
      yieldCurve_df(temp_yc_df)
      defaultCurve_df(temp_dc_df)
      
      if (nrow(yieldCurve_df()) > 0){
        new_yc_list <- riskFactors_df2list(temp_yc_df)
        yieldCurve_ls(new_yc_list)
      }
      
      if (nrow(defaultCurve_df()) > 0){
        new_dc_list <- riskFactors_df2list(temp_dc_df)
        defaultCurve_ls(new_dc_list)
      }
    }
  })
  
  
  # Update data frames with single risk factor insertion
  observeEvent(input$rf_add, {
    
    # Create a list of inputs
    inputs <- list(
      type = input$rf_type,
      label = input$rf_label,
      ref_date = as.character(input$rf_ref_date),
      tenors = list(input$rf_tenor1, input$rf_tenor2, input$rf_tenor3, input$rf_tenor4),
      rates = list(input$rf_rate1, input$rf_rate2, input$rf_rate3, input$rf_rate4)
    )
    
    # Unlist the tenors and rates
    tenors_unlisted <- unlist(inputs$tenors)
    rates_unlisted <- unlist(inputs$rates)
    
    # Create a new data frame with columns for the inputs
    new_row <- data.frame(
      rfType = inputs$type,
      label = inputs$label,
      referenceDate = inputs$ref_date,
      stringsAsFactors = FALSE
    )
    
    # Add columns for the unlisted tenors and rates
    for (i in 1:length(tenors_unlisted)) {
      new_row[[paste0('tenor.', i)]] <- tenors_unlisted[i]
      new_row[[paste0('rate.', i)]] <- rates_unlisted[i]
    }
    
    if(input$rf_type == 'YieldCurve'){
      
      temp_yc_df <- bind_rows(yieldCurve_df(), new_row)
      temp_yc_df <- temp_yc_df[!duplicated(temp_yc_df),]
      yieldCurve_df(temp_yc_df)
      
      new_yc_list <- riskFactors_df2list(temp_yc_df)
      yieldCurve_ls(new_yc_list)
      
    }else{
      
      temp_dc_df <- bind_rows(defaultCurve_df(), new_row)
      temp_dc_df <- temp_dc_df[!duplicated(temp_dc_df),]
      defaultCurve_df(temp_dc_df)
      
      new_dc_list <- riskFactors_df2list(temp_dc_df)
      defaultCurve_ls(new_dc_list)
    }
    
  })
  
  
  # Render yieldCurve data table
  output$yieldCurve_df <- renderDataTable({
    yieldCurve_df() %>% datatable(options = list(
      scrollX = TRUE,
      columnDefs = list(list(className = "nowrap", targets = "_all"))
    ),
    selection = list(mode = 'single'),
    editable = TRUE
    )
  })
  
  # Render defaultCurve data table
  output$defaultCurve_df <- renderDataTable({
    defaultCurve_df() %>% datatable(options = list(
      scrollX = TRUE,
      columnDefs = list(list(className = "nowrap", targets = "_all"))
    ),
    selection = list(mode = 'single'),
    editable = TRUE
    )
  })
  
  
  # Render Yield Curve plot on row select
  observeEvent(input$yieldCurve_df_rows_selected, {
    selected_row <- input$yieldCurve_df_rows_selected
    yc <- yieldCurve_ls()[[selected_row]]
    yc$TenorDates <- Filter(function(x) !is.null(x) & !is.na(x) & x != '', yc$TenorDates)
    yc$Tenors <- Filter(function(x) !is.null(x) & !is.na(x) & x != '', yc$Tenors)
    yc$Rates <- Filter(function(x) !is.null(x) & !is.na(x) & x != '', yc$Rates)
    
    output$ycDetails <- renderPrint({
      print(yc)
    })
    
    output$ycPlot <- renderPlot({
      plot(yc)
    })
    
  })
  
  # Render Default Curve plot on row select
  observeEvent(input$defaultCurve_df_rows_selected, {
    selected_row <- input$defaultCurve_df_rows_selected
    dc <- defaultCurve_ls()[[selected_row]]
    dc$TenorDates <- Filter(function(x) !is.null(x) & !is.na(x) & x != '', dc$TenorDates)
    dc$Tenors <- Filter(function(x) !is.null(x) & !is.na(x) & x != '', dc$Tenors)
    dc$Rates <- Filter(function(x) !is.null(x) & !is.na(x) & x != '', dc$Rates)
    
    output$dcDetails <- renderPrint({
      print(dc)
    })
    
    output$dcPlot <- renderPlot({
      plot(dc)
    })
    
  })
  
  
  # Duplicate selected row in dataset
  observeEvent(input$yc_duplicate, {
    selected_row <- input$yieldCurve_df_rows_selected
    new_row <- yieldCurve_df()[selected_row, ]
    new_row$label <- paste0(new_row$label, "_2")
    yieldCurve_df(bind_rows(yieldCurve_df(), new_row))
    
    new_yc_list <- riskFactors_df2list(yieldCurve_df())
    yieldCurve_ls(new_yc_list)
  })
  
  # Duplicate selected row in dataset
  observeEvent(input$dc_duplicate, {
    selected_row <- input$defaultCurve_df_rows_selected
    new_row <- defaultCurve_df()[selected_row, ]
    new_row$label <- paste0(new_row$label, "_2")
    defaultCurve_df(bind_rows(defaultCurve_df(), new_row))
    
    new_dc_list <- riskFactors_df2list(defaultCurve_df())
    defaultCurve_ls(new_dc_list)
  })
  
  # Remove selected row in dataste
  observeEvent(input$yc_remove, {
    selected_row <- input$yieldCurve_df_rows_selected
    yieldCurve_df(subset(yieldCurve_df(), label != yieldCurve_df()[selected_row, "label"]))
    
    new_yc_list <- riskFactors_df2list(yieldCurve_df())
    yieldCurve_ls(new_yc_list)
    
  })
  
  # Remove selected row in dataste
  observeEvent(input$dc_remove, {
    selected_row <- input$defaultCurve_df_rows_selected
    defaultCurve_df(subset(defaultCurve_df(), label != defaultCurve_df()[selected_row, "label"]))
    
    new_dc_list <- riskFactors_df2list(defaultCurve_df())
    defaultCurve_ls(new_dc_list)
  })
  
  # Observe GUI modification of a yield curve
  observeEvent(input$yieldCurve_df_cell_edit, {
    
    info <- input$yieldCurve_df_cell_edit
    temp_yc_df <- yieldCurve_df()
    temp_yc_df[info$row, info$col] <- info$value
    yieldCurve_df(temp_yc_df)
    
    new_yc_list <- riskFactors_df2list(yieldCurve_df())
    yieldCurve_ls(new_yc_list)
  })
  
  # Observe GUI modification of a default curve
  observeEvent(input$defaultCurve_df_cell_edit, {
    
    info <- input$defaultCurve_df_cell_edit
    temp_dc_df <- defaultCurve_df()
    temp_dc_df[info$row, info$col] <- info$value
    defaultCurve_df(temp_dc_df)
    
    new_dc_list <- riskFactors_df2list(defaultCurve_df())
    defaultCurve_ls(new_dc_list)
  })
  
  # Downloadable csv of selected dataset
  output$downloadYC <- downloadHandler(
    filename = function() {
      paste('user_yieldCurves', ".csv", sep = "")
    },
    content = function(file) {
      write.csv(yieldCurve_df(), file, row.names = FALSE)
    }
  )
  
  # Downloadable csv of selected dataset
  output$downloadDC <- downloadHandler(
    filename = function() {
      paste('user_defaultCurves', ".csv", sep = "")
    },
    content = function(file) {
      write.csv(defaultCurve_df(), file, row.names = FALSE)
    }
  )
  
  
  
  
  # Institutions -------------------------------------
  
  institution_vec <- reactiveVal(c())
  institution_ls <- reactiveVal(list())

  current_inst <- reactiveVal()
  
  inst_accounts <- reactiveVal(c())
  
  str_nodes <- reactiveVal(c())
  str_node_parents <- reactiveVal(c())
  
  nodes <- reactiveVal(c())
  
  ctrs <- reactiveVal(data.frame())
  
  market_obj_vec <- reactiveVal(c('None'))
  
  # Institution Creation / Deletion ------------------
  
  observeEvent(input$inst_add, {
    
    if (input$inst_name %in% institution_vec()) {
      output$inst_warning <- renderUI({
        tags$div("Institution already exists!", style = "color: red;")
      })
    }else if(input$inst_name == ''){
      output$inst_warning <- renderUI({
        tags$div("Please enter a value!", style = "color: orange;")
      })
      output$inst_panel <- reactive(FALSE)
      outputOptions(output, "inst_panel", suspendWhenHidden = FALSE)
    }else{
      output$inst_warning <- NULL
      new_inst <- input$inst_name
      temp_inst_vec <- c(institution_vec(),new_inst)
      institution_vec(temp_inst_vec)
      
      inst_tree <- createInstitution(new_inst)
      
      inst_list <- list(
        tree = inst_tree
      )
      
      inst_ls <- institution_ls()
      inst_ls <- append(inst_ls, list(inst_list))
      institution_ls(inst_ls)
      output$inst_panel <- reactive(TRUE)
      outputOptions(output, "inst_panel", suspendWhenHidden = FALSE)
    }
    
  })
  
  # Update dropdown choices when reactive values object changes for selection of institution view
  observe({
    updateSelectInput(session, inputId = "inst_view", choices = institution_vec())
  })
  
  # Observe and set current institution Node based on institution selection from dropdown
  observeEvent(input$inst_view, {
    if (length(institution_vec())>0){
      inst_id <- which(institution_vec() == input$inst_view)
      inst <- institution_ls()[[inst_id]]$tree
      current_inst(inst)
    }
  })
  
  # Observe and delete selected institution
  observeEvent(input$inst_delete, {
    
    inst_id <- which(institution_vec() == input$inst_view)
    inst_ls <- institution_ls()
    inst_ls[[inst_id]] <- NULL
    
    institution_ls(inst_ls)
    institution_vec(institution_vec()[!institution_vec() == input$inst_view])
    
    if (length(institution_vec()) == 0){
      output$inst_panel <- reactive(FALSE)
      outputOptions(output, "inst_panel", suspendWhenHidden = FALSE)
    }
    
  })
  
  
  
  
  # Multi Contract Import ----------------------------
  
  observeEvent(input$ct_import, {
    
    if(is.null(input$ct_file)){
      output$ct_file_notification <- renderUI({
        tags$div('Please upload a file!', style = 'color: red;')
      })
    }else{
      output$ct_file_notification <- NULL
      inst_id <- which(institution_vec() == input$inst_view)
      inst <- institution_ls()[[inst_id]]$tree
      
      path <- input$ct_file$datapath
      
      ct_df <- utils::read.csv(path)
      ct_type <- ct_df$contractType[1]
      
      if((ct_type == 'ANN' && input$ct_ptf_type == 'Annuities') || 
         (ct_type == 'PAM' && input$ct_ptf_type == 'PrincipalAtMaturities') ||
         (ct_type == 'Investments' && input$ct_ptf_type == 'Operations') ||
         (ct_type == 'OperationalCF' && input$ct_ptf_type == 'Operations')){
        
        if(input$ct_ptf_type != 'Operations'){
          output$ct_file_notification <- renderUI({
            tags$div('File imported!', style = 'color: green;')
          })
          
          ct_ptf <- samplePortfolio(path, 'contracts')
          inst <- assignContracts2Tree(inst, ct_ptf)
          
          current_inst(inst)
          node <- input$fc_view
          ct_df <- getContractsAsDataFrames(inst, node)
          ctrs(ct_df)
          
          output$fc_ui <- renderUI({
            tagList(
              DTOutput("fc_df"),
              br(),
              uiOutput("ct_buttons")
            )
          })
          
          output$error_log_df <- renderDataTable({
            current_inst()$errorLog %>% datatable(options = list(
              scrollX = TRUE,
              columnDefs = list(list(className = "nowrap", targets = "_all"))
            ),
            selection = list(mode = 'single'),
            editable = TRUE
            )
          })
          
          output$inst_market_df <- renderDataTable({
            current_inst()$rfs %>% datatable(options = list(
              scrollX = TRUE,
              columnDefs = list(list(className = "nowrap", targets = "_all"))
            ),
            selection = list(mode = 'single'),
            editable = TRUE
            )
          })

          output$fc_df <- renderDataTable({
            ctrs() %>% datatable(options = list(
              scrollX = TRUE,
              columnDefs = list(list(className = "nowrap", targets = "_all"))
            ),
            selection = list(mode = 'single')
            )
          })
          
        }else{
          ops_df <- samplePortfolio(path, 'operations')
          
          output$ct_file_notification <- renderUI({
            tags$div('Operations file not supported yet.', style = 'color: red;')
          })
        }
        
      }else{
        output$ct_file_notification <- renderUI({
          tags$div("Uploaded file doesn't match 'Portfolio Type'!", style = 'color: red;')
        })
      }
    }
  })
  
  # Single Contract Import ---------------------------
  
  # Observe and update account types vector
  observe({
    if (!is.null(current_inst())){
      inst <- current_inst()
      children <- Traverse(inst)
      children_names <- sapply(seq_along(children), function(i) children[[i]]$name)
      new_vector <- c(children_names)
      nodes(new_vector)
      updateSelectInput(session, inputId = "inst_account", choices = nodes())
    }
  })
  
  
  # Update dropdown choices when reactive values object changes for applicable market objects
  observe({
    yc_df <- yieldCurve_df()
    yc_labels <- yc_df$label
    new_labels <- c('None', yc_labels)
    market_obj_vec(new_labels)
    updateSelectInput(session, inputId = "ct_moc", choices = market_obj_vec())
  })
  
  
  # Structure ----------------------------------------
  
  find_node_by_name <- function(node, name) {
    if (node$name == name) {
      return(node)
    } else if (length(node$children) > 0) {
      for (child in node$children) {
        result <- find_node_by_name(child, name)
        if (!is.null(result)) {
          return(result)
        }
      }
    }
    return(NULL)
  }
  
  # Render institution node structure based on current selected institution
  output$inst_structure <- renderPrint({
    print(current_inst())
  })
  
  # Observe and update 'Node' dropdown in 'Structure' tab
  observe({
    if (!is.null(current_inst())){
      nodes <- nodes()
      new_vector <- c('New', nodes[-1])
      str_nodes(new_vector)
      updateSelectInput(session, inputId = "str_node", choices = str_nodes())
    }
  })
  
  observeEvent(input$str_node, {
    if (input$str_node == 'New'){
      output$str_node_options_1_1 <- renderUI({
        selectInput('str_node_parent', 'Parent', choices = nodes())
      })
      output$str_node_options_2_1 <- renderUI({
        textInput('str_new_node', 'New Node', placeholder = 'Add new node...')
      })
      output$str_node_options_2_2 <- renderUI({
        actionButton('str_add_new_node', 'Add', width = '100%')
      })
    }else{
      output$str_node_options_1_2 <- NULL
      output$str_node_options_2_2 <- NULL
      output$str_node_options_1_1 <- renderUI({
        actionButton('str_remove_node', 'Remove', width = '100%')
      })
      output$str_node_options_2_1 <- renderUI({
        actionButton('str_rename_node', 'Rename', width = '100%')
      })
      output$str_notification <- NULL
    }
  })
  
  
  observeEvent(input$str_add_new_node, {
    if(input$str_new_node > ''){

      if (input$str_new_node %in% str_node_parents()){
        output$str_notification <- renderUI({
          tags$div("A node with the same name already exists!", style = "color: red;")
        })
      }else{
        inst <- current_inst()
        parent <- input$str_node_parent
        parent_object <- find_node_by_name(inst, parent)
        new_node <- input$str_new_node
        parent_object$AddChild(new_node)
        
        inst <- reassignNonLeafContracts(inst)
        current_inst(inst)
        
        output$inst_structure <- renderPrint({
          print(current_inst())
        })
        
        inst <- current_inst()
        children <- Traverse(inst)
        children_names <- sapply(seq_along(children), function(i) children[[i]]$name)
        new_vector <- c(children_names)
        nodes(new_vector)
        nodes <- nodes()
        
        new_vector <- c('New', nodes[-1])
        str_nodes(new_vector)
        
        updateSelectInput(session, inputId = "str_node", choices = str_nodes())
        updateSelectInput(session, inputId = "inst_account", choices = nodes())
        
        output$str_notification <- NULL
        
      }
    }else{
      output$str_notification <- renderUI({
        tags$div("'New Node' cannot be empty!", style = "color: red;")
      })
    }
  })
  
  observeEvent(input$str_remove_node, {
    output$str_node_options_1_1 <- renderUI({
      actionButton('str_confirm_remove_node', 'Confirm', width = '100%')
    })
    output$str_node_options_2_1 <- renderUI({
      actionButton('str_cancel_remove_node', 'Cancel', width = '100%')
    })
  })
  
  observeEvent(input$str_cancel_remove_node, {
    output$str_node_options_1_1 <- renderUI({
      actionButton('str_remove_node', 'Remove', width = '100%')
    })
    output$str_node_options_2_1 <- renderUI({
      actionButton('str_rename_node', 'Rename', width = '100%')
    })
  })
  
  observeEvent(input$str_confirm_remove_node, {
    inst <- current_inst()
    node <- input$str_node
    node_object <- find_node_by_name(inst, node)
    parent <- node_object$parent
    parent$RemoveChild(node)
    
    output$inst_structure <- renderPrint({
      print(current_inst())
    })
    
    inst <- current_inst()
    children <- Traverse(inst)
    children_names <- sapply(seq_along(children), function(i) children[[i]]$name)
    new_vector <- c(children_names)
    nodes(new_vector)
    nodes <- nodes()
    
    new_vector <- c('New', nodes[-1])
    str_nodes(new_vector)
    
    updateSelectInput(session, inputId = "str_node", choices = str_nodes())
    updateSelectInput(session, inputId = "inst_account", choices = nodes())
  })
  
  observeEvent(input$str_rename_node, {
    output$str_node_options_1_1 <- renderUI({
      textInput('str_new_node_name', 'New Name', placeholder = 'Add new name...')
    })
    output$str_node_options_2_1 <- renderUI({
      actionButton('str_rename_node_2', 'Rename', width = '100%')
    })
    output$str_node_options_2_2 <- renderUI({
      actionButton('str_cancel_rename_node', 'Cancel', width = '100%')
    })
  })
  
  observeEvent(input$str_cancel_rename_node, {
    output$str_node_options_1_1 <- renderUI({
      actionButton('str_remove_node', 'Remove', width = '100%')
    })
    output$str_node_options_2_1 <- renderUI({
      actionButton('str_rename_node', 'Rename', width = '100%')
    })
    output$str_node_options_2_2 <- NULL
    output$str_notification <- NULL
  })
  
  observeEvent(input$str_rename_node_2, {
    if(input$str_new_node_name > ''){
      inst <- current_inst()
      node <- input$str_node
      node_object <- find_node_by_name(inst, node)
      node_object$name <- input$str_new_node_name
      
      output$inst_structure <- renderPrint({
        print(current_inst())
      })
      
      inst <- current_inst()
      children <- Traverse(inst)
      children_names <- sapply(seq_along(children), function(i) children[[i]]$name)
      new_vector <- c(children_names)
      nodes(new_vector)
      nodes <- nodes()
      
      new_vector <- c('New', nodes[-1])
      str_nodes(new_vector)
      
      updateSelectInput(session, inputId = "str_node", choices = str_nodes())
      updateSelectInput(session, inputId = "inst_account", choices = nodes())
      
      output$str_notification <- NULL

    }else{
      output$str_notification <- renderUI({
        tags$div("'New Name' cannot be empty!", style = "color: red;")
      })
    }
    
    
    
  })
  
  # Financial Contracts ------------------------------
  
  observe({
    updateSelectInput(session, inputId = "fc_view", choices = nodes())
  })
  
  observeEvent(input$fc_view, {
    if(!is.null(nodes())){
      inst <- current_inst()
      node <- input$fc_view
      df <- getContractsAsDataFrames(inst, node)
      ctrs(df)
      
      output$fc_ui <- renderUI({
        tagList(
          DTOutput("fc_df"),
          br(),
          uiOutput("ct_buttons")
        )
      })
      
      output$fc_df <- renderDataTable({
        ctrs() %>% datatable(options = list(
          scrollX = TRUE,
          columnDefs = list(list(className = "nowrap", targets = "_all"))
        ),
        selection = list(mode = 'single')
        )
      })
      
      output$ct_buttons <- renderUI({
        tagList(
          actionButton("ct_details", "Details"),
          actionButton("ct_duplicate", "Duplicate"),
          actionButton("ct_remove", "Remove"),
          actionButton("ct_move", "Move"),
          div(downloadButton("ct_download", "Download"), style = "float:right")
        )
      })
    }
  })
  
  observeEvent(input$ct_details, {
    inst <- current_inst()
    selected_row <- input$fc_df_rows_selected
    if(!is.null(selected_row)){
      ct <- ctrs()[selected_row,]
      ctid <- ct$contractID
      
      ctObject <- getSingleContract(inst, ctid)
      ct_details_df <- as.data.frame(ctObject$contractTerms)
      
      output$fc_ui <- renderUI({
        tagList(
          DTOutput("fc_df"),
          br(),
          uiOutput("ct_buttons"),
          plotOutput("ev_plot"),
          DTOutput("ev_df")
        )
      })
      
      output$fc_df <- renderDataTable({
        ct_details_df %>% datatable(options = list(
          scrollX = TRUE,
          columnDefs = list(list(className = "nowrap", targets = "_all"))
        ),
        selection = list(mode = 'single'),
        editable = TRUE
        )
      })
      
      evs <- EventSeries(ctObject, "https://demo.actusfrf.org:8080/", RFConn())
      
      output$ev_plot <- renderPlot({
        cashflowPlot(evs)
      })

      output$ev_df <- renderDataTable({
        evs$events_df %>% datatable(options = list(
          scrollX = TRUE,
          columnDefs = list(list(className = "nowrap", targets = "_all"))
        ),
        selection = list(mode = 'single')
        )
      })
      
      output$ct_buttons <- renderUI({
        actionButton("ct_details_close", "Close")
      })
    }
  })
  
  observeEvent(input$fc_df_cell_edit, {
    
    inst <- current_inst()
    info <- input$fc_df_cell_edit
    row <- ctrs()[info$row,]
    node <- row$node
    ctid <- row$contractID
    term <- info$col
    value <- info$value
    
    inst <- updateContract(inst, node, ctid, term, value)
    current_inst(inst)
    
    node <- input$fc_view
    df <- getContractsAsDataFrames(current_inst(), node)
    ctrs(df)
    
    if(term == 6){
      ctid <- value
    }
    
    ctObject <- getSingleContract(inst, ctid)
    ct_details_df <- as.data.frame(ctObject$contractTerms)

    output$fc_df <- renderDataTable({
      ct_details_df %>% datatable(options = list(
        scrollX = TRUE,
        columnDefs = list(list(className = "nowrap", targets = "_all"))
      ),
      selection = list(mode = 'single'),
      editable = TRUE
      )
    })

    evs <- EventSeries(ctObject, "https://demo.actusfrf.org:8080/", RFConn())

    output$ev_plot <- renderPlot({
      cashflowPlot(evs)
    })

    output$ev_df <- renderDataTable({
      evs$events_df %>% datatable(options = list(
        scrollX = TRUE,
        columnDefs = list(list(className = "nowrap", targets = "_all"))
      ),
      selection = list(mode = 'single')
      )
    })
    
  })
  
  observeEvent(input$ct_details_close, {
    
    output$fc_ui <- renderUI({
      tagList(
        DTOutput("fc_df"),
        br(),
        uiOutput("ct_buttons")
      )
    })
    
    output$fc_df <- renderDataTable({
      ctrs() %>% datatable(options = list(
        scrollX = TRUE,
        columnDefs = list(list(className = "nowrap", targets = "_all"))
      ),
      selection = list(mode = 'single')
      )
    })
    
    output$ct_buttons <- renderUI({
      tagList(
        actionButton("ct_details", "Details"),
        actionButton("ct_duplicate", "Duplicate"),
        actionButton("ct_remove", "Remove"),
        actionButton("ct_move", "Move"),
        div(downloadButton("ct_download", "Download"), style = "float:right")
      )
    })
  })
  
  
  observeEvent(input$ct_duplicate, {
    
    inst <- current_inst()
    selected_row <- input$fc_df_rows_selected
    if(!is.null(selected_row)){
      
      ct <- ctrs()[selected_row,]
      ctid <- ct$contractID
      node <- ct$node
      
      inst <- duplicateContract(inst, node, ctid)

      current_inst(inst)
      node <- input$fc_view
      df <- getContractsAsDataFrames(current_inst(), node)
      ctrs(df)
      
      output$error_log_df <- renderDataTable({
        current_inst()$errorLog %>% datatable(options = list(
          scrollX = TRUE,
          columnDefs = list(list(className = "nowrap", targets = "_all"))
        ),
        selection = list(mode = 'single'),
        editable = TRUE
        )
      })

      output$inst_market_df <- renderDataTable({
        current_inst()$rfs %>% datatable(options = list(
          scrollX = TRUE,
          columnDefs = list(list(className = "nowrap", targets = "_all"))
        ),
        selection = list(mode = 'single'),
        editable = TRUE
        )
      })

      output$fc_df <- renderDataTable({
        ctrs() %>% datatable(options = list(
          scrollX = TRUE,
          columnDefs = list(list(className = "nowrap", targets = "_all"))
        ),
        selection = list(mode = 'single')
        )
      })
    }
    
  })
  
  
  observeEvent(input$ct_remove, {
    
    inst <- current_inst()
    selected_row <- input$fc_df_rows_selected
    if(!is.null(selected_row)){
      
      ct <- ctrs()[selected_row,]
      ctid <- ct$contractID
      node <- ct$node
      
      inst <- removeContract(inst, node, ctid)
      current_inst(inst)
      
      node <- input$fc_view
      df <- getContractsAsDataFrames(current_inst(), node)
      ctrs(df)
      
      output$fc_df <- renderDataTable({
        ctrs() %>% datatable(options = list(
          scrollX = TRUE,
          columnDefs = list(list(className = "nowrap", targets = "_all"))
        ),
        selection = list(mode = 'single')
        )
      })
      
    }
  })
  
  
  observeEvent(input$ct_move, {
    inst <- current_inst()
    selected_row <- input$fc_df_rows_selected
    if(!is.null(selected_row)){
      ct <- ctrs()[selected_row,]
      node <- ct$node
      
      leave_nodes <- Traverse(inst$leaves)
      leave_names <- sapply(seq_along(leave_nodes), function(i) leave_nodes[[i]]$name)
      leave_names <- leave_names[leave_names != node]
      
      output$ct_buttons <- renderUI({
        tagList(
          selectInput('ct_target_node', 'Target Node', choices = leave_names),
          actionButton('ct_move_2', 'Move'),
          actionButton("ct_details_close", "Cancel")
        )
      })
    }
  })
  
  observeEvent(input$ct_move_2, {
    
    inst <- current_inst()
    selected_row <- input$fc_df_rows_selected
    if(!is.null(selected_row)){
      
      ct <- ctrs()[selected_row,]
      ctid <- ct$contractID
      source_node <- ct$node
      target_node <- input$ct_target_node
      
      source_nodeObject <- FindNode(inst, source_node)
      target_nodeObject <- FindNode(inst, target_node)
      
      ctObject <- getSingleContract(source_nodeObject, ctid)
      inst <- removeContract(inst, source_node, ctid)
      current_inst(inst)
      
      ctObject$contractTerms$node <- target_node
      target_nodeObject$contracts <- c(target_nodeObject$contracts, ctObject)
      
      node <- input$fc_view
      df <- getContractsAsDataFrames(current_inst(), node)
      ctrs(df)
      
      output$fc_df <- renderDataTable({
        ctrs() %>% datatable(options = list(
          scrollX = TRUE,
          columnDefs = list(list(className = "nowrap", targets = "_all"))
        ),
        selection = list(mode = 'single')
        )
      })
      
      output$ct_buttons <- renderUI({
        tagList(
          actionButton("ct_details", "Details"),
          actionButton("ct_duplicate", "Duplicate"),
          actionButton("ct_remove", "Remove"),
          actionButton("ct_move", "Move"),
          div(downloadButton("ct_download", "Download"), style = "float:right")
        )
      })
    }
    
  })
  
  
  output$ct_download <- downloadHandler(
    
    filename = function() {
      paste("financial_contracts_", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      
      ann_cols <- c('node','calendar','businessDayConvention','endOfMonthConvention','contractType','statusDate','contractRole',
                    'legalEntityIDRecordCreator','contractID','legalEntityIDCounterparty','cycleAnchorDateOfInterestPayment',
                    'cycleOfInterestPayment','nominalInterestRate','dayCountConvention','accruedInterest','cyclePointOfInterestPayment',
                    'currency','amortizationDate','contractDealDate','initialExchangeDate','premiumDiscountAtIED','maturityDate',
                    'notionalPrincipal','cycleAnchorDateOfPrincipalRedemption','cycleOfPrincipalRedemption','nextPrincipalRedemptionPayment',
                    'terminationDate','cycleAnchorDateOfRateReset','cycleOfRateReset','rateSpread','marketObjectCodeOfRateReset',
                    'cyclePointOfRateReset','rateMultiplier','description','contrStrucObj.marketObjectCode','contrStruc.referenceType','contrStruc.referenceRole') 
      
      pam_cols <- c('node','calendar','businessDayConvention','endOfMonthConvention','contractType','statusDate','contractRole',
                   'legalEntityIDRecordCreator','contractID','legalEntityIDCounterparty','cycleAnchorDateOfInterestPayment','cycleOfInterestPayment',
                   'arrayCycleAnchorDateOfInterestPayment','arrayCycleOfInterestPayment','nominalInterestRate','dayCountConvention','accruedInterest',
                   'capitalizationEndDate','cycleAnchorDateOfInterestCalculationBase','cycleOfInterestCalculationBase','interestCalculationBase',
                   'interestCalculationBaseAmount','cyclePointOfInterestPayment','currency','amortizationDate','contractDealDate','initialExchangeDate','premiumDiscountAtIED',
                   'maturityDate','notionalPrincipal','cycleAnchorDateOfPrincipalRedemption','cycleOfPrincipalRedemption','nextPrincipalRedemptionPayment',
                   'arrayCycleAnchorDateOfPrincipalRedemption','arrayCycleOfPrincipalRedemption','arrayNextPrincipalRedemptionPayment','arrayIncreaseDecrease','purchaseDate',
                   'priceAtPurchaseDate','terminationDate','priceAtTerminationDate','marketObjectCodeOfScalingIndex','scalingIndexAtStatusDate','cycleAnchorDateOfScalingIndex',
                   'cycleOfScalingIndex','scalingEffect','cycleAnchorDateOfRateReset','cycleOfRateReset','rateSpread','arrayCycleAnchorDateOfRateReset','arrayCycleOfRateReset',
                   'arrayRate','arrayFixedVariable','marketObjectCodeOfRateReset','cyclePointOfRateReset','fixingDays','rateMultiplier','description','contrStrucObj.marketObjectCode',
                   'contrStruc.referenceType','contrStruc.referenceRole')
      
      ops_cols <- c('node','contractType','contractID','contractRole','currency','notionalPrincipal','initialExchangeDate','maturityDate','repetition','frequency','times','inverted','description')
      
      ctrs <- getAllContracts(current_inst())
      split_list <- split(ctrs, sapply(ctrs, function(x) x$contractTerms$contractType))
      
      df_list <- list()
      
      for(type in split_list){
        
        ctrs <- lapply(type, function(ct) ct$contractTerms)
        
        if(ctrs[[1]]$contractType == 'ANN'){
          col_names <- ann_cols
        }else if(ctrs[[1]]$contractType == 'PAM'){
          col_names <- pam_cols
        }else{
          col_names <- ops_cols
        }

        crid <- 1:length(ctrs)
        df <- data.frame(crid)
        
        for(col in col_names) {
          df[col] <- unlist(sapply(ctrs, function(ct) if(is.null(ct[[col]])) 'NULL' else ct[[col]]))
        }
        
        df <- subset(df, select = -crid)
        
        df_list <- append(df_list, list(df))
      }
      
      df_list %>%
        imap(function(x,y){
          if(!is.null(x)){
            file_name <- glue("{x$contractType[1]}_financial_contracts.csv")
            readr::write_csv(x, file.path(temp_directory, file_name))
          }
        })
      
      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
      
    },
    contentType = "application/zip"
    
  )
  
  
  #---------------------------------------------------
  #---------------------Downloads---------------------
  #---------------------------------------------------
  
  # Dataset Download ---------------------------------
  
  # Reactive value for selected dataset
  datasetInput <- reactive({
    switch(input$dataset,
           "annuities" = annuities,
           "yieldCurves" = yieldCurves)
  })
  
  # Table of selected dataset
  output$table <- renderDataTable({
    datasetInput()
  }, options = list(scrollX = TRUE,
                    columnDefs = list(list(className = "nowrap", targets = "_all"))
                    )
  )
  
  # Table documentation of selected dataset
  output$tableDoc <- renderText({
    temp = Rd2HTML(Rd_fun(input$dataset), out = tempfile("doc"))
    content = read_file(temp)
    file.remove(temp)
    content
  })

  
  # Downloadable csv of selected dataset
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
  
  
  #---------------------------------------------------
  #----------------------Closure----------------------
  #---------------------------------------------------
  
  # Close App ----------------------------------------
  
  # Define function to trigger when close button is clicked
  observeEvent(input$close, {
    removeModal()  # Hide the confirmation dialog
    session$close()  # Close the Shiny session
  })
  
  # Define function to trigger when download button is clicked
  output$downloadAll <- downloadHandler(
    # Define download logic here
  )
  
  
  
}
