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
  
  
  #---------------------------------------------------
  #---------------------Downloads---------------------
  #---------------------------------------------------
  
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
  
}
