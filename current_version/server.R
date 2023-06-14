# This is for making the functional version of it that isn't based on a dynamic UI


library(shiny)
library(shinydashboard)
library(shinyalert)
library(rmarkdown)
library(knitr)
library(DT)
library(jsonlite)

source('app_fn.R')
input_values <- read.csv('input_values.csv')
element_values <- read.csv('element_values.csv')
reacts <- reactiveValues(examplechoice = 1, score = 0, input_scores = data.frame(inputs = input_values$name, values = rep(0, nrow(input_values))))
#because this needs to be reactive and manually made right now
output_tables <- reactiveValues()

shinyServer(function(input, output, session) {
  #Warning message
  shinyalert("Welcome!", "This is currently in development and should not be used for any reason other than that.", type = "info")
 
 
 for (j in 1:nrow(element_values)) {
   # Need local so that each item gets its own number. Without it, the value
   # of i in the renderPlot() will be the same across all instances, because
   # of when the expression is evaluated.
    local({
    el_num <- element_values$element[j]
    el_name <- element_values$name[j]
    el_desc <- element_values$desc[j]
    inputs <- input_values[which(input_values$element == el_num),]
    output[[paste0("element_", el_num)]] <- renderUI({
      fns <- list()
      tt <- list()
      for (i in 1:nrow(inputs)) {
        fn <- dynamic_input_fn(name = inputs$name[i], desc = inputs$desc[i], type = inputs$type[i], value = inputs$value[i], restrict = inputs$restrict[i], multiple = inputs$multiple[i])
        fns <- list(fns, fn)
        # if (!is.na(inputs$tooltip[i])) {
        #   tt <- bsTooltip(inputs$name[i], title = inputs$tooltip[i], placement = "top")
        #   fns <- list(fns, tt)
        # }
      }
      do.call(box, list(title = el_name, p(el_desc), fns, collapsible = TRUE, collapsed = FALSE, width = 8))
    })
    output[[paste0("example_", el_num)]] <- renderUI({
      fns <- list()
      fns <- lapply(1:nrow(inputs), function(i) {
        p(HTML(paste0(tags$strong(inputs$desc[i]), ": ", inputs[[paste0("example", reacts$examplechoice)]][i])))
      })
      do.call(box, list(title = paste0(el_name, " - ", gsub("[[:digit:]])", "", input$examples)), fns, collapsible = TRUE, collapsed = FALSE, width = 4))
    })
    })
 }
    
  
  observeEvent(input$examples, {
    reacts$examplechoice <- switch(input$examples,
           "1) Contaminated food" = 1, "2) Polluted river" = 2, "3) Human exposure" = 3, "Additional Information" = 4
           )
  })
  
  # output$sop_export <- downloadHandler(
  #   filename = function() {
  #     paste0("SPT_SOPFormat_", Sys.Date(), ".docx")
  #   },
  #   content = function(file) {
  #     input_table <- input_values[,c("element", "name", "desc")]
  #     element_names <- sapply(1:nrow(input_table), function(x) element_values$name[which(element_values$element == input_table$element[x])])
  #     input_data <- sapply(input_table$name, function(y) paste(input[[y]], collapse = ", "))
  #     dat <- data.frame(element = input_table$element, element_name = element_names, id = input_table$name, description = input_table$desc, input = input_data)
  #     rownames(dat) <- dat$id
  #     outputfile <- rmarkdown::render('template/sop_template.Rmd', 
  #                                     output_file = "test_output.docx", 
  #                                     params = list(dat = dat))
  #     file.copy(outputfile, file)
  #   }
  # )
  
  # output$plan_export <- downloadHandler(
  #   filename = function() {
  #     paste0("SPT_PlanFormat_", Sys.Date(), ".csv")
  #   },
  #   content = function(file) {
  #     input_table <- input_values[,c("element", "name", "desc")]
  #     element_names <- sapply(1:nrow(input_table), function(x) element_values$name[which(element_values$element == input_table$element[x])])
  #     input_data <- sapply(input_table$name, function(y) input[[y]])
  #     output_table <- cbind(element = input_table$element, element_name = element_names, id = input_table$name, description = input_table$desc, input = input_data)
  #     write.csv(output_table, file, row.names = FALSE)
  #   }
  # )
  
  output$raw_export <- downloadHandler(
    filename = function() {
      paste0("SPTrawdata_", Sys.Date(), ".JSON")
    },
    content = function(file) {
      export_dat <- sapply(names(input)[grep("^[[:digit:]]+[[:alpha:]]_", names(input))], function(x) input[[x]])
      output_dat <- jsonlite::toJSON(export_dat)
      write(output_dat, file)
    }
  )
  
  observeEvent(input$browser, {browser()})
  
  ## List Matrix tables
  
  #2B_2
  output$table_2B_2 <- renderDT({
    parameters <- input[["2B_2"]]
    if (!is.null(parameters)) {
      val <- input_values$value[which(input_values$name == "2B_2")]
      val <- unlist(strsplit(val, "!"))
      inputs <- unlist(strsplit(val[2], ";"))
      output_tables$table_2B_2 <- data.frame(matrix("", nrow = length(parameters), ncol = length(inputs), dimnames = list(parameters, inputs)))
      output_tables$table_2B_2
    }
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE))
  
  observeEvent(input$table_2B_2_cell_edit, {
    row  <- input$table1_cell_edit$row
    clmn <- input$table1_cell_edit$col
    output_tables$table_2B_2[row, clmn] <- input$table1_cell_edit$value
  })
  
  #4B_3
  output$table_4B_3 <- renderDT({
    parameters <- input[["4B_3"]]
    if (!is.null(parameters)) {
      val <- input_values$value[which(input_values$name == "4B_3")]
      val <- unlist(strsplit(val, "!"))
      inputs <- unlist(strsplit(val[2], ";"))
      output_tables$table_4B_3 <- data.frame(matrix("", nrow = length(parameters), ncol = length(inputs), dimnames = list(parameters, inputs)))
      output_tables$table_4B_3
    }
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE))
  
  observeEvent(input$table_4B_3_cell_edit, {
    row  <- input$table1_cell_edit$row
    clmn <- input$table1_cell_edit$col
    output_tables$table_4B_3[row, clmn] <- input$table1_cell_edit$value
  })
  
  #4C_3
  output$table_4C_3 <- renderDT({
    parameters <- input[["4C_3"]]
    if (!is.null(parameters)) {
      val <- input_values$value[which(input_values$name == "4C_3")]
      val <- unlist(strsplit(val, "!"))
      inputs <- unlist(strsplit(val[2], ";"))
      output_tables$table_4C_3 <- data.frame(matrix("", nrow = length(parameters), ncol = length(inputs), dimnames = list(parameters, inputs)))
      output_tables$table_4C_3
    }
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE))
  
  observeEvent(input$table_4C_3_cell_edit, {
    row  <- input$table1_cell_edit$row
    clmn <- input$table1_cell_edit$col
    output_tables$table_4C_3[row, clmn] <- input$table1_cell_edit$value
  })
  
  #5B_3
  output$table_5B_3 <- renderDT({
    parameters <- input[["5B_3"]]
    if (!is.null(parameters)) {
      val <- input_values$value[which(input_values$name == "5B_3")]
      val <- unlist(strsplit(val, "!"))
      inputs <- unlist(strsplit(val[2], ";"))
      output_tables$table_5B_3 <- data.frame(matrix("", nrow = length(parameters), ncol = length(inputs), dimnames = list(parameters, inputs)))
      output_tables$table_5B_3
    }
  }, editable = TRUE, options = list(dom = 't', ordering = FALSE, paging = FALSE))
  
  observeEvent(input$table_5B_3_cell_edit, {
    row  <- input$table1_cell_edit$row
    clmn <- input$table1_cell_edit$col
    output_tables$table_5B_3[row, clmn] <- input$table1_cell_edit$value
  })
  
  
  ## MODAL START - must be manually modified in this file
  #3A_1 -- modal start
  
  observeEvent(input$add_3A_1, {
    showModal(
      dynamic_modal(
        title = input_values$desc[which(input_values$name == "3A_1")],
        name = "3A_1",
        inputs = input_values$value[which(input_values$name == "3A_1")],
        helps = input_values$tooltip[which(input_values$name == "3A_1")],
        ok = "Add Value",
        size = "xl"
      )
    )
  })
  
  observeEvent(input$modal_ok_3A_1, {
    get_input_inds <- names(input)[grep("modal_3A_1", names(input))]
    output_tables$table_3A_1 <- rbind(output_tables$table_3A_1, sapply(get_input_inds, function(x) input[[x]]))
    colnames(output_tables$table_3A_1) <- unlist(strsplit(input_values$value[which(input_values$name == "3A_1")], split = ";"))
    output$table_3A_1 <- DT::renderDT(output_tables$table_3A_1, 
                                      selection = 'single', 
                                      options = list(paging = FALSE, dom = 't', ordering = FALSE), width = '50%')
    removeModal()
  })
  
  observeEvent(input$remove_3A_1, {
    if (nrow(output_tables$table_3A_1) == 1) {
      output_tables$table_3A_1 <- data.frame()
    }
    if (nrow(output_tables$table_3A_1) > 1) {
      output_tables$table_3A_1 <- output_tables$table_3A_1[-input$table_3A_1_rows_selected,,drop = FALSE]
  }
  })

  #3A_1 -- modal end
  
  #3A_2 -- modal start
  
  observeEvent(input$add_3A_2, {
    showModal(
      dynamic_modal(
        title = input_values$desc[which(input_values$name == "3A_2")],
        name = "3A_2",
        inputs = input_values$value[which(input_values$name == "3A_2")],
        helps = input_values$tooltip[which(input_values$name == "3A_2")],
        ok = "Add Value",
        size = "xl"
      )
    )
  })
  
  observeEvent(input$modal_ok_3A_2, {
    get_input_inds <- names(input)[grep("modal_3A_2", names(input))]
    output_tables$table_3A_2 <- rbind(output_tables$table_3A_2, sapply(get_input_inds, function(x) input[[x]]))
    colnames(output_tables$table_3A_2) <- unlist(strsplit(input_values$value[which(input_values$name == "3A_2")], split = ";"))
    output$table_3A_2 <- DT::renderDT(output_tables$table_3A_2, 
                                      selection = 'single', 
                                      options = list(paging = FALSE, dom = 't', ordering = FALSE), width = '50%')
    removeModal()
  })
  
  observeEvent(input$remove_3A_2, {
    if (nrow(output_tables$table_3A_2) == 1) {
      output_tables$table_3A_2 <- data.frame()
    }
    if (nrow(output_tables$table_3A_2) > 1) {
      output_tables$table_3A_2 <- output_tables$table_3A_2[-input$table_3A_2_rows_selected,,drop = FALSE]
    }
  })
  
  #3A_2 -- modal end
  
  #3A_3 -- modal start
  
  observeEvent(input$add_3A_3, {
    showModal(
      dynamic_modal(
        title = input_values$desc[which(input_values$name == "3A_3")],
        name = "3A_3",
        inputs = input_values$value[which(input_values$name == "3A_3")],
        helps = input_values$tooltip[which(input_values$name == "3A_3")],
        ok = "Add Value",
        size = "xl"
      )
    )
  })
  
  observeEvent(input$modal_ok_3A_3, {
    get_input_inds <- names(input)[grep("modal_3A_3", names(input))]
    output_tables$table_3A_3 <- rbind(output_tables$table_3A_3, sapply(get_input_inds, function(x) input[[x]]))
    colnames(output_tables$table_3A_3) <- unlist(strsplit(input_values$value[which(input_values$name == "3A_3")], split = ";"))
    output$table_3A_3 <- DT::renderDT(output_tables$table_3A_3, 
                                      selection = 'single', 
                                      options = list(paging = FALSE, dom = 't', ordering = FALSE), width = '50%')
    removeModal()
  })
  
  observeEvent(input$remove_3A_3, {
    if (nrow(output_tables$table_3A_3) == 1) {
      output_tables$table_3A_3 <- data.frame()
    }
    if (nrow(output_tables$table_3A_3) > 1) {
      output_tables$table_3A_3 <- output_tables$table_3A_3[-input$table_3A_3_rows_selected,,drop = FALSE]
    }
  })
  
  #3A_3 -- modal end
  
  #4D_1 -- modal start
  
  observeEvent(input$add_4D_1, {
    showModal(
      dynamic_modal(
        title = input_values$desc[which(input_values$name == "4D_1")],
        name = "4D_1",
        inputs = input_values$value[which(input_values$name == "4D_1")],
        helps = input_values$tooltip[which(input_values$name == "4D_1")],
        ok = "Add Value",
        size = "xl"
      )
    )
  })
  
  observeEvent(input$modal_ok_4D_1, {
    get_input_inds <- names(input)[grep("modal_4D_1", names(input))]
    output_tables$table_4D_1 <- rbind(output_tables$table_4D_1, sapply(get_input_inds, function(x) input[[x]]))
    colnames(output_tables$table_4D_1) <- unlist(strsplit(input_values$value[which(input_values$name == "4D_1")], split = ";"))
    output$table_4D_1 <- DT::renderDT(output_tables$table_4D_1, 
                                      selection = 'single', 
                                      options = list(paging = FALSE, dom = 't', ordering = FALSE), width = '50%')
    removeModal()
  })
  
  observeEvent(input$remove_4D_1, {
    if (nrow(output_tables$table_4D_1) == 1) {
      output_tables$table_4D_1 <- data.frame()
    }
    if (nrow(output_tables$table_4D_1) > 1) {
      output_tables$table_4D_1 <- output_tables$table_4D_1[-input$table_4D_1_rows_selected,,drop = FALSE]
    }
  })
  
  #4D_1 -- modal end
  
  #5C_1 -- modal start
  
  observeEvent(input$add_5C_1, {
    showModal(
      dynamic_modal(
        title = input_values$desc[which(input_values$name == "5C_1")],
        name = "5C_1",
        inputs = input_values$value[which(input_values$name == "5C_1")],
        helps = input_values$tooltip[which(input_values$name == "5C_1")],
        ok = "Add Value",
        size = "xl"
      )
    )
  })
  
  observeEvent(input$modal_ok_5C_1, {
    get_input_inds <- names(input)[grep("modal_5C_1", names(input))]
    output_tables$table_5C_1 <- rbind(output_tables$table_5C_1, sapply(get_input_inds, function(x) input[[x]]))
    colnames(output_tables$table_5C_1) <- unlist(strsplit(input_values$value[which(input_values$name == "5C_1")], split = ";"))
    output$table_5C_1 <- DT::renderDT(output_tables$table_5C_1, 
                                      selection = 'single',
                                      options = list(paging = FALSE, dom = 't', ordering = FALSE), width = '50%')
    removeModal()
  })
  
  observeEvent(input$remove_5C_1, {
    if (nrow(output_tables$table_5C_1) == 1) {
      output_tables$table_5C_1 <- data.frame()
    }
    if (nrow(output_tables$table_5C_1) > 1) {
      output_tables$table_5C_1 <- output_tables$table_5C_1[-input$table_5C_1_rows_selected,,drop = FALSE]
    }
  })
  
  #5C_1 -- modal end
  
  #6A_1 -- modal start

  observeEvent(input$add_6A_1, {
    showModal(
      dynamic_modal(
        title = input_values$desc[which(input_values$name == "6A_1")],
        name = "6A_1",
        inputs = input_values$value[which(input_values$name == "6A_1")],
        helps = input_values$tooltip[which(input_values$name == "6A_1")],
        ok = "Add Value",
        size = "xl"
      )
    )
  })
  
  observeEvent(input$modal_ok_6A_1, {
    get_input_inds <- names(input)[grep("modal_6A_1", names(input))]
    output_tables$table_6A_1 <- rbind(output_tables$table_6A_1, sapply(get_input_inds, function(x) input[[x]]))
    colnames(output_tables$table_6A_1) <- unlist(strsplit(input_values$value[which(input_values$name == "6A_1")], split = ";"))
    output$table_6A_1 <- DT::renderDT(output_tables$table_6A_1, 
                                      selection = 'single',
                                      options = list(paging = FALSE, dom = 't', ordering = FALSE), width = '50%')
    removeModal()
  })
  
  observeEvent(input$remove_6A_1, {
    if (nrow(output_tables$table_6A_1) == 1) {
      output_tables$table_6A_1 <- data.frame()
    }
    if (nrow(output_tables$table_6A_1) > 1) {
      output_tables$table_6A_1 <- output_tables$table_6A_1[-input$table_6A_1_rows_selected,,drop = FALSE]
    }
  })
  
  #6A_1 -- modal end
  
  # for (k in 1:nrow(input_values)) {
  #   observeEvent(input[[input_values$name[k]]], {
  #     reacts$input_scores$values[which(reacts$input_scores$names == input_values$name[k])] <- 1
  #     
  #   })
  # }
  # 
  # reactive({
  #   output$progress <- renderHTML(paste0("You have completed ", sum(reacts$input_scores$values, na.rm = TRUE)/length(reacts$input_scores$values), " %"))
  # })
  
})
