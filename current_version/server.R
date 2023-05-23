# This is for making the functional version of it that isn't based on a dynamic UI


library(shiny)
library(shinydashboard)
library(shinyalert)
library(rmarkdown)
library(knitr)

source('app_fn.R')
input_values <- read.csv('input_values.csv')
element_values <- read.csv('element_values.csv')
reacts <- reactiveValues(examplechoice = 1, score = 0, input_scores = data.frame(inputs = input_values$name, values = rep(0, nrow(input_values))))

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
        if (!is.na(inputs$tooltip[i])) {
          tt <- bsTooltip(inputs$name[i], title = inputs$tooltip[i], placement = "top")
          fns <- list(fns, tt)
        }
      }
      do.call(box, list(title = el_name, p(el_desc), fns, collapsible = TRUE, collapsed = FALSE))
    })
    output[[paste0("example_", el_num)]] <- renderUI({
      fns <- list()
      fns <- lapply(1:nrow(inputs), function(i) {
        p(HTML(paste0(tags$strong(inputs$desc[i]), ": ", inputs[[paste0("example", reacts$examplechoice)]][i])))
      })
      do.call(box, list(title = paste0(el_name, " - ", gsub("[[:digit:]])", "", input$examples)), fns, collapsible = TRUE, collapsed = FALSE))
    })
    })
 }
    
  
  observeEvent(input$examples, {
    reacts$examplechoice <- switch(input$examples,
           "1) Contaminated food" = 1, "2) Polluted river" = 2, "3) Human exposure" = 3, "Additional Information" = 4
           )
  })
  
  output$sop_export <- downloadHandler(
    filename = function() {
      paste0("SPT_SOPFormat_", Sys.Date(), ".docx")
    },
    content = function(file) {
      input_table <- input_values[,c("element", "name", "desc")]
      element_names <- sapply(1:nrow(input_table), function(x) element_values$name[which(element_values$element == input_table$element[x])])
      input_data <- sapply(input_table$name, function(y) paste(input[[y]], collapse = ", "))
      dat <- data.frame(element = input_table$element, element_name = element_names, id = input_table$name, description = input_table$desc, input = input_data)
      rownames(dat) <- dat$id
      outputfile <- rmarkdown::render('template/sop_template.Rmd', 
                                      output_file = "test_output.docx", 
                                      params = list(dat = dat))
      file.copy(outputfile, file)
    }
  )
  
  output$plan_export <- downloadHandler(
    filename = function() {
      paste0("SPT_PlanFormat_", Sys.Date(), ".csv")
    },
    content = function(file) {
      input_table <- input_values[,c("element", "name", "desc")]
      element_names <- sapply(1:nrow(input_table), function(x) element_values$name[which(element_values$element == input_table$element[x])])
      input_data <- sapply(input_table$name, function(y) input[[y]])
      output_table <- cbind(element = input_table$element, element_name = element_names, id = input_table$name, description = input_table$desc, input = input_data)
      write.csv(output_table, file, row.names = FALSE)
    }
  )
  
  observeEvent(input$browser, {browser()})
  
  
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
