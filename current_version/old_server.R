# This is for making the functional version of it that isn't based on a dynamic UI


library(shiny)
library(shinydashboard)
library(shinyalert)
source('app_fn.R')
input_values <- read.csv('input_values.csv')
reacts <- reactiveValues(examplechoice = 1)

shinyServer(function(input, output) {
  
  #Warning message
  #shinyalert("Welcome!", "This is currently in development and should not be used for any reason other than that.", type = "info")
  
  output$element_1A <- renderUI({
    h3("Define the objective(s) of the NTA study, including the possible hypothesis or research question.")
    inputs <- input_values[which(input_values$element == "1A"),]
    fns <- list()
    for (i in 1:nrow(inputs)) {
      fn <- dynamic_input_fn(name = inputs$name[i], desc = inputs$desc[i], type = inputs$type[i], value = inputs$value[i], restrict = inputs$restrict[i], multiple = inputs$multiple[i])
      fns <- list(fns, fn)
      if (!is.na(inputs$tooltip[i])) {
        tt <- bsTooltip(inputs$name[i], title = inputs$tooltip[i], placement = "top")
        fns <- list(fns, tt)
      }
    }
    do.call(box, list(title = "Define study goals", fns, collapsible = TRUE, collapsed = TRUE))
  })
  
  output$example_1A <- renderUI({
    inputs <- input_values[which(input_values$element == "1A"),]
    fns <- lapply(1:nrow(inputs), function(i) {
      p(HTML(paste0(tags$strong(inputs$desc[i]), ": ", inputs[[paste0("example", reacts$examplechoice)]][i])))
    })
    do.call(box, list(title = paste0("Define study goals - ", gsub("[[:digit:]])", "", input$examples)), fns, collapsible = TRUE, collapsed = TRUE))
  })
  
  output$element_1B <- renderUI({
    h3("Define the scope of the study and potential limitations, including the domain of applicability of the study.")
    inputs <- input_values[which(input_values$element == "1B"),]
    fns <- list()
    for (i in 1:nrow(inputs)) {
      fn <- dynamic_input_fn(name = inputs$name[i], desc = inputs$desc[i], type = inputs$type[i], value = inputs$value[i], restrict = inputs$restrict[i], multiple = inputs$multiple[i])
      fns <- list(fns, fn)
      if (!is.na(inputs$tooltip[i])) {
        tt <- bsTooltip(inputs$name[i], title = inputs$tooltip[i], placement = "top")
        fns <- list(fns, tt)
      }
    }
    do.call(box, list(title = "Define study scope/limitations", fns, collapsible = TRUE, collapsed = TRUE))
  })
  
  output$example_1B <- renderUI({
    inputs <- input_values[which(input_values$element == "1B"),]
    fns <- lapply(1:nrow(inputs), function(i) {
      p(paste0(inputs$desc[i], ": ", inputs[[paste0("example", reacts$examplechoice)]][i]))
    })
    do.call(box, list(title = paste0("Define study scope/limitations - ", gsub("[[:digit:]])", "", input$examples)), fns, collapsible = TRUE, collapsed = TRUE))
  })
  
  observeEvent(input$examples, {
    reacts$examplechoice <- switch(input$examples,
           "1) Contaminated food" = 1, "2) Polluted river" = 2, "3) Human exposure" = 3
           )
  })
})
