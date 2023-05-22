library(shiny)
library(shinydashboard)
library(shinyalert)

dynamic_input_fn <- function(name, desc, type, value, restrict = NULL, width = "100%", multiple = FALSE) {
  returnfn <- NULL
  if (type == "text") {
    returnfn <- textInput(inputId = name, label = desc, value = value, width = width)
  }
  if (type == "textarea") {
    returnfn <- textAreaInput(inputId = name, label = desc, value = value, width = width)
  }
  if (type == "numeric") {
    if (!is.na(restrict)) {
      restricts <- as.numeric(unlist(strsplit(restrict, split = ":")))
      returnfn <- numericInput(inputId = name, label = desc, value = value, min = restricts[1], max = restricts[2], step = restricts[3], width = width)
    }
    if (is.na(restrict)) {
      returnfn <- numericInput(inputId = name, label = desc, value = value, width = width)
    }
  }
  if (type == "list") {
    returnfn <- selectizeInput(inputId = name, label = desc, choices = unlist(strsplit(value, split = ";")), width = width, multiple = multiple)
  }
  returnfn
}