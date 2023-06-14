library(shiny)
library(shinydashboard)
library(shinyalert)
library(DT)

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
  if (type == "list create") {
    returnfn <- selectizeInput(inputId = name, 
                               label = desc, 
                               choices = unlist(strsplit(value, split = ";")), 
                               width = width, 
                               multiple = multiple,
                               options = list(create = TRUE))
  }
  if (type == "list matrix") {
    val <- unlist(strsplit(value, split = "!"))
    val <- val[1]
    returnfn <- list(
      selectizeInput(inputId = name,
                     label = desc,
                     choices = unlist(strsplit(val, split = ";")), 
                     width = width, 
                     multiple = multiple,
                     options = list(create = TRUE)),
      DT::DTOutput(outputId = paste0("table_",name))
    )
  }
  if (type == "modal table") {
    returnfn <- list(
      p(desc),
      actionButton(paste0("add_", name), "Add Value"),
      actionButton(paste0("remove_", name), "Remove Value"),
      DT::DTOutput(outputId = paste0("table_", name)),
      br()
    )
  }
  returnfn
}

dynamic_modal <- function(title, name, inputs, helps, ok = "Add value", size = "xl") {
  inputs <- unlist(strsplit(inputs, split = ";"))
  helps <- unlist(strsplit(helps, split = ";"))
  modalDialog(
    title = title,
    size = size,
    easyClose = TRUE,
    lapply(1:length(inputs), function(x)
    fluidRow(
      textInput(inputId = paste0("modal_", name, inputs[x]), inputs[x]), p(helps[x])
    )),
    footer = tagList(actionButton(paste0("modal_ok_", name), ok), modalButton("Cancel"))
  )
}