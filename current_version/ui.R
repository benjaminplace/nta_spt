# This is for making the functional version of it that isn't based on a dynamic UI


library(shiny)
library(shinydashboard)
library(shinyalert)
library(shinyBS)
library(rmarkdown)

# Define header
header <- dashboardHeader(title = "NTA-SPT")

# Define sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(id = "tabs",
  menuItem("About", tabName = "about", selected = TRUE, icon = icon("question", lib = "font-awesome")),
  menuItem("Examples", tabName = "examples", icon = icon("id-card", lib = "font-awesome")),
  menuItem("Background and Objectives", tabName = "background", icon = icon("scroll", lib = "font-awesome")),
  menuItem("Sample Information", tabName = "samples", icon = icon("glass-water", lib = "font-awesome")),
  menuItem("Standards and Controls", tabName = "standards",icon = icon("flask", lib = "font-awesome")),
  menuItem("Data Acquisition", tabName = "method", icon = icon("microscope", lib = "font-awesome")),
  menuItem("Data Processing", tabName = "dataproc", icon = icon("computer", lib = "font-awesome")),
  menuItem("QA/QC", tabName = "qaqc", icon = icon("list-check", lib = "font-awesome")),
  menuItem("Export SMRT", tabName = "export", icon = icon("download", lib = "font-awesome")),
  selectInput("examples",label = "Load", choices = c("Additional Information", "1) Contaminated food", "2) Polluted river", "3) Human exposure"), selected = NULL),
  actionButton("user_guide", label = "Launch User Guide"),
  actionButton("browser", label = "Browser"),
  htmlOutput("progress")
  )
)

# Define body

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "about",
            p("This is where about information will go")
            ),
    tabItem(tabName = "export",
            #fluidRow(downloadButton(outputId = "sop_export", label = "Download data in a SOP format", icon = icon("file-download", verify_fa = FALSE))),
            #fluidRow(downloadButton(outputId = "plan_export", label = "Download data in a Study Plan format", icon = icon("file-download", verify_fa = FALSE))),
            fluidRow(downloadButton(outputId = "raw_export", label = "Download raw data as a JSON", icon = icon("file-download", verify_fa = FALSE)))
            ),
    tabItem(tabName = "examples",
            h2("Example Study Designs"),
            h4("We have provided 3 different examples for studies and provided details in each section about that study. You can populate the examples by selecting the drop-down menu on the left."),
            fluidRow(box(title = "Example 1 - Contaminated food",
                p("You are a scientist at a state regulatory lab (environmental/public health) that has experience in measuring contaminants in food and agricultural products via non-targeted analysis. You need to design a generalizable study to be ready for the following example: The local police department has a case of a single person getting sick from eating at a local farm-to-table restaurant, the only difference between the victim's meal and others was that they ate mashed potatoes with their meal. To discern the possibility that the mashed potatoes were the culprit, the police officers bring 10 g of sample in a sealed plastic bag to you to test for any possible chemical contaminants.")
                )), 
            fluidRow(box(title = "Example 2 - Polluted water",
                p("Evaluate potential contamination from a chemical manufacturing facility located on a river")
            )), 
            fluidRow(box(title = "Example 3 - Human exposure",
                p("A epidemiologist wants to conduct an exploratory analysis that helps identify unsuspected chemical exposures in a population with a particular disease.")
            ))
      ),
    tabItem(tabName = "background",
            fluidRow(uiOutput("element_1A"), uiOutput("example_1A"))
    ),
    tabItem(tabName = "samples",
            fluidRow(uiOutput("element_2A"), uiOutput("example_2A")),
            fluidRow(uiOutput("element_2B"), uiOutput("example_2B"))
    ),
    tabItem(tabName = "standards",
            fluidRow(uiOutput("element_3A"), uiOutput("example_3A"))
    ),
    tabItem(tabName = "method",
            fluidRow(uiOutput("element_4A"), uiOutput("example_4A")),
            fluidRow(uiOutput("element_4B"), uiOutput("example_4B")),
            fluidRow(uiOutput("element_4C"), uiOutput("example_4C"))
    ),
    tabItem(tabName = "dataproc",
            fluidRow(uiOutput("element_5A"), uiOutput("example_5A")),
            fluidRow(uiOutput("element_5B"), uiOutput("example_5B")),
            fluidRow(uiOutput("element_5C"), uiOutput("example_5C")),
            fluidRow(uiOutput("element_5D"), uiOutput("example_5D")),
            fluidRow(uiOutput("element_5E"), uiOutput("example_5E")),
            fluidRow(uiOutput("element_5F"), uiOutput("example_5F"))
    ),
    tabItem(tabName = "qaqc",
            fluidRow(uiOutput("element_6A"), uiOutput("example_6A")),
            fluidRow(uiOutput("element_6B"), uiOutput("example_6B"))
        )
)
)

# create UI


ui <- dashboardPage( header, sidebar, body, skin = "blue")