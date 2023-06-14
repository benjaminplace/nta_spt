

#Import required packages
packs <- c("shiny", "shinydashboard", "shinyalert", "shinyBS", "knitr", "rmarkdown", "DT", "jsonlite")

installed_packages <- installed.packages()

packs_TRUE  <- which(packs %in% installed_packages)
packs_FALSE <- packs[-packs_TRUE]
if (length(packs_FALSE) > 0) {
  install.packages(pkgs         = packs_FALSE,
                   quiet        = FALSE,
                   dependencies = TRUE)
}
lapply(packs, library, character.only = TRUE, quietly = TRUE)
rm(packs, packs_TRUE, packs_FALSE)