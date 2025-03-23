packages <-  c(
  "tidyverse", 
  "ggplot2",
  "gridExtra",
  "forcats",
  "broom", 
  "fixest", 
  "fastDummies",
  "plm",
  "modelsummary", 
  "table1",
  "gt")

package.check <- lapply(
  packages,
  FUN <-  function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

# Path
output_path <- "research_paper/output"
