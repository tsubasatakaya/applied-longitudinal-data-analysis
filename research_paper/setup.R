packages <-  c(
  "tidyverse", 
  "ggplot2",
  "haven",
  "broom", 
  "fixest", 
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
