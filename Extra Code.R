Extra Code

required_packages <- c("shiny",
                       "ggvis",
                       "data.table", 
                       "ggplot2",
                       "tidyverse",
                       "shinydashboard"
)
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(required_packages, require, character.only = TRUE)
