## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
### 0.2 Bibliotheken ####

## load base libraries, this needs to be done first, so other packages are loaded after and can
## override function calls
packages_base <- c(
  "base",
  "methods",
  "utils",
  "stats",
  "graphics",
  "grDevices",
  "datasets")

purrr::walk(packages_base, ~library(., character.only = TRUE))

## load shiny libraries
library(shiny)
library(waiter)
library(shinythemes)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinyalert)
library(shinyjs)

## tables
library(DT)
## TODO: Moet dit nog?
library(knitr)
library(kableExtra)

## shiny tools: plots and databases
library(ggplot2)
library(plotly)
library(scales)

## SQL
library(DBI)
library(odbc)
library(Rcpp)

## config
##' *INFO* Config should never be loaded with library: https://github.com/rstudio/config/issues/4
#library(config)

## Color pallete
library(RColorBrewer)

## Spinner
library(shinycssloaders)

## Popover/tooltip
library(spsComps)

## Esquisse
library(esquisse)

## Themes (shiny and ggplot)
library(ggpubr)
library(thematic)
library(fresh)

## ggplot extensions
library(ggalluvial)

## load tidyverse and data prep packages
library(janitor)
library(readr)
library(stringdist)
library(stringr)
library(tidyr)
library(purrr)
library(dplyr)

## Aanvullende packages gebruikt in PFS
# library(shinyjs)
# library(hrbrthemes)
# library(ggthemes)
# library(ggtext)
# library(glue)
# library(htmlTable)
# library(formattable)
# library(waffle)
# library(grid)
# library(gridExtra)
# library(shinyBS)

rm(packages_base)
