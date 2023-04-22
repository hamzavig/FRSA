library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(devtools)
library(gbRd)
library(tools)
library(readr)
library(shinyFeedback)
library(shinyjs)
library(dplyr)
library(lubridate)
library(timeDate)
library("FRSA")


css <- "
.nowrap {
  white-space: nowrap;
}

#tableDoc.shiny-html-output.shiny-bound-output .container {
  margin-left: 0px !important;
  padding-left: 0px !important;
}

div[data-value='Market'] {
  margin-top: 20px !important;
}

div[data-value='Institution'] {
  margin-top: 20px !important;
}

.shiny-file-input-progress {
  display: none
}

#rf_import {
  margin-bottom: 30px !important;
}

#ct_import {
  margin-bottom: 30px !important;
}

#ycPlot {
  margin-top: 30px !important;
}

#dcPlot {
  margin-top: 30px !important;
}

#inst_add_col {
  padding-left: 0px !important;
}

"
