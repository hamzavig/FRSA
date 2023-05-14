library(shiny)
library(shinythemes)
library(shinycssloaders)
library(DT)
library(devtools)
library(gbRd)
library(tools)
library(readr)
library(dplyr)
library(lubridate)
library(timeDate)
library(data.tree)
library(utils)
library(glue)
library(tidyverse)
library("FRSA")

# Define object styling using custom CSS
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

div[data-value='Risk Analysis'] {
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

.selectize-input {
  height: 40px !important;
}

#inst_creation_div {
  margin-top: 20px !important;
}

#inst_add {
  height: 40px !important;
}

#inst_delete {
  height: 40px !important;
}

#str_remove_node {
  margin-top: 28px !important;
}

#str_confirm_remove_node {
  margin-top: 28px !important;
}

#str_cancel_remove_node {
  margin-top: 28px !important;
}

#str_rename_node {
  margin-top: 28px !important;
}

#str_rename_node_2 {
  margin-top: 28px !important;
}

#str_cancel_rename_node {
  margin-top: 28px !important;
}
"
