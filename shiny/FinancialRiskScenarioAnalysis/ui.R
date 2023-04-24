#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#

fluidPage(
  theme = shinytheme("readable"),
  img(
    src = "actus-logo.png", height = 100, width = 220,
    style = "float:right; padding-right:25px"
  ),
  img(src = "soe.png", height = 138, width = 239),
  
  # Link custom CSS styling
  tags$head(
    tags$style(HTML(css))
  ),

  navbarPage("Financial Risk Scenario Analysis",
    fluid = TRUE,
    tabPanel("Scenario Analysis",
      id = "scenarioAnalysis",
      tabsetPanel(
        tabPanel("Market",
          id = "market",
          sidebarLayout(
            sidebarPanel(
              width = 3,
              fileInput("rf_file", "Upload Risk Factors",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv"
                ),
                placeholder = "No file selected."
              ),
              p(
                "There are predefined Yield Curves and Default Curves datasets in the ",
                strong("downloads"),
                " section. You can upload them as they are or extend the downloaded files with your own riskfactors."
              ),
              actionButton("rf_import", "Import Risk Factor", width = "100%"),
              p("- or -"),
              br(),
              selectInput("rf_type", "Risk Factor Type",
                choices = c(
                  "YieldCurve",
                  "DefaultCurve"
                )
              ),
              textInput("rf_label", "Label", placeholder = "YC_CH_AAA"),
              dateInput("rf_ref_date", "Reference Date", value = Sys.Date(), format = "yyyy-mm-dd"),
              fluidRow(
                id = "rf_tenors",
                column(3, textInput("rf_tenor1", label = "Tenor 1", placeholder = "1Y")),
                column(3, textInput("rf_tenor2", label = "Tenor 2", placeholder = "2Y")),
                column(3, textInput("rf_tenor3", label = "Tenor 3", placeholder = "5Y")),
                column(3, textInput("rf_tenor4", label = "Tenor 4", placeholder = "10Y"))
              ),
              fluidRow(
                id = "rf_rates",
                column(3, numericInput("rf_rate1", label = "Rate 1", value = 0.01, step = 0.01, min = 0.01, max = 1.0)),
                column(3, numericInput("rf_rate2", label = "Rate 2", value = 0.01, step = 0.01, min = 0.01, max = 1.0)),
                column(3, numericInput("rf_rate3", label = "Rate 3", value = 0.01, step = 0.01, min = 0.01, max = 1.0)),
                column(3, numericInput("rf_rate4", label = "Rate 4", value = 0.01, step = 0.01, min = 0.01, max = 1.0))
              ),
              br(),
              actionButton("rf_add", "Add Risk Factor", width = "100%")
            ),
            mainPanel(
              width = 9,
              tabsetPanel(
                tabPanel(
                  "Yield Curves",
                  br(),
                  DTOutput("yieldCurve_df"),
                  br(),
                  actionButton("yc_duplicate", "Duplicate"),
                  actionButton("yc_remove", "Remove"),
                  div(downloadButton("downloadYC", "Download"), style = "float:right"),
                  br(),
                  plotOutput("ycPlot"),
                  verbatimTextOutput("ycD")
                ),
                tabPanel(
                  "Default Curves",
                  br(),
                  DTOutput("defaultCurve_df"),
                  br(),
                  actionButton("dc_duplicate", "Duplicate"),
                  actionButton("dc_remove", "Remove"),
                  div(downloadButton("downloadDC", "Download"), style = "float:right"),
                  br(),
                  plotOutput("dcPlot")
                )
              )
            )
          )
        ),
        tabPanel("Institution",
          id = "institution",
          
          fluidRow(
            id = 'inst_creation_div',
            column(
              width = 2,
              textInput("inst_name", NULL, placeholder = "Add institution name...", width = "100%"),
              uiOutput('inst_warning')
            ),
            column(
              width = 1,
              actionButton("inst_add", "Create", icon = icon("plus"), width = "100%"),
            ),
            conditionalPanel(
              condition = 'output.inst_panel == true',
              column(
                width = 9,
                selectInput(inputId = "inst_view", NULL, choices = NULL, width = '100%')
              )
            )
          ),
          conditionalPanel(
            condition = 'output.inst_panel == true',
            sidebarLayout(
              sidebarPanel(
                width = 3,
                selectInput("ct_ptf_type", "Portfolio Type", choices = c("Annuities", "PrincipalAtMaturities", "Operations")),
                fileInput("ct_file", "Upload Financial Contracts",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv"
                  ),
                  placeholder = "No file selected."
                ),
                p(
                  "There are predefined Financial Contract datasets in the ",
                  strong("downloads"),
                  " section. You can upload them as they are or extend the downloaded files with your own contracts."
                ),
                actionButton("ct_import", "Import Financial Contracts", width = "100%"),
                p("- or -", style = "text-align: center;"),
                br(),
                selectInput("inst_account_type", "Account Type", choices = NULL),
                selectInput("inst_account", "Account", choices = NULL),
                selectInput("ct_type", "Contract Type",
                  choices = c(
                    "ANN",
                    "PAM",
                    "Investments",
                    "OperationalCF"
                  )
                ),
                conditionalPanel(
                  condition = 'input.ct_type == "ANN" || input.ct_type == "PAM"',
                  fluidRow(
                    column(
                      width = 6,
                      selectInput("ct_role", "Contract Role",
                        choices = c(
                          "RPA",
                          "RPL"
                        )
                      ),
                      selectInput("ct_dcc", "Day Count Convention",
                        choices = c(
                          "30E60",
                          "B252",
                          "A360",
                          "A365"
                        )
                      ),
                      numericInput("ct_nom_prc", "Nominal Principal", value = 1000, step = 100, min = 100),
                      numericInput("ct_nom_rate", "Nominal Interest Rate", value = 0.01, step = 0.001, min = 0.001, max = 1.0)
                    ),
                    column(
                      width = 6,
                      textInput("ct_id", "Contract Identifier", placeholder = "CTR0001"),
                      dateInput("ct_ied", "Initial Exchange Date", value = Sys.Date(), format = "yyyy-mm-dd"),
                      dateInput("ct_mtdt", "Maturity Date", value = Sys.Date(), format = "yyyy-mm-dd"),
                      selectInput("ct_currency", "Currency",
                        choices = c(
                          "CHF",
                          "USD",
                          "EUR"
                        )
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      width = 12,
                      br(),
                      actionButton("rateResetToggle", "Rate Reset", icon = icon("plus"), width = "100%"),
                    )
                  ),
                  conditionalPanel(
                    condition = "input.rateResetToggle % 2 == 1",
                    toggle = TRUE,
                    fluidRow(
                      column(
                        width = 12,
                        br(),
                        selectInput("ct_moc", "Market Object Code of Rate Reset",
                          choices = NULL
                        )
                      )
                    )
                  )
                ),
                conditionalPanel(
                  condition = 'input.ct_type == "Investments" || input.ct_type == "OperationalCF"',
                  p("Investments / OpCF fields")
                ),
                br(),
                actionButton("ct_add", "Add Financial Contract", width = "100%")
              ),
              mainPanel(
                width = 9,
                tabsetPanel(
                  id = "institutions",
                  tabPanel(
                    "Structure",
                    br(),
                    fluidRow(
                      column(
                        width = 6,
                        verbatimTextOutput('inst_structure')
                      ),
                      column(
                        width = 6,
                        p('In this section, you have the ability to modify the existing structure of the institution by adding new account types and/or accounts, renaming or removing them as needed.',
                          style = "text-align: justify;"),
                        p('Note that any changes made to the structure will affect the accounts and balances associated with the institution, so be careful when making changes and ensure that the structure accurately reflects the financial hierarchy of the institution.', 
                          style = "text-align: justify;"),
                        fluidRow(
                          column(
                            width = 4,
                            selectInput('str_node', 'Node', choices = NULL),
                            uiOutput('str_notification')
                          ),
                          column(
                            width = 4,
                            uiOutput('str_node_options_1_1'),
                            uiOutput('str_node_options_1_2')
                          ),
                          column(
                            width = 4,
                            uiOutput('str_node_options_2_1'),
                            uiOutput('str_node_options_2_2')
                          )
                        )
                      )
                    )
                  ),
                  tabPanel(
                    "Assets"
                  ),
                  tabPanel(
                    "Liabilities"
                  ),
                  tabPanel(
                    "Market"
                  )
                )
              )
            )
          )
        ),
        tabPanel("Risk Analysis")
      )
    ),
    tabPanel("Downloads",
      id = "downloads",
      fluidRow(
        column(
          width = 3,
          selectInput("dataset",
            "Choose a dataset:",
            choices = c("annuities", "yieldCurves")
          ),
          downloadButton("downloadData", "Download")
        ),
        column(
          width = 9,
          tabsetPanel(
            tabPanel(
              "Data",
              br(),
              DTOutput("table")
            ),
            tabPanel(
              "Description",
              br(),
              htmlOutput("tableDoc")
            )
          )
        )
      )
    ),
    tabPanel("Instructions")
  ) # close navbarPage
) # close fluidPage
