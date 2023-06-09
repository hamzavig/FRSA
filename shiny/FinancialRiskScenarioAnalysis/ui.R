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
              uiOutput("rf_file_notification"),
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
                column(3, textInput("rf_tenor1", label = "Tenor 1", value = '1Y')),
                column(3, textInput("rf_tenor2", label = "Tenor 2", value = NULL)),
                column(3, textInput("rf_tenor3", label = "Tenor 3", value = NULL)),
                column(3, textInput("rf_tenor4", label = "Tenor 4", value = NULL))
              ),
              fluidRow(
                id = "rf_rates",
                column(3, numericInput("rf_rate1", label = "Rate 1", value = 0.01, step = 0.01, min = 0.01, max = 1.0)),
                column(3, numericInput("rf_rate2", label = "Rate 2", value = NULL, step = 0.01, min = 0.01, max = 1.0)),
                column(3, numericInput("rf_rate3", label = "Rate 3", value = NULL, step = 0.01, min = 0.01, max = 1.0)),
                column(3, numericInput("rf_rate4", label = "Rate 4", value = NULL, step = 0.01, min = 0.01, max = 1.0))
              ),
              uiOutput("rf_single_notification"),
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
        tabPanel("Institutions",
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
                width = 7,
                selectInput("inst_view", NULL, choices = NULL, width = '100%')
              ),
              column(
                width = 1,
                actionButton("inst_delete", "Delete", width = "100%")
              ),
              column(
                width = 1,
                actionButton("inst_clone", "Clone", width = "100%")
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
                uiOutput("ct_file_notification"),
                p(
                  "There are predefined Financial Contract datasets in the ",
                  strong("downloads"),
                  " section. You can upload them as they are or extend the downloaded files with your own contracts."
                ),
                actionButton("ct_import", "Import Financial Contracts", width = "100%"),
                p("- or -", style = "text-align: center;"),
                br(),
                selectInput("node", "Account", choices = NULL),
                selectInput("contractType", "Contract Type",
                  choices = c(
                    "ANN",
                    "PAM",
                    "Investments",
                    "OperationalCF"
                  )
                ),
                conditionalPanel(
                  condition = 'input.contractType == "ANN" || input.ct_type == "PAM"',
                  fluidRow(
                    column(
                      width = 6,
                      selectInput("contractRole", "Contract Role",
                        choices = c(
                          "RPA",
                          "RPL"
                        )
                      ),
                      selectInput("dayCountConvention", "Day Count Convention",
                        choices = c(
                          "30E360",
                          "B252",
                          "A360",
                          "A365"
                        )
                      ),
                      numericInput("notionalPrincipal", "Nominal Principal", value = 1000, step = 100, min = 100),
                      numericInput("nominalInterestRate", "Nominal Interest Rate", value = 0.01, step = 0.001, min = 0.001, max = 1.0)
                    ),
                    column(
                      width = 6,
                      textInput("contractID", "Contract Identifier", placeholder = "CTR0001"),
                      dateInput("initialExchangeDate", "Initial Exchange Date", value = Sys.Date(), format = "yyyy-mm-dd"),
                      dateInput("maturityDate", "Maturity Date", value = "", format = "yyyy-mm-dd"),
                      selectInput("currency", "Currency",
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
                      actionButton("calendarToggle", "Calendar", width = "100%"),
                    )
                  ),
                  conditionalPanel(
                    condition = "input.calendarToggle % 2 == 1",
                    toggle = TRUE,
                    fluidRow(
                      column(
                        width = 12,
                        br(),
                        selectInput("calendar", "Calendar", choices = c('NOCALENDAR')),
                        selectInput("businessDayConvention", "Business Day Convention", choices = NULL),
                        selectInput("endOfMonthConvention", "End Of Month Convention", choices = NULL)
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      width = 12,
                      br(),
                      actionButton("contractIdentificationToggle", "Contract Identification", width = "100%"),
                    )
                  ),
                  conditionalPanel(
                    condition = "input.contractIdentificationToggle % 2 == 1",
                    toggle = TRUE,
                    fluidRow(
                      column(
                        width = 12,
                        br(),
                        dateInput("statusDate", "Status Date", value = "", format = "yyyy-mm-dd"),
                        textInput("legalEntityIDRecordCreator", "Legal Entity ID Record Creator"),
                        textInput("legalEntityIDCounterparty", "Legal Entity ID Counterparty"),
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      width = 12,
                      br(),
                      actionButton("interestToggle", "Interest", width = "100%"),
                    )
                  ),
                  conditionalPanel(
                    condition = "input.interestToggle % 2 == 1",
                    toggle = TRUE,
                    fluidRow(
                      column(
                        width = 12,
                        br(),
                        dateInput("cycleAnchorDateOfInterestPayment", "Cycle Anchor Date of Interest Payment", value = "", format = "yyyy-mm-dd"),
                        selectInput("cycleOfInterestPayment", "Cycle of Interest Payment", choices = c('P1YL0', 'P2YL0', 'P6ML0', 'P3ML0', 'P1ML0', 'None')),
                        selectInput("cyclePointOfInterestPayment", "Cycle Point Of Interest Payment", choices = c('E', 'B')),
                        numericInput("accruedInterest", "Accrued Interest", value = NULL, min = 0),
                        
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      width = 12,
                      br(),
                      actionButton("notionalPrincipalToggle", "NotionalPrincipal", width = "100%"),
                    )
                  ),
                  conditionalPanel(
                    condition = "input.notionalPrincipalToggle % 2 == 1",
                    toggle = TRUE,
                    fluidRow(
                      column(
                        width = 12,
                        br(),
                        dateInput("amortizationDate", "Amortization Date", value = "", format = "yyyy-mm-dd"),
                        dateInput("contractDealDate", "Contract Deal Date", value = Sys.Date(), format = "yyyy-mm-dd"),
                        dateInput("terminationDate", "Termination Date", value = "", format = "yyyy-mm-dd"),
                        numericInput("premiumDiscountAtIED", "Premium Discount At IED", value = 0, min = 0),
                        dateInput("cycleAnchorDateOfPrincipalRedemption", "Cycle Anchor Date Of Principal Redemption", value = "", format = "yyyy-mm-dd"),
                        selectInput("cycleOfPrincipalRedemption", "Cycle Of Principal Redemption", choices = c('P1YL0', 'P2YL0', 'P6ML0', 'P3ML0', 'P1ML0', 'None'), selected = NULL, multiple = FALSE),
                        numericInput("nextPrincipalRedemptionPayment", "Next Principal Redemption Payment", value = NULL, min = 0)
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      width = 12,
                      br(),
                      actionButton("rateResetToggle", "Rate Reset", width = "100%"),
                    )
                  ),
                  conditionalPanel(
                    condition = "input.rateResetToggle % 2 == 1",
                    toggle = TRUE,
                    fluidRow(
                      column(
                        width = 12,
                        br(),
                        dateInput("cycleAnchorDateOfRateReset", "Cycle Anchor Date of Rate Reset", value = "", format = "yyyy-mm-dd"),
                        selectInput("cycleOfRateReset", "Cycle of Rate Reset", choices = c('None', 'P1YL0', 'P2YL0', 'P6ML0', 'P3ML0', 'P1ML0'), selected = NULL, multiple = FALSE),
                        selectInput("cyclePointOfRateReset", "Cycle Point Of Rate Reset", choices = c('B', 'E')),
                        numericInput("rateSpread", "Rate Spread", value = 0, step = 0.001, min = 0),
                        numericInput("rateMultiplier", "Rate Multiplier", value = 1, step = 0.001, min = 1),
                        selectInput("marketObjectCodeOfRateReset", "Market Object Code of Rate Reset",
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
                uiOutput("ct_single_notification"),
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
                    "Financial Contracts",
                    br(),
                    fluidRow(
                      column(
                        width = 12,
                        selectInput("fc_view", NULL, choices = NULL, width = "100%"),
                        uiOutput("fc_ui"),
                        br()
                      )
                    )
                  ),
                  tabPanel(
                    "Market Objects",
                    br(),
                    fluidRow(
                      column(
                        width = 12,
                        p('This section captures the effective market object per financial contract.'),
                        DTOutput("inst_market_df", width = '100%')
                      )
                    )
                  ),
                  tabPanel(
                    "Log",
                    br(),
                    fluidRow(
                      column(
                        width = 12,
                        p('This section captures only the logs of the last initiated financial contract import whether it was a file upload or a single financial contract import.'),
                        DTOutput("error_log_df", width = '100%')
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        tabPanel("Risk Analysis",
                 id = "riskAnalysis",
                 sidebarLayout(
                   sidebarPanel(
                     width = 3,
                     selectInput("ra_inst", "Select Institution", choices = NULL),
                     selectInput("ra_scenario", "Risk Scenario", choices = c("Interest Rate Risk", "Default Risk")),
                     conditionalPanel(
                       condition = "input.ra_scenario == 'Interest Rate Risk'",
                       selectInput("ra_sub_scenario", "Interest Rate Risk Scenario", choices = c('Parallel Shift')),
                       selectInput("ra_mocs", "Market Objects", choices = NULL, multiple = TRUE),
                       p("Please add new Market Object in tab 'Market' if you wish to proceed with different Market Object."),
                       fluidRow(
                         id = "ra_irr_shift_amounts",
                         column(3, numericInput("ra_irr_shift_amount1", label = "Shift 1", value = 0.01, step = 0.001, min = 0.001, max = 1.0)),
                         column(3, numericInput("ra_irr_shift_amount2", label = "Shift 2", value = NULL, step = 0.001, min = 0.001, max = 1.0)),
                         column(3, numericInput("ra_irr_shift_amount3", label = "Shift 3", value = NULL, step = 0.001, min = 0.001, max = 1.0)),
                         column(3, numericInput("ra_irr_shift_amount4", label = "Shift 4", value = NULL, step = 0.001, min = 0.001, max = 1.0))
                       )
                     ),
                     conditionalPanel(
                       condition = "input.ra_scenario == 'Default Risk'",
                       selectInput("ra_dr_sub_scenario", "Default Risk Scenario", choices = c('Probability')),
                       selectInput("ra_dr_mocs", "Market Objects", choices = NULL, multiple = TRUE),
                       p("Please add a new Market Object in tab 'Market' if you wish to proceed with different Risk Factor Object for the simulation."),
                       fluidRow(
                         id = "ra_dr_recovery_rates",
                         column(3, numericInput("ra_dr_recovery_rate1", label = "Recovery 1", value = 0.01, step = 0.001, min = 0.001, max = 1.0)),
                         column(3, numericInput("ra_dr_recovery_rate2", label = "Recovery 2", value = NULL, step = 0.001, min = 0.001, max = 1.0)),
                         column(3, numericInput("ra_dr_recovery_rate3", label = "Recovery 3", value = NULL, step = 0.001, min = 0.001, max = 1.0)),
                         column(3, numericInput("ra_dr_recovery_rate4", label = "Recovery 4", value = NULL, step = 0.001, min = 0.001, max = 1.0))
                       ),
                       dateInput("ra_dr_from", "Default From", value = as.Date(paste0(format(Sys.Date() + years(1), "%Y"), "-01-01")), format = "yyyy-mm-dd")
                     ),
                     selectInput("ra_value_view", "Value View", choices = c("nominal", "market")),
                     conditionalPanel(
                       condition = "input.ra_value_view == 'market'",
                       selectInput("ra_discount_engine", "Discounting Object", choices = NULL),
                       p("Please add a new Discounting Object in tab 'Market' if you wish to proceed with different Discounting Object.")
                     ),
                     selectInput("ra_income_view", "Income View", choices = c("marginal", "cumulative")),
                     selectInput("ra_scale", "Scale", choices = c("in millions", "in thousands", "no scale")),
                     dateInput("ra_from", "From", value = as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01")), format = "yyyy-mm-dd"),
                     dateInput("ra_to", "To", value = as.Date(paste0(format(Sys.Date() + years(1), "%Y"), "-01-01")), format = "yyyy-mm-dd"),
                     br(),
                     actionButton("ra_start", "Start Risk Analysis", width = "100%")
                   ),
                   mainPanel(
                     width = 9,
                     fluidRow(
                       column(
                         width = 11,
                         selectInput("ra_view", NULL, choices = NULL, width = '100%')
                       ),
                       column(
                         width = 1,
                         actionButton("ra_delete", "Delete", icon = icon("minus"), width = "100%")
                       )
                     ),
                     fluidRow(
                       column(
                         width = 12,
                         actionButton("ra_detailsToggle", "Details", width = "100%", style = "margin-bottom: 20px !important;")
                       )
                     ),
                     conditionalPanel(
                       condition = "input.ra_detailsToggle % 2 == 1",
                       toggle = TRUE,
                       div(
                         fluidRow(
                           column(6,
                                  verbatimTextOutput("ra_inst_output"),
                                  verbatimTextOutput("ra_scenario_output"),
                                  verbatimTextOutput("ra_sub_scenario_output"),
                                  verbatimTextOutput("ra_from_output"),
                                  verbatimTextOutput("ra_to_output")
                           ),
                           column(6,
                                  verbatimTextOutput("ra_mocs_output"),
                                  verbatimTextOutput("ra_rates_output"),
                                  verbatimTextOutput("ra_value_view_output"),
                                  verbatimTextOutput("ra_income_view_output"),
                                  verbatimTextOutput("ra_scale_output")
                           )
                         ),
                         style = "margin-bottom: 20px; padding: 19px; border: 1px solid #e5e5e5; border-radius:4px;"
                       )
                     ),
                     verbatimTextOutput("debug"),
                     uiOutput("ra_uiOutput")
                   )
                 )
        )
      )
    ),
    tabPanel("Downloads",
      id = "downloads",
      fluidRow(
        column(
          width = 3,
          selectInput("dataset",
            "Choose a dataset:",
            choices = c("yieldCurves", "defaultCurves", "annuities", "principalAtMaturities", "operations")
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
    )
  ) # close navbarPage
) # close fluidPage
