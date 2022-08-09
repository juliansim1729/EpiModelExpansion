library(shiny)
library(EpiModel)

shinyUI(
  navbarPage(title = NULL, windowTitle = "EpiModel: Network Models",
             tabPanel("About",
                      
                      column(6, offset = 1,
                             h2("Stochastic Network Models with EpiModel",
                                style = "color: #445555;"),
                             p(a("EpiModel", href = "http://www.epimodel.org/",
                                 target = "_blank"),
                               "is an R package that provides tools for simulating and
             analyzing mathematical models of infectious disease.
             Details about the package, including the epidemic model classes
             supported by the software can be found at the link above."),
                             p("This web-based application allows for simple modeling of epidemics
              over dynamic contact networks. These stochastic network models are
             based on the statistical framework of",
                               a("temporal exponential random graph models.",
                                 href = "http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3891677/",
                                 target = "_blank"), "This web application is built with",
                               a("Shiny,", href = "https://shiny.rstudio.com/", target = "_blank"),
                               "and may be lauched via an R session with EpiModel and Shiny
             installed (see the", code("epiweb"), "function), or directly on any
             web browser (no R needed)",
                               a("here.", href = "https://statnet.shinyapps.io/epinet",
                                 target = "_blank")),
                             p("To get started, create a statistical network model in the Model
            Estimation page using one of the two model specification methods.
            This page fits a temporal ERGM using the", code("netest"),
                               "function and runs diagnostics on the fitted model with the",
                               code("netdx"), "function. After the model is properly specified,
            simulate an epidemic on the network using the Epidemic Simulation
            page. This runs the", code("netsim"), "function in EpiModel, and the
            epidemic parameters are described in detail in the help pages there.
            Model output may be plotted to show the epidemic time series or
            static network plots, as well as viewing numerical data
            summaries."),
                             p("The author of this application is Emily Beylerian, Software
            Developer at the University of Washington Centers for Studies in
            Demoraphy and Ecology. The authors of the larger EpiModel project
            are Samuel Jenness at Emory University, and Steven Goodreau and
            Martina Morris at the University of Washington. Development of this
            software is supported by the following grants from the National
            Institutes of Health: R01HD68395 (NICHD), T32HD007543 (NICHD), and
             R24HD042828 (NICHD).")
                      )
             ),
             tabPanel("Stateflow Builder",
                      
                      tags$head(
                        tags$style(HTML("hr {border-top: 1px solid #000000;}"))
                      ),
                      
                      # Application title
                      titlePanel("EpiModel Stateflow Builder"),
                      
                      # Sidebar (collapse possible?)
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("numStates", label = "Number of States", value = 3, min = 2, max = 10, step = 1),
                          textInput("stateNames", label = "State Names", value = "S, I, R", placeholder = "Separate Entries By Comma"),
                          hr(),
                          
                          p(HTML("<b>Non-Temporal State Flows</b>"), style = "text-align:center"),
                          
                          # TODO: need to update this to work with non-letter states
                          textInput("iFlow", label = "State Flow Through Infection", value = "SII", placeholder = "No Infectious Flow"),
                          helpText(HTML("This text input requires entries to be made up of three states (letters) in the following order.
        <b>S</B>usceptible Nodes become <b>I</b>nfected Nodes when infected by <b>I</b>nfectious Nodes. If multiple state
        flows occur through infection, then they should be separated by commas.")),
                          textInput("iFlowNum", label = "Infection Flow Rate", value = 0.1, placeholder = "A probability for the 
                                    infection to spread from an \"infected\" node to a \"noninfected\" node."),
                          hr(),
                          
                          p(HTML("<b>Additional Nodal Attributes</b>"), style = "text-align:center"),
                          checkboxInput("attr_imm", "Immunity", value = FALSE),
                          conditionalPanel("input.attr_imm == 1",
                                           selectInput("attr_imm_baseline", "Baseline Immunity",
                                                       c(flat = 'Flat', pois = 'Poisson distributed')),
                                           sliderInput("attr_imm_baseval", "Value/Mean for Baseline Immunity",
                                                       value = 0.5, min = 0, max = 10, step = 0.1),
                                           # TODO: this following should really be a slow gain while in an "infected" state
                                           sliderInput("attr_imm_gain", "Immunity Gain On Recovery (simplified)",
                                                       value = 2, min = 0, max = 10, step = 0.1),
                                           sliderInput("attr_imm_decay", "Immunity Decay Over Time (at each time step)",
                                                       value = 0.05, min = 0, max = 1, step = 0.01)
                          ),
                          checkboxInput("attr_age", "Age", value = FALSE),
                          conditionalPanel("input.attr_age == 1",
                                           selectInput("attr_age_dist", "Age Distribution",
                                                       c('Realistic (US)',"Random", "Custom")),
                                           conditionalPanel("input.attr_age_dist == 'Custom'",
                                                            textInput("attr_age_5", "Proportion of Population < 5", value = 0.057),
                                                            textInput("attr_age_15", "Proportion of Population < 15", value = 0.125),
                                                            textInput("attr_age_24", "Proportion of Population 15-24", value = 0.130),
                                                            textInput("attr_age_34", "Proportion of Population 25-34", value = 0.137),
                                                            textInput("attr_age_44", "Proportion of Population 35-44", value = 0.131),
                                                            textInput("attr_age_54", "Proportion of Population 45-54", value = 0.123),
                                                            textInput("attr_age_64", "Proportion of Population 55-64", value = 0.129),
                                                            textInput("attr_age_74", "Proportion of Population 65-74", value = 0.101),
                                                            textInput("attr_age_up", "Proportion of Population 75+", value = 0.067)        
                                           )
                          ),
                          hr(),
                          
                          p(HTML("<b> Additional Model Processes </b>"), style = "text-align:center"),
                          checkboxInput("proc_birth", "Birth", value = FALSE),
                          conditionalPanel("input.proc_birth == 1",
                                           selectInput("proc_birth_type", "Type of Birth", c("Exponential Birth", "Linear Birth")),
                                           sliderInput("proc_birth_rate", "Birth Rate", value = 0.05, min = 0, max = 1, step = 0.01)),
                          checkboxInput("proc_death", "Death", value = FALSE),
                          conditionalPanel("input.proc_death == 1",
                                           checkboxInput("proc_death_infAdj", "Adjust Death Rates for State Status", value = TRUE),
                                           selectInput("proc_death_dist", "Death Rate Distribution",
                                                       c('Realistic (US)',"Flat", "Custom")),
                                           # TODO: FIND VALUES
                                           # conditionalPanel("input.attr_age_dist == 'Custom'",
                                           #                  textInput("attr_age_5", "Proportion of Population < 5", value = 0.057),
                                           #                  textInput("attr_age_15", "Proportion of Population < 15", value = 0.125),
                                           #                  textInput("attr_age_24", "Proportion of Population 15-24", value = 0.130),
                                           #                  textInput("attr_age_34", "Proportion of Population 25-34", value = 0.137),
                                           #                  textInput("attr_age_44", "Proportion of Population 35-44", value = 0.131),
                                           #                  textInput("attr_age_54", "Proportion of Population 45-54", value = 0.123),
                                           #                  textInput("attr_age_64", "Proportion of Population 55-64", value = 0.129),
                                           #                  textInput("attr_age_74", "Proportion of Population 65-74", value = 0.101),
                                           #                  textInput("attr_age_up", "Proportion of Population 75+", value = 0.067)        
                                           # )
                                           
                          )),
                        
                        # Show plot
                        mainPanel(
                          actionButton("preset_cov19", "COVID-19 Preset",
                                       style = "margin-bottom: 10px;"),
                          actionButton("preset_ebola", "Ebola Preset",
                                       style = "margin-bottom: 10px;"),
                          
                          p(HTML("This editable table represents probabilities at which states flow into each other at each time step. For
               temporal state flows, the probability of a node going from one state to another depends <b>only</b> on
               time. However, for other state flows, such as infectivity, this can depend on external factors such
               as the status of connected nodes.")),
                          DTOutput("sf_df")
                        )
                      )
             ),
             tabPanel("Network Model Estimation",
                      tagList(
                        tags$head(
                          tags$link(rel = "stylesheet", type = "text/css",
                                    href = "style.css")
                        )
                      ),
                      fluidRow(
                        column(4,
                               br(),
                               wellPanel(
                                 h4(tags$u("Network Model Estimation")),
                                 
                                 actionButton("runMod", "Fit Model & Run Diagnostics",
                                              style = "margin-bottom: 10px;"),
                                 fluidRow(
                                   column(7, numericInput("num",
                                                          label = "Number of Nodes",
                                                          value = 100,
                                                          min = 0))
                                 ),
                                 
                                 hr(),
                                 h4("Specification: Method 1", style = "margin-bottom:0;"),
                                 helpText("Summary Stat Targets", style = "margin-top:0;"),
                                 
                                 sliderInput("meandeg",
                                             label = "Mean Degree",
                                             value = 0.5,
                                             min = 0.1,
                                             max = 1.5,
                                             step = 0.01),
                                 sliderInput("meandur",
                                             label = "Mean Partnership Duration",
                                             value = 50,
                                             min = 1,
                                             max = 100),
                                 selectInput("conc",
                                             label = "Concurrency Rule",
                                             choices =
                                               c("Concurrency not included in model",
                                                 "Target % concurrency")),
                                 conditionalPanel("input.conc == 'Target % concurrency'",
                                                  helpText("Note: A mean degree greater
                                                than one always implies some
                                                level of concurrency. The model
                                                will not be run if concurrency
                                                is too low for the chosen mean
                                                degree."),
                                                  uiOutput("percConcSlider")
                                 ),
                                 hr(),
                                 h4("Specification: Method 2", style = "margin-bottom:0;"),
                                 helpText("Model and NW Stat Targets",
                                          style = "margin-top:0;"),
                                 fluidRow(
                                   column(7, selectInput("formation",
                                                         label = "Formation Formula",
                                                         choices =
                                                           c("~edges",
                                                             "~edges + concurrent"))),
                                   column(5, numericInput("edge.target",
                                                          label = "Target: edges",
                                                          value = 25,
                                                          step = 0.1),
                                          conditionalPanel("input.formation ==
                                                '~edges + concurrent'",
                                                           numericInput("conc.target",
                                                                        label = "Target: concurrent",
                                                                        value = 10)
                                          )
                                   )),
                                 
                                 fluidRow(
                                   column(7, selectInput("dissolution",
                                                         label = "Dissolution Formula",
                                                         choices = c("~offset(edges)"))),
                                   column(5, numericInput("dur",
                                                          label = "Edge Durations",
                                                          value = 50)))
                               )), #end sidebar
                        #main panel
                        column(8,
                               br(),
                               fluidRow(
                                 column(3, numericInput("dx.nsims",
                                                        label = "Simulations",
                                                        value = 5, min = 1),
                                        actionButton("runDx",
                                                     label = "Re-Run Diagnostics")),
                                 
                                 column(3, numericInput("dx.nsteps",
                                                        label = "Time Steps per Sim",
                                                        value = 500, min = 1)),
                                 column(3, selectInput("nwstats",
                                                       label = "Network Stats to Track",
                                                       multiple = TRUE,
                                                       choices = c("edges",
                                                                   "concurrent",
                                                                   "isolates",
                                                                   "mean degree" =
                                                                     "meandeg"),
                                                       selected = "edges"))
                               ),
                               plotOutput("dxplot", height = "600px"),
                               wellPanel(
                                 h4("Plot Options"),
                                 fluidRow(
                                   column(4,
                                          selectInput("dxtype",
                                                      label = "Plot Type",
                                                      choices = c("formation", "dissolution",
                                                                  "duration"))),
                                   column(5,
                                          sliderInput(inputId = "dx.qntsrng",
                                                      label = "Quantile Band",
                                                      min = 0,
                                                      max = 1,
                                                      value = 0.5,
                                                      step = 0.01))
                                 ),
                                 fluidRow(
                                   column(3,
                                          checkboxInput(inputId = "plots.joined",
                                                        label = "Join Plots",
                                                        value = TRUE)),
                                   column(3,
                                          checkboxInput(inputId = "dx.showmean",
                                                        label = "Mean Line",
                                                        value = TRUE)),
                                   column(3,
                                          checkboxInput(inputId = "dx.showsims",
                                                        label = "Sim Lines",
                                                        value = FALSE)),
                                   column(3,
                                          checkboxInput(inputId = "dx.showleg",
                                                        label = "Legend",
                                                        value = FALSE))),
                                 fluidRow(
                                   column(3,
                                          downloadButton("dxplotDL", label = "Download Plot"))
                                 )
                               ),
                               
                               verbatimTextOutput("modeldx"))
                      )
             ),
             tabPanel("Epidemic Simulation",
                      fluidRow(
                        column(4,
                               br(),
                               wellPanel(
                                 h4(tags$u("Epidemic Simulation")),
                                 actionButton("runEpi", label = "Simulate Epidemic",
                                              style = "margin-bottom: 10px"),
                                 helpText("Click the button above after changing model",
                                          "parameters or conditions."),
                                 selectInput("modtype",
                                             label = "Disease Type",
                                             choices = c("SI", "SIR", "SIS", "Use Model Builder")),
                                 
                                 h4("Initial Conditions", style = "margin-top: 25px;"),
                                 numericInput(inputId = "i.num",
                                              label = "Number Infected",
                                              value = 1,
                                              min = 0),
                                 conditionalPanel("input.modtype == 'SIR'",
                                                  numericInput(inputId = "r.num",
                                                               label = "Number Recovered",
                                                               value = 0,
                                                               min = 0)),
                                 
                                 h4("Time and Simulations", style = "margin-top: 25px;"),
                                 numericInput("epi.nsims",
                                              label = "Simulations",
                                              value = 5, min = 1),
                                 numericInput("epi.nsteps",
                                              label = "Time Steps per Sim",
                                              value = 500, min = 0),
                                 
                                 h4("Parameters", style = "margin-top: 25px;"),
                                 conditionalPanel("input.modtype != 'Use Model Builder'",
                                   numericInput("inf.prob",
                                                label = "Transmission Probability per Act",
                                                min = 0,
                                                max = 1,
                                                value = 0.1,
                                                step = 0.01)
                                   ),
                                 numericInput(inputId = "act.rate",
                                              label = "Act Rate",
                                              min = 0,
                                              value = 0.5,
                                              step = 0.01),
                                 conditionalPanel("input.modtype != 'SI' && input.modtype != 'Use Model Builder'",
                                                  numericInput(inputId = "rec.rate",
                                                               label = "Recovery Rate",
                                                               min = 0,
                                                               value = 0,
                                                               step = 0.01))
                                 #                  numericInput(inputId = "a.rate",
                                 #                               label = "Arrival Rate",
                                 #                               min = 0,
                                 #                               value = 0.0,
                                 #                               step = 0.005),
                                 #                  numericInput(inputId = "ds.rate",
                                 #                               label = "Departure Rate (Sus.)",
                                 #                               min = 0,
                                 #                               value = 0.0,
                                 #                               step = 0.005),
                                 #                  numericInput(inputId = "di.rate",
                                 #                               label = "Departure Rate (Inf.)",
                                 #                               min = 0,
                                 #                               value = 0.0,
                                 #                               step = 0.005),
                                 #                  conditionalPanel("input.modtype == 'SIR'",
                                 #                                   numericInput(inputId = "dr.rate",
                                 #                                                label =
                                 #                                                   "Departure Rate (Rec.)",
                                 #                                                min = 0,
                                 #                                                value = 0.0,
                                 #                                                step = 0.005))
                               )), #end sidebar
                        #main panel
                        column(8,
                               tabsetPanel(
                                 tabPanel("Time Series Plots",
                                          plotOutput("epiplot", height = "600px"),
                                          wellPanel(
                                            h4("Plot Options"),
                                            fluidRow(
                                              column(5,
                                                     selectInput(inputId = "compsel",
                                                                 label = strong("Plot Type"),
                                                                 choices =
                                                                   c("Compartment Prevalence",
                                                                     "Compartment Size",
                                                                     "Disease Incidence"))),
                                              column(5,
                                                     sliderInput(inputId = "epi.qntsrng",
                                                                 label = "Quantile Band",
                                                                 min = 0,
                                                                 max = 1,
                                                                 value = 0.5,
                                                                 step = 0.01))
                                            ),
                                            fluidRow(
                                              column(3,
                                                     checkboxInput(inputId = "epi.showmean",
                                                                   label = "Mean Line",
                                                                   value = TRUE)),
                                              column(3,
                                                     checkboxInput(inputId = "epi.showsims",
                                                                   label = "Sim Lines",
                                                                   value = FALSE)),
                                              column(3,
                                                     checkboxInput(inputId = "epi.showleg",
                                                                   label = "Legend",
                                                                   value = TRUE))),
                                            fluidRow(
                                              downloadButton("epiplotDL", "Download Plot")
                                            )
                                          )
                                 ),
                                 tabPanel("Network Plots",
                                          uiOutput("plotUI"),
                                          br(),
                                          wellPanel(
                                            h4("Plot Options"),
                                            helpText("Plotting the mean network shows the plot
                                      of the simulation that is closest to
                                      the mean prevalence at each time step."),
                                            checkboxInput("secondplot",
                                                          label = "Plot two time steps",
                                                          value = FALSE),
                                            uiOutput("plotoptionsUI")
                                          )
                                 ),
                                 tabPanel("Data",
                                          div(style = "margin: auto; width: 90%;",
                                              br(),
                                              helpText("Select output as the time-specific
                                        means or standard deviations across
                                        simulations, or individual simulation
                                        values (if the last, also input the
                                        desired simulation number)."),
                                              fluidRow(
                                                column(3,
                                                       selectInput(inputId = "datasel",
                                                                   label =
                                                                     strong("Data Selection"),
                                                                   choices =
                                                                     c("Means",
                                                                       "Standard Deviations",
                                                                       "Simulations"))),
                                                conditionalPanel("input.datasel ==
                                                  'Simulations'",
                                                                 column(3,
                                                                        uiOutput("simnoControl"))),
                                                column(3,
                                                       numericInput(inputId = "tabdig",
                                                                    label =
                                                                      "Significant Digits",
                                                                    min = 0,
                                                                    value = 2))),
                                              fluidRow(
                                                dataTableOutput("outData")),
                                              fluidRow(
                                                downloadButton(outputId = "dlData",
                                                               label = "Download Data"))
                                          )
                                 ),
                                 tabPanel("Summary",
                                          br(),
                                          uiOutput("sumtimeui"),
                                          verbatimTextOutput("episum")
                                 )
                               )
                               
                        ) #end main panel
                      )
             ) #end epi page
  )
)