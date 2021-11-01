shinyUI(fluidPage(
  
  # Application title
  titlePanel("Model Selection"),
  tabsetPanel(
    tabPanel("Data",
             verbatimTextOutput(outputId = "DataSummary"),
             fluidRow(
               column(width = 4,
                      sliderInput(inputId = "Multiplier", label = "IQR multiplier", min = 0, max = 10, step = 0.1, value = 1.5)
               ),
               column(width = 3,
                      checkboxInput(inputId = "Normalise", label = "Standardise chart", value = TRUE)
               )
             ),
             plotOutput(outputId = "BoxPlots"),
             plotOutput(outputId = "Missing"),
             plotOutput(outputId = "Corr"),
             DT::dataTableOutput(outputId = "Table")
    ), 
    tabPanel("Split",
             sliderInput(inputId = "Split", label = "Train proportion", min = 0, max = 1, value = 0.8),
             verbatimTextOutput(outputId = "SplitSummary")
    ),
    tabPanel("Available methods",
             h3("Regression methods in caret"),
             shinycssloaders::withSpinner(DT::dataTableOutput(outputId = "Available"))
    ),
    tabPanel("Methods",
             checkboxInput(inputId = "Parallel", label = "Use parallel processing", value = FALSE),
             bsTooltip(id = "Parallel", title = "Turn off parallel processing to view any training errors in the console"),
             "The preprocessing steps and their order are important.",
             HTML("See function <code>dynamicSteps</code> in global.R for interpretation of preprocessing options. "),
             "Documentation", tags$a("here", href = "https://www.rdocumentation.org/packages/recipes/versions/0.1.16", target = "_blank"),
             
             tabsetPanel(type = "pills",
                         tabPanel("NULL Model",
                                  br(),
                                  fluidRow(
                                    column(width = 4),
                                    column(width = 1,
                                           actionButton(inputId = "null_Go", label = "Train", icon = icon("play")),
                                           bsTooltip(id = "null_Go", title = "This will train or retrain your model")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "null_Metrics"),
                                  hr(),
                                  verbatimTextOutput(outputId = "null_Recipe")
                         ),
                         tabPanel("GLMnet Model",
                                  verbatimTextOutput(outputId = "glmnet_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "glmnet_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(glmnet_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = glmnet_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "glmnet_Preprocess", title = "These entries will be populated in the correct order from the saved model once it loads")

                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmnet_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "glmnet_Go", title = "This will train or retrain your model")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "glmnet_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "glmnet_ModelPlots"),
                                  verbatimTextOutput(outputId = "glmnet_Recipe"),
                                  verbatimTextOutput(outputId = "glmnet_ModelSummary2")
                         ),
                         tabPanel("PLS Model",
                                  verbatimTextOutput(outputId = "pls_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "pls_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(pls_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = pls_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "pls_Preprocess", title = "These entries will be populated in the correct order from the saved model once it loads")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pls_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "pls_Go", title = "This will train or retrain your model")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "pls_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "pls_ModelPlots"),
                                  verbatimTextOutput(outputId = "pls_Recipe"),
                                  verbatimTextOutput(outputId = "pls_ModelSummary2")
                         ),
                         tabPanel("Rpart Model",
                                  verbatimTextOutput(outputId = "rpart_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models                                 selectizeInput(inputId = "rpart_Preprocess",
                                           selectizeInput(inputId = "rpart_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(rpart_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = rpart_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "rpart_Preprocess", title = "These entries will be populated in the correct order from the saved model once it loads")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rpart_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "rpart_Go", title = "This will train or retrain your model")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "rpart_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "rpart_ModelPlots"),
                                  plotOutput(outputId = "rpart_ModelTree"), #  <- this tree plot is unique to the rpart method
                                  verbatimTextOutput(outputId = "rpart_Recipe")
                         ),
                         
                         
                         # maintenance point ------------------------------------------------------------------------------
                         # add further tabs (with controls) here
                         
                         
                         tabPanel("Ridge Model",
                                  verbatimTextOutput(outputId = "ridge_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "ridge_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(ridge_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = ridge_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "ridge_Preprocess", title = "These entries will be populated in the correct order from the saved model once it loads")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "ridge_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "ridge_Go", title = "This will train or retrain your model")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "ridge_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "ridge_ModelPlots"),
                                  verbatimTextOutput(outputId = "ridge_Recipe"),
                                  verbatimTextOutput(outputId = "ridge_ModelSummary2")
                         ),
                         
                         
                         tabPanel("Ppr Model",
                                  verbatimTextOutput(outputId = "ppr_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "ppr_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(ppr_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = ppr_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "ppr_Preprocess", title = "These entries will be populated in the correct order from the saved model once it loads")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "ppr_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "ppr_Go", title = "This will train or retrain your model")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "ppr_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "ppr_ModelPlots"),
                                  verbatimTextOutput(outputId = "ppr_Recipe"),
                                  verbatimTextOutput(outputId = "ppr_ModelSummary2")
                         ),
                         
                         
                         tabPanel("Rqnc Model",
                                  verbatimTextOutput(outputId = "rqnc_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "rqnc_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(rqnc_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = rqnc_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "rqnc_Preprocess", title = "These entries will be populated in the correct order from the saved model once it loads")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rqnc_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "rqnc_Go", title = "This will train or retrain your model")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "rqnc_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "rqnc_ModelPlots"),
                                  verbatimTextOutput(outputId = "rqnc_Recipe"),
                                  verbatimTextOutput(outputId = "rqnc_ModelSummary2")
                         ),
                         
                         
                         tabPanel("Spikeslab Model",
                                  verbatimTextOutput(outputId = "spikeslab_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "spikeslab_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(spikeslab_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = spikeslab_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "spikeslab_Preprocess", title = "These entries will be populated in the correct order from the saved model once it loads")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "spikeslab_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "spikeslab_Go", title = "This will train or retrain your model")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "spikeslab_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "spikeslab_ModelPlots"),
                                  verbatimTextOutput(outputId = "spikeslab_Recipe"),
                                  verbatimTextOutput(outputId = "spikeslab_ModelSummary2")
                         ),
                         
                         
                         tabPanel("LeapForward Model",
                                  verbatimTextOutput(outputId = "leapForward_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "leapForward_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(leapForward_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = leapForward_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "leapForward_Preprocess", title = "These entries will be populated in the correct order from the saved model once it loads")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "leapForward_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "leapForward_Go", title = "This will train or retrain your model")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "leapForward_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "leapForward_ModelPlots"),
                                  verbatimTextOutput(outputId = "leapForward_Recipe"),
                                  verbatimTextOutput(outputId = "leapForward_ModelSummary2")
                         ),
                         
                         
                         tabPanel("LeapBackward Model",
                                  verbatimTextOutput(outputId = "leapBackward_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "leapBackward_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(leapBackward_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = leapBackward_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "leapBackward_Preprocess", title = "These entries will be populated in the correct order from the saved model once it loads")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "leapBackward_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "leapBackward_Go", title = "This will train or retrain your model")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "leapBackward_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "leapBackward_ModelPlots"),
                                  verbatimTextOutput(outputId = "leapBackward_Recipe"),
                                  verbatimTextOutput(outputId = "leapBackward_ModelSummary2")
                         ),
                         
                         
                         tabPanel("LeapSeq Model",
                                  verbatimTextOutput(outputId = "leapSeq_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "leapSeq_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(leapSeq_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = leapSeq_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "leapSeq_Preprocess", title = "These entries will be populated in the correct order from the saved model once it loads")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "leapSeq_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "leapSeq_Go", title = "This will train or retrain your model")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "leapSeq_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "leapSeq_ModelPlots"),
                                  verbatimTextOutput(outputId = "leapSeq_Recipe"),
                                  verbatimTextOutput(outputId = "leapSeq_ModelSummary2")
                         ),
                         
                         
                         
                         tabPanel("Lm Model",
                                  verbatimTextOutput(outputId = "lm_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "lm_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(lm_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = lm_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "lm_Preprocess", title = "These entries will be populated in the correct order from the saved model once it loads")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "lm_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "lm_Go", title = "This will train or retrain your model")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "lm_Metrics"),
                                  hr(),
                                  #plotOutput(outputId = "lm_ModelPlots"),
                                  verbatimTextOutput(outputId = "lm_Recipe"),
                                  verbatimTextOutput(outputId = "lm_ModelSummary2")
                         ),
                         

                         
                         tabPanel("Rlm Model",
                                  verbatimTextOutput(outputId = "rlm_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "rlm_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(rlm_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = rlm_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "rlm_Preprocess", title = "These entries will be populated in the correct order from the saved model once it loads")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rlm_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "rlm_Go", title = "This will train or retrain your model")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "rlm_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "rlm_ModelPlots"),
                                  verbatimTextOutput(outputId = "rlm_Recipe"),
                                  verbatimTextOutput(outputId = "rlm_ModelSummary2")
                         ),
                         
                         
                         
                         
                         tabPanel("LmStepAIC Model",
                                  verbatimTextOutput(outputId = "lmStepAIC_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "lmStepAIC_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(lmStepAIC_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = lmStepAIC_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "lmStepAIC_Preprocess", title = "These entries will be populated in the correct order from the saved model once it loads")
                                           
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "lmStepAIC_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "lmStepAIC_Go", title = "This will train or retrain your model")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "lmStepAIC_Metrics"),
                                  hr(),
                                  
                                  verbatimTextOutput(outputId = "lmStepAIC_Recipe"),
                                  verbatimTextOutput(outputId = "lmStepAIC_ModelSummary2")
                         )
                         
                         
                         
                         # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
             )
    ),
    tabPanel("Model Selection",
             tags$h5("Cross validation results:"),
             checkboxInput(inputId = "Notch", label = "Show notch", value = FALSE),
             checkboxInput(inputId = "NullNormalise", label = "Normalise", value = TRUE),
             checkboxInput(inputId = "HideWorse", label = "Hide models worse than null model", value = TRUE),
             plotOutput(outputId = "SelectionBoxPlot"),
             radioButtons(inputId = "Choice", label = "Model choice", choices = c(""), inline = TRUE )
    ),
    tabPanel("Performance",
             htmlOutput(outputId = "Title"),
             verbatimTextOutput(outputId = "TestSummary"),
             fluidRow(
               column(offset = 2, width = 4,
                      plotOutput(outputId = "TestPlot")
               ),
               column(width = 2,
                      plotOutput(outputId = "TestResiduals")
               ),
               column(width = 2,
                      plotOutput(outputId = "TrainResiduals"),
               )
             ),
             sliderInput(inputId = "IqrM", label = "IQR multiplier", min = 0, max = 5, value = 1.5, step = 0.1),
    )
  )
))
