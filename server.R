shinyServer(function(input, output, session) {
  
  # initialisation ----
  models <- reactiveValues()  # this is a collection of the models

    
  # Ensure the "SavedModels folder exists
  if (!"./SavedModels" %in% list.dirs()) {
    dir.create("./SavedModels")
  }
  
  # load the previously trained models - Note: you can delete files in the SavedModels directory
  for (rdsfile in list.files(path = "SavedModels", pattern = "\\.rds")) {
    name <- gsub(rdsfile, pattern = "\\.rds$", replacement = "")
    rdsfile <- paste0(".", .Platform$file.sep, "SavedModels", .Platform$file.sep, rdsfile)
    showNotification(paste("Loading trained model", name, "from file", rdsfile), session = session, duration = 3)
    model <- readRDS(file = rdsfile)
    models[[name]] <- model
    
    # try to update the preprocessing steps with the ones that were used
    steps <- model$recipe$steps
    seld <- c()
    for (step in steps) {
      s <- gsub(pattern = "step_", replacement = "", x = class(step)[1])
      if (s == "date") {
        s <- step$features[1]
      }
      seld <- c(seld, s)
    }
    preprocessingInputId <- paste0(name, "_Preprocess")
    updateSelectizeInput(session = session, inputId = preprocessingInputId, choices = ppchoices, selected = seld)
    if (length(seld) > 0) {
      showNotification(paste("Setting preprocessing for", name, "to", paste(seld, collapse = ",")), session = session, duration = 5)
    }
  }
  
  shiny::onSessionEnded(stopApp)

  
  # reactive getData ----
  getData <- reactive({
    d <- read.csv(file = "Data.csv", row.names = "Patient", stringsAsFactors = TRUE)  # "Patient" is no longer a variable
    d$ObservationDate <- as.Date(d$ObservationDate, "%Y-%m-%d")
    d
  })
  
  # output BoxPlots ----
  output$BoxPlots <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, input$Multiplier, length(numeric) > 0)
    d <- scale(d[,numeric], center = input$Normalise, scale = input$Normalise)
    boxplot(d, outline = TRUE, main = paste("Boxplot using IQR multiplier of", input$Multiplier), range = input$Multiplier, las = 2)
  })
  
  # output Missing ----
  output$Missing <- renderPlot({
    d <- getData()
    vis_dat(d)
  })
  
  # output Corr ----
  output$Corr <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, length(numeric) > 0)
    corrgram::corrgram(d, order = "OLO", main = "Numeric Data Correlation")
  })
  
  # output DataSummary ----
  output$DataSummary <- renderPrint({
    str(getData())
  })
  
  # output Table ----
  output$Table <- DT::renderDataTable({
    d <- getData()
    numeric <- c(FALSE, sapply(d, is.numeric)) # never round rownames which are the first column (when shown)
    DT::datatable(d) %>%
      formatRound(columns = numeric, digits = 3)
  })
  
  # reactive get Split
  getSplit <- reactive({
    set.seed(199)
    createDataPartition(y = getData()$Y, p = input$Split, list = FALSE)
  })
  
  # reactive getMethods ----
  getMethods <- reactive({
    mi <- caret::getModelInfo()
    Label <- vector(mode = "character", length = length(mi))
    Package <- vector(mode = "character", length = length(mi))
    Hyperparams <- vector(mode = "character", length = length(mi))
    Regression <- vector(mode = "logical", length = length(mi))
    Classification <- vector(mode = "logical", length = length(mi))
    Tags <- vector(mode = "character", length = length(mi))
    ClassProbs <- vector(mode = "character", length = length(mi))
    for (row in 1:length(mi)) {
      Label[row] <- mi[[row]]$label
      libs <- mi[[row]]$library
      libs <- na.omit(libs[libs != ""]) # remove blank libraries
      if (length(libs) > 0) {
        present <- vector(mode = "logical", length = length(libs))
        suppressWarnings({
          for (lib in 1:length(libs)) {
            present[lib] <- require(package = libs[lib], warn.conflicts = FALSE, character.only = TRUE, quietly = TRUE)
          }
        })
        check <- ifelse(present, "", as.character(icon(name = "ban")))
        Package[row] <- paste(collapse = "<br/>", paste(mi[[row]]$library, check))
      }
      d <- mi[[row]]$parameters
      Hyperparams[row] <- paste(collapse = "<br/>", paste0(d$parameter, " - ", d$label, " [", d$class,"]"))
      Regression[row] <- ifelse("Regression" %in% mi[[row]]$type, as.character(icon("check-square", class = "fa-3x")), "")
      Classification[row] <- ifelse("Classification" %in% mi[[row]]$type , as.character(icon("check-square", class = "fa-3x")),"")
      Tags[row] <- paste(collapse = "<br/>", mi[[row]]$tags)
      ClassProbs[row] <- ifelse(is.function(mi[[row]]$prob), as.character(icon("check-square", class = "fa-3x")), "")
    }
    data.frame(Model = names(mi), Label, Package, Regression, Classification, Tags, Hyperparams, ClassProbs, stringsAsFactors = FALSE)
  })
  
  # output Available ----
  output$Available <- DT::renderDataTable({
     m <- getMethods()
     m <- m[m$Regression != "", !colnames(m) %in% c("Regression", "Classification", "ClassProbs")]  # hide columns because we are looking at regression methods only
     DT::datatable(m, escape = FALSE, options = list(pageLength = 5, lengthMenu = c(5,10,15)), rownames = FALSE, selection = "none")
  })
  
  # reactive getTrainData ----
  getTrainData <- reactive({
    getData()[getSplit(),]
  })
  
  # reactive getTestData ----
  getTestData <- reactive({
    getData()[-getSplit(),]
  })
  
  # reactive getTrControl ----
  getTrControl <- reactive({
    # shared bootstrap specification i.e. 25 x bootstrap
    y <- getTrainData()[,"Y"]
    n <- 25
    set.seed(673)
    seeds <- vector(mode = "list", length = n + 1)
    for (i in 1:n) {
      seeds[[i]] <- as.integer(c(runif(n = 15, min = 1000, max = 5000)))
    }
    seeds[[n + 1]] <- as.integer(runif(n = 1, min = 1000, max = 5000))
    trainControl(method = "boot", number = n, repeats = NA, allowParallel = TRUE, search = "grid", 
                 index = caret::createResample(y = y, times = n), savePredictions = "final", seeds = seeds, 
                 trim = TRUE)
  })
  
  # output SplitSummary ----
  output$SplitSummary <- renderPrint({
    cat(paste("Training observations:", nrow(getTrainData()), "\n", "Testing observations:", nrow(getTestData())))
  })
  

  
  # METHOD * null ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getNullRecipe ----
  getNullRecipe <- reactive({
    form <- formula(Y ~ .)
    environment(form) <- NULL
    recipes::recipe(form, data = getTrainData())
  })
  
  # observeEvent null_Go ----
  observeEvent(
    input$null_Go,
    {
      method <- "null"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getNullRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl())
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # observeEvent null_Metrics ----
  output$null_Metrics <- renderTable({
    req(models$null)
    models$null$results[ which.min(models$null$results[, "RMSE"]), ]
  })
  
  # output null_Recipe ---
  output$null_Recipe <- renderPrint({
    req(models$null)
    models$null$recipe
  })  
  

  
  
  # METHOD * glmnet ---------------------------------------------------------------------------------------------------------------------------

  # reactive getGlmnetRecipe ----
  getGlmnetRecipe <- reactive({
    form <- formula(Y ~ .)
    environment(form) <- NULL
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$glmnet_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent glmnet_Go ----
  observeEvent(
    input$glmnet_Go,
    {
      library(glmnet)   #  <------ Declare any modelling packages that are needed (see Method List tab)
      method <- "glmnet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getGlmnetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5, na.action = na.pass)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
     }
  )
  
  # output glmnet_ModelSummary (text) ----
  output$glmnet_ModelSummary0 <- renderText({
    description("glmnet")   # Use the caret method name here
  })
  
  # output glmnet_Metrics (table) ----
  output$glmnet_Metrics <- renderTable({
    req(models$glmnet)
    models$glmnet$results[ which.min(models$glmnet$results[, "RMSE"]), ]
  })
  
  # output glmnet_ModelPlots (plot) ----
  output$glmnet_ModelPlots <- renderPlot({
    req(models$glmnet)
    plot(models$glmnet)
  })

  # output glmnet_Recipe (print) ----
  output$glmnet_Recipe <- renderPrint({
    req(models$glmnet)
    models$glmnet$recipe
  })  
  
  # output glmnet_ModelSummary2 (print) ----
  output$glmnet_ModelSummary2 <- renderPrint({
    req(models$glmnet)
    print(models$glmnet)
  })


  
  
  # METHOD * pls ---------------------------------------------------------------------------------------------------------------------------

  # reactive getPlsRecipe ----
  getPlsRecipe <- reactive({
    form <- formula(Y ~ .)
    environment(form) <- NULL
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$pls_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent pls_Go ----
  observeEvent(
    input$pls_Go,
    {
      library(pls)  #  <------ Declare any modelling packages that are needed (see Method List tab)
      method <- "pls"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getPlsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                              tuneLength = 25, na.action = na.pass)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output pls_ModelSummary0 (text) ----
  output$pls_ModelSummary0 <- renderText({
    description("pls")   # Use the caret method name here
  })

  # output pls_Metrics (table) ----
  output$pls_Metrics <- renderTable({
    req(models$pls)
    models$pls$results[ which.min(models$pls$results[, "RMSE"]), ]
  })
  
  # output pls_ModelPlots (plot) ----
  output$pls_ModelPlots <- renderPlot({
    req(models$pls)
    plot(models$pls)
  })     
  
  # output pls_Recipe (print) ----
  output$pls_Recipe <- renderPrint({
    req(models$pls)
    models$pls$recipe
  })  

  # output pls_ModelSummary2 (print) ----
  output$pls_ModelSummary2 <- renderPrint({
    req(models$pls)
    summary(models$pls$finalModel)
  })
  

  
  # METHOD * rpart ---------------------------------------------------------------------------------------------------------------------------

  # reactive getRpartRecipe ----
  getRpartRecipe <- reactive({
    form <- formula(Y ~ .)
    environment(form) <- NULL
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$rpart_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))
  })
  
  # observeEvent rpart_Go ----
  observeEvent(
    input$rpart_Go,
    {
      library(rpart)  #  <------ Declare any modelling packages that are needed (see Method List tab)
      method <- "rpart"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getRpartRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                              tuneLength = 5, na.action = na.rpart)  #<- note the rpart-specific value for na.action (not needed for other methods)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  # output rpart_ModelSummary0 (print) ----
  output$rpart_ModelSummary0 <- renderText({
    description("rpart")   # Use the caret method name here
  })
  
  # output rpart_Metrics (table) ----
  output$rpart_Metrics <- renderTable({
    req(models$rpart)
    models$rpart$results[ which.min(models$rpart$results[, "RMSE"]), ]
  })
  
  # output rpart_Recipe (print) ----
  output$rpart_Recipe <- renderPrint({
    req(models$rpart)
    models$rpart$recipe
  })  
  
  # output rpart_ModelPlots (plot) ----
  output$rpart_ModelPlots <- renderPlot({
    req(models$rpart)
    plot(models$rpart)
  })
  
  # output rpart_ModelTree (plot) ----
  output$rpart_ModelTree <- renderPlot({
    library(rpart.plot)
    req(models$rpart)
    rpart.plot::rpart.plot(models$rpart$finalModel, roundint = FALSE)
  })     
  

  
  # maintenance point ---------------------------------------------------------------------------------------------------------------------------
  # add further methods here  

  
  
  # METHOD * leapForward ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getLeapForwardRecipe ----
  getLeapForwardRecipe <- reactive({
    form <- formula(Y ~ .)
    environment(form) <- NULL
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$leapForward_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent leapForward_Go ----
  observeEvent(
    input$leapForward_Go,
    {
      library(leaps)   #  <------ Declare any modelling packages that are needed (see Method List tab)
      method <- "leapForward"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getLeapForwardRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5, na.action = na.pass)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output leapForward_ModelSummary (text) ----
  output$leapForward_ModelSummary0 <- renderText({
    description("leapForward")   # Use the caret method name here
  })
  
  # output leapForward_Metrics (table) ----
  output$leapForward_Metrics <- renderTable({
    req(models$leapForward)
    models$leapForward$results[ which.min(models$leapForward$results[, "RMSE"]), ]
  })
  
  # output leapForward_ModelPlots (plot) ----
  output$leapForward_ModelPlots <- renderPlot({
    req(models$leapForward)
    plot(models$leapForward)
  })
  
  # output leapForward_Recipe (print) ----
  output$leapForward_Recipe <- renderPrint({
    req(models$leapForward)
    models$leapForward$recipe
  })  
  
  # output leapForward_ModelSummary2 (print) ----
  output$leapForward_ModelSummary2 <- renderPrint({
    req(models$leapForward)
    print(models$leapForward)
  })
  
  
  
  
  # METHOD * ridge ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getRidgeRecipe ----
  getRidgeRecipe <- reactive({
    form <- formula(Y ~ .)
    environment(form) <- NULL
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$ridge_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent ridge_Go ----
  observeEvent(
    input$ridge_Go,
    {
      library(elasticnet)   #  <------ Declare any modelling packages that are needed (see Method List tab)
      method <- "ridge"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getRidgeRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5, na.action = na.pass)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output ridge_ModelSummary (text) ----
  output$ridge_ModelSummary0 <- renderText({
    description("ridge")   # Use the caret method name here
  })
  
  # output ridge_Metrics (table) ----
  output$ridge_Metrics <- renderTable({
    req(models$ridge)
    models$ridge$results[ which.min(models$ridge$results[, "RMSE"]), ]
  })
  
  # output ridge_ModelPlots (plot) ----
  output$ridge_ModelPlots <- renderPlot({
    req(models$ridge)
    plot(models$ridge)
  })
  
  # output ridge_Recipe (print) ----
  output$ridge_Recipe <- renderPrint({
    req(models$ridge)
    models$ridge$recipe
  })  
  
  # output ridge_ModelSummary2 (print) ----
  output$ridge_ModelSummary2 <- renderPrint({
    req(models$ridge)
    print(models$ridge)
  })
  
  
 
  # METHOD * ppr ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getPprRecipe ----
  getPprRecipe <- reactive({
    form <- formula(Y ~ .)
    environment(form) <- NULL
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$ppr_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent ppr_Go ----
  observeEvent(
    input$ppr_Go,
    {
      #library(ppr)   #  <------ Declare any modelling packages that are needed (see Method List tab)
      method <- "ppr"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getPprRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5, na.action = na.pass)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output ppr_ModelSummary (text) ----
  output$ppr_ModelSummary0 <- renderText({
    description("ppr")   # Use the caret method name here
  })
  
  # output ppr_Metrics (table) ----
  output$ppr_Metrics <- renderTable({
    req(models$ppr)
    models$ppr$results[ which.min(models$ppr$results[, "RMSE"]), ]
  })
  
  # output ppr_ModelPlots (plot) ----
  output$ppr_ModelPlots <- renderPlot({
    req(models$ppr)
    plot(models$ppr)
  })
  
  # output ppr_Recipe (print) ----
  output$ppr_Recipe <- renderPrint({
    req(models$ppr)
    models$ppr$recipe
  })  
  
  # output ppr_ModelSummary2 (print) ----
  output$ppr_ModelSummary2 <- renderPrint({
    req(models$ppr)
    print(models$ppr)
  })
  
  
  # METHOD * rqnc ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getRqncRecipe ----
  getRqncRecipe <- reactive({
    form <- formula(Y ~ .)
    environment(form) <- NULL
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$rqnc_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent rqnc_Go ----
  observeEvent(
    input$rqnc_Go,
    {
      library(rqPen)   #  <------ Declare any modelling packages that are needed (see Method List tab)
      method <- "rqnc"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getRqncRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5, na.action = na.pass)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output rqnc_ModelSummary (text) ----
  output$rqnc_ModelSummary0 <- renderText({
    description("rqnc")   # Use the caret method name here
  })
  
  # output rqnc_Metrics (table) ----
  output$rqnc_Metrics <- renderTable({
    req(models$rqnc)
    models$rqnc$results[ which.min(models$rqnc$results[, "RMSE"]), ]
  })
  
  # output rqnc_ModelPlots (plot) ----
  output$rqnc_ModelPlots <- renderPlot({
    req(models$rqnc)
    plot(models$rqnc)
  })
  
  # output rqnc_Recipe (print) ----
  output$rqnc_Recipe <- renderPrint({
    req(models$rqnc)
    models$rqnc$recipe
  })  
  
  # output rqnc_ModelSummary2 (print) ----
  output$rqnc_ModelSummary2 <- renderPrint({
    req(models$rqnc)
    print(models$rqnc)
  })
  
  
  
  # METHOD * leapSeq ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getLeapSeqRecipe ----
  getLeapSeqRecipe <- reactive({
    form <- formula(Y ~ .)
    environment(form) <- NULL
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$leapSeq_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent leapSeq_Go ----
  observeEvent(
    input$leapSeq_Go,
    {
      library(leaps)   #  <------ Declare any modelling packages that are needed (see Method List tab)
      method <- "leapSeq"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getLeapSeqRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5, na.action = na.pass)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output leapSeq_ModelSummary (text) ----
  output$leapSeq_ModelSummary0 <- renderText({
    description("leapSeq")   # Use the caret method name here
  })
  
  # output leapSeq_Metrics (table) ----
  output$leapSeq_Metrics <- renderTable({
    req(models$leapSeq)
    models$leapSeq$results[ which.min(models$leapSeq$results[, "RMSE"]), ]
  })
  
  # output leapSeq_ModelPlots (plot) ----
  output$leapSeq_ModelPlots <- renderPlot({
    req(models$leapSeq)
    plot(models$leapSeq)
  })
  
  # output leapSeq_Recipe (print) ----
  output$leapSeq_Recipe <- renderPrint({
    req(models$leapSeq)
    models$leapSeq$recipe
  })  
  
  # output leapSeq_ModelSummary2 (print) ----
  output$leapSeq_ModelSummary2 <- renderPrint({
    req(models$leapSeq)
    print(models$leapSeq)
  })
  
  
  
  # METHOD * spikeslab ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getSpikeslabRecipe ----
  getSpikeslabRecipe <- reactive({
    form <- formula(Y ~ .)
    environment(form) <- NULL
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$spikeslab_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent spikeslab_Go ----
  observeEvent(
    input$spikeslab_Go,
    {
      library(spikeslab)   #  <------ Declare any modelling packages that are needed (see Method List tab)
      library(plyr)
      method <- "spikeslab"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getSpikeslabRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5, na.action = na.pass)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output spikeslab_ModelSummary (text) ----
  output$spikeslab_ModelSummary0 <- renderText({
    description("spikeslab")   # Use the caret method name here
  })
  
  # output spikeslab_Metrics (table) ----
  output$spikeslab_Metrics <- renderTable({
    req(models$spikeslab)
    models$spikeslab$results[ which.min(models$spikeslab$results[, "RMSE"]), ]
  })
  
  # output spikeslab_ModelPlots (plot) ----
  output$spikeslab_ModelPlots <- renderPlot({
    req(models$spikeslab)
    plot(models$spikeslab)
  })
  
  # output spikeslab_Recipe (print) ----
  output$spikeslab_Recipe <- renderPrint({
    req(models$spikeslab)
    models$spikeslab$recipe
  })  
  
  # output spikeslab_ModelSummary2 (print) ----
  output$spikeslab_ModelSummary2 <- renderPrint({
    req(models$spikeslab)
    print(models$spikeslab)
  })
  
  
  
  # METHOD * rlm ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getRlmRecipe ----
  getRlmRecipe <- reactive({
    form <- formula(Y ~ .)
    environment(form) <- NULL
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$rlm_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent rlm_Go ----
  observeEvent(
    input$rlm_Go,
    {
      library(MASS)   #  <------ Declare any modelling packages that are needed (see Method List tab)
      method <- "rlm"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getRlmRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5, na.action = na.pass)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output rlm_ModelSummary (text) ----
  output$rlm_ModelSummary0 <- renderText({
    description("rlm")   # Use the caret method name here
  })
  
  # output rlm_Metrics (table) ----
  output$rlm_Metrics <- renderTable({
    req(models$rlm)
    models$rlm$results[ which.min(models$rlm$results[, "RMSE"]), ]
  })
  
  # output rlm_ModelPlots (plot) ----
  output$rlm_ModelPlots <- renderPlot({
    req(models$rlm)
    plot(models$rlm)
  })
  
  # output rlm_Recipe (print) ----
  output$rlm_Recipe <- renderPrint({
    req(models$rlm)
    models$rlm$recipe
  })  
  
  # output rlm_ModelSummary2 (print) ----
  output$rlm_ModelSummary2 <- renderPrint({
    req(models$rlm)
    print(models$rlm)
  })
  
  
  
  # METHOD * leapBackward ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getLeapBackwardRecipe ----
  getLeapBackwardRecipe <- reactive({
    form <- formula(Y ~ .)
    environment(form) <- NULL
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$leapBackward_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent leapBackward_Go ----
  observeEvent(
    input$leapBackward_Go,
    {
      library(leaps)   #  <------ Declare any modelling packages that are needed (see Method List tab)
      method <- "leapBackward"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getLeapBackwardRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5, na.action = na.pass)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output leapBackward_ModelSummary (text) ----
  output$leapBackward_ModelSummary0 <- renderText({
    description("leapBackward")   # Use the caret method name here
  })
  
  # output leapBackward_Metrics (table) ----
  output$leapBackward_Metrics <- renderTable({
    req(models$leapBackward)
    models$leapBackward$results[ which.min(models$leapBackward$results[, "RMSE"]), ]
  })
  
  # output leapBackward_ModelPlots (plot) ----
  output$leapBackward_ModelPlots <- renderPlot({
    req(models$leapBackward)
    plot(models$leapBackward)
  })
  
  # output leapBackward_Recipe (print) ----
  output$leapBackward_Recipe <- renderPrint({
    req(models$leapBackward)
    models$leapBackward$recipe
  })  
  
  # output leapBackward_ModelSummary2 (print) ----
  output$leapBackward_ModelSummary2 <- renderPrint({
    req(models$leapBackward)
    print(models$leapBackward)
  })
  
  
  
  # METHOD * lmStepAIC ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getLmStepAICRecipe ----
  getLmStepAICRecipe <- reactive({
    form <- formula(Y ~ .)
    environment(form) <- NULL
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$lmStepAIC_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent lmStepAIC_Go ----
  observeEvent(
    input$lmStepAIC_Go,
    {
      library(MASS)   #  <------ Declare any modelling packages that are needed (see Method List tab)
      method <- "lmStepAIC"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getLmStepAICRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5, na.action = na.pass)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output lmStepAIC_ModelSummary (text) ----
  output$lmStepAIC_ModelSummary0 <- renderText({
    description("lmStepAIC")   # Use the caret method name here
  })
  
  # output lmStepAIC_Metrics (table) ----
  output$lmStepAIC_Metrics <- renderTable({
    req(models$lmStepAIC)
    models$lmStepAIC$results[ which.min(models$lmStepAIC$results[, "RMSE"]), ]
  })
  
  # output lmStepAIC_ModelPlots (plot) ----
  output$lmStepAIC_ModelPlots <- renderPlot({
    req(models$lmStepAIC)
    plot(models$lmStepAIC)
  })
  
  # output lmStepAIC_Recipe (print) ----
  output$lmStepAIC_Recipe <- renderPrint({
    req(models$lmStepAIC)
    models$lmStepAIC$recipe
  })  
  
  # output lmStepAIC_ModelSummary2 (print) ----
  output$lmStepAIC_ModelSummary2 <- renderPrint({
    req(models$lmStepAIC)
    print(models$lmStepAIC)
  })
  
  
  
  # METHOD * lm ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getLmRecipe ----
  getLmRecipe <- reactive({
    form <- formula(Y ~ .)
    environment(form) <- NULL
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$lm_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent lm_Go ----
  observeEvent(
    input$lm_Go,
    {
      #library(lm)   #  <------ Declare any modelling packages that are needed (see Method List tab)
      method <- "lm"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        model <- caret::train(getLmRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5, na.action = na.pass)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  # output lm_ModelSummary (text) ----
  output$lm_ModelSummary0 <- renderText({
    description("lm")   # Use the caret method name here
  })
  
  # output lm_Metrics (table) ----
  output$lm_Metrics <- renderTable({
    req(models$lm)
    models$lm$results[ which.min(models$lm$results[, "RMSE"]), ]
  })
  
  # output lm_ModelPlots (plot) ----
  output$lm_ModelPlots <- renderPlot({
    req(models$lm)
    plot(models$lm)
  })
  
  # output lm_Recipe (print) ----
  output$lm_Recipe <- renderPrint({
    req(models$lm)
    models$lm$recipe
  })  
  
  # output lm_ModelSummary2 (print) ----
  output$lm_ModelSummary2 <- renderPrint({
    req(models$lm)
    print(models$lm)
  })
  
  

  
  
  
  
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------

  
  # reactive getResamples ----
  getResamples <- reactive({
    models <- reactiveValuesToList(models)
    req(length(models) > 1)
    results <- caret::resamples(models)
    
    #scale metrics using null model. Tough code to follow -sorry
    NullModel <- "null"
    if (input$NullNormalise & NullModel %in% results$models) {
      actualNames <- colnames(results$values)
      # Normalise the various hyper-metrics except R2 (as this is already normalised)
      for (metric in c("RMSE", "MAE")) {
        col <- paste(sep = "~", NullModel, metric)
        if (col %in% actualNames) {
          nullMetric <- mean(results$values[, col], na.rm = TRUE)
          if (!is.na(nullMetric) & nullMetric != 0) {
            for (model in results$models) {
              mcol <- paste(sep = "~", model, metric)
              if (mcol %in% actualNames) {
                results$values[, mcol] <- results$values[, mcol] / nullMetric
              }
            }
          }
        }
      }
    }

    # hide results worse than null model
    subset <- rep(TRUE, length(models))
    if (input$HideWorse & NullModel %in% names(models)) {
      actualNames <- colnames(results$values)
      col <- paste(sep = "~", "null","RMSE" )
      if (col %in% actualNames) {
        nullMetric <- mean(results$values[, col], na.rm = TRUE)
        if (!is.na(nullMetric)) {
          m <- 0
          for (model in results$models) {
            m <- m + 1
            mcol <- paste(sep = "~", model, "RMSE")
            if (mcol %in% actualNames) {
              subset[m] <- mean(results$values[, mcol], na.rm = TRUE) <= nullMetric
            }
          }
        }
      }
      results$models <- results$models[subset]
    }
    
    updateRadioButtons(session = session, inputId = "Choice", choices = results$models)
    results
  })
  
  # output SelectionBoxPlot (plot) ----
  output$SelectionBoxPlot <- renderPlot({
    mod <- getResamples()
    bwplot(mod, notch = input$Notch)
  })
  
  # output Title (UI) ----
  output$Title <- renderUI({
    tags$h3(paste("Unseen data results for chosen model:", input$Choice))
  })
  
  # reactive getTestResults ----
  getTestResults <- reactive({
    dat <- getTestData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = dat)
    d <- data.frame(dat$Y, predictions, row.names = rownames(dat))
    colnames(d) <- c("obs", "pred")
    d
  })

  # reactive getTrainResults ----
  getTrainResults <- reactive({
    dat <- getTrainData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = dat)
    d <- data.frame(dat$Y, predictions, row.names = rownames(dat))
    colnames(d) <- c("obs", "pred")
    d
  })
  
  # output TestSummary (print)
  output$TestSummary <- renderPrint({
    if (is.na(input$Choice) || input$Choice == "") {
      cat("No model chosen")
    } else {
      caret::defaultSummary(getTestResults())
    }
  })
  
  # output TestPlot (plot) ----
  output$TestPlot <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    par(pty = "s")
    range <- range(c(d$obs, d$pred), na.rm = TRUE)
    plot(d, xlim = range, ylim = range, main = "Predicted versus Observed for test data")
    abline(a = 0, b = 1, col = c("blue"), lty = c(2), lwd = c(3))
  })
  
  # output TestResiduals (plot) ----
  output$TestResiduals <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    d$residuals <- d$obs - d$pred
    coef <- input$IqrM
    limits <- boxplot.stats(x = d$residuals, coef = coef)$stats
    label <- ifelse(d$residuals < limits[1] | d$residuals > limits[5], rownames(d), NA)
    ggplot(d, mapping = aes(y = residuals, x = 0, label = label)) +
      geom_boxplot(coef = coef, orientation = "vertical") +
      ggrepel::geom_text_repel() +
      labs(title = "Test-Residual Boxplot",  subtitle = paste(coef, "IQR Multiplier")) +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })

  # output TrainResiduals (plot) ----
  output$TrainResiduals <- renderPlot({
    d <- getTrainResults()
    req(nrow(d) > 0)
    d$residuals <- d$obs - d$pred
    coef <- input$IqrM
    limits <- boxplot.stats(x = d$residuals, coef = coef)$stats
    label <- ifelse(d$residuals < limits[1] | d$residuals > limits[5], rownames(d), NA)
    ggplot(d, mapping = aes(y = residuals, x = 0, label = label)) +
      geom_boxplot(coef = coef, orientation = "vertical") +
      ggrepel::geom_text_repel() +
      labs(title = "Train-Residual Boxplot",  subtitle = paste(coef, "IQR Multiplier")) +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })
  
    
})
