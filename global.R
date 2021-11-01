library(shiny)
library(shinyBS) # Additional Bootstrap Controls (tooltips)
library(DT)
library(corrgram)
library(visdat)
library(shinycssloaders) # busy spinner
library(recipes) # The recipes package is central to preprocessing
library(doParallel) # We employ a form of parallelism that works for MAC/Windows/Ubuntu
library(caret) # This code implements the CARET framework: see http://topepo.github.io/caret/index.html for details
library(rlang)
if (!library("mixOmics", quietly = TRUE, logical.return = TRUE)) {
  library(devtools)
  install_github("mixOmicsTeam/mixOmics")  # for some reason this has been withdrawn from CRAN
  library("mixOmics")
}


glmnet_initial <- c("naomit", "month", "dummy") # <-- These are arbitrary starting values. Set these to your best recommendation
pls_initial <- c("impute_knn","dow", "dummy")   # <-- These are arbitrary starting values. Set these to your best recommendation
rpart_initial <- c("dow", "month") # <-- These are arbitrary starting values. Set these to your best recommendation
ridge_initial <- c("impute_knn","dow", "center", "scale", "dummy")   # <-- These are arbitrary starting values. Set these to your best recommendation
ppr_initial <- c("impute_bag","dow", "center", "scale", "pls", "dummy")   # <-- These are arbitrary starting values. Set these to your best recommendation
rqnc_initial <- c("impute_knn","dow", "center", "scale", "dummy")   # <-- These are arbitrary starting values. Set these to your best recommendation
spikeslab_initial <- c("impute_knn","dow", "center", "scale", "dummy")   # <-- These are arbitrary starting values. Set these to your best recommendation
leapForward_initial <- c("impute_knn","dow", "center", "scale",  "dummy")   # <-- These are arbitrary starting values. Set these to your best recommendation
leapBackward_initial <- c("impute_knn","dow", "center", "scale", "dummy")   # <-- These are arbitrary starting values. Set these to your best recommendation
leapSeq_initial <- c("impute_knn","dow","center", "scale", "dummy")   # <-- These are arbitrary starting values. Set these to your best recommendation
lm_initial <- c("impute_knn","dow", "center", "scale", "dummy")   # <-- These are arbitrary starting values. Set these to your best recommendation
rlm_initial <- c("impute_knn","dow", "center", "scale", "dummy")   # <-- These are arbitrary starting values. Set these to your best recommendation
lmStepAIC_initial <- c("impute_knn","dow", "center", "scale", "dummy")   # <-- These are arbitrary starting values. Set these to your best recommendation


startMode <- function(Parallel = TRUE) {
  if (Parallel) {
    clus <- makeCluster(min(c(3,detectCores(all.tests = FALSE, logical = TRUE))))
    registerDoParallel(clus)
    clus
  } else {
    NULL
  }
}

stopMode <- function(clus) {
  if (!is.null(clus)) {
    stopCluster(clus)
    registerDoSEQ()
  }
}

ppchoices <- c("impute_knn", "impute_bag", "impute_median", "impute_mode", "YeoJohnson", "naomit", "pca", "pls", "ica", "center", "scale", "month", "dow", "dateDecimal", "nzv", "zv","other", "dummy","indicate_na")

# This function turns the method's selected preprocessing into a recipe that honours the same order. 
# You are allowed to add more recipe steps to this.
dynamicSteps <- function(recipe, preprocess) {
  for (s in preprocess) {
    if (s == "impute_knn") {
      recipe <- step_impute_knn(recipe, all_numeric_predictors(), all_nominal_predictors(), neighbors = 5) # 5 is a reasonable guess
    } else if (s == "impute_bag") {
      recipe <- step_impute_bag(recipe, all_numeric_predictors(), all_nominal_predictors(), trees = 25)
    } else if (s == "impute_median") {
      recipe <- step_impute_median(recipe, all_numeric_predictors())
    } else if (s == "impute_mode") {
      recipe <- recipes::step_impute_mode(recipe, all_nominal_predictors())
    } else if (s == "YeoJohnson") {
      recipe <- recipes::step_YeoJohnson(recipe, all_numeric_predictors())
    } else if (s == "naomit") {
      recipe <- recipes::step_naomit(recipe, all_predictors(), skip = TRUE)
    } else if (s == "pca") {
      recipe <- recipes::step_pca(recipe, all_numeric_predictors(), num_comp = 25)
    } else if (s == "pls") {
      recipe <- recipes::step_pls(recipe, all_numeric_predictors(), outcome = "Y", num_comp = 25)
    } else if (s == "ica") {
      recipe <- recipes::step_ica(recipe, all_numeric_predictors(), num_comp = 25)
    } else if (s == "center") {
      recipe <- recipes::step_center(recipe, all_numeric_predictors())
    } else if (s == "scale") {
      recipe <- recipes::step_scale(recipe, all_numeric_predictors())
    } else if (s == "month") {
      recipe <- recipes::step_date(recipe, has_type("date"), features = c("month"), ordinal = FALSE)
    } else if (s == "dow") {
      recipe <- recipes::step_date(recipe, has_type("date"), features = c("dow"), ordinal = FALSE)
    } else if (s == "dateDecimal") {
      recipe <- recipes::step_date(recipe, has_type("date"), features = c("decimal"), ordinal = FALSE)
    } else if (s == "zv") {
      recipe <- recipes::step_zv(recipe, all_predictors())
    } else if (s == "nzv") {
      recipe <- recipes::step_nzv(recipe, all_predictors(), freq_cut = 95/5, unique_cut = 10)
    } else if (s == "other") {
      recipe <- recipes::step_other(recipe, all_nominal_predictors())
    } else if (s == "dummy") {
      recipe <- recipes::step_dummy(recipe, all_nominal_predictors(), one_hot = FALSE)
    } else if (s == "poly") {
      recipe <- recipes::step_poly(recipe, all_numeric_predictors(), degree = 2)
    } else if (s == "indicate_na") {
      recipe <- recipes::step_indicate_na(recipe, all_predictors()) #shadow variables (this needs to precede dealing with NA)
    } else if (s == "rm") {
      # intentionally blank
    } else {
      stop(paste("Attempting to use an unknown recipe step:", s))
    }
  }
  recipe
}

description <- function(name) {
  regexName <- paste0("^", name, "$") # force an regular expression exact match
  mlist <- caret::getModelInfo(model = regexName)[[name]]
  line1 <- paste0("Method \"", name, "\" is able to do ", paste(collapse = " and ", mlist$type), ".")
  line2 <- paste0("It uses parameters: ", paste0(collapse = ", ", mlist$parameters$parameter), ".")
  line3 <- paste0("Its characteristics are: ", paste0(collapse = ", ", mlist$tags))
  paste(sep = "\n", line1, line2, line3)
}

deleteRds <- function(name) {
  ok <- TRUE
  file <- paste0(".", .Platform$file.sep, "SavedModels", .Platform$file.sep, name, ".rds")
  if (file.exists(file)) {
    ok <- unlink(file, force = TRUE)
  }
  ok
}

# strip off the global environment to save file size
clearEnvironment <- function(object) {
  rapply(object, classes = "ANY", how = "replace", function(o) {
    if (!is.null(attr(o, ".Environment"))) {
      attr(o, ".Environment") <- NULL
    }
    o
  })
  object
}

# attempts to keep the model file size small by not saving the global environment with each model
saveToRds <- function(model, name) {
  file <- paste0(".", .Platform$file.sep, "SavedModels", .Platform$file.sep, name, ".rds")
  model <- clearEnvironment(rlang::duplicate(model, shallow = FALSE))
  #cat("size = ", length(serialize(model, NULL)) )
  saveRDS(model, file)
}
