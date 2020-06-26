#' Linear model fitting with optional best subset selection, recursive feature selection or forward future selection
#'
#' This function allows you to fit a model between
#' dependend and independend variables and calculate various selection routines.
#' @param predictors a data.frame with the predictor variables as columns
#' @param response a vector with the response variable
#' @param method string, type of statistical model for more details see caret::train()
#' @param ctrl trainControl object
#' @param subset string, subset selection routine that should be carried out.
#' One of full, best, rfe or ffs
#' @param preProc string or vector, pre processing routines that should be applied
#' to predictors
#' @param metric string, performance metric that is used to select the best model,
#' defaults to RMSE for numeric and Accuracy for factor response
#' @param maximize string, should metric be maximized?
#' @param tuneLength numeric, see caret::train() for more details
#' @param tuneGrid see caret::train() for more details
#' @param withinSE Logical Models are only selected if they are better than the
#' currently best models Standard error
#' @param minVar Numeric. Number of variables to combine for the first selection
#' @param ... further arguments passed on to caret::train()
#' @keywords bioclimatic index, linear model, subset selection
#' @export

rg_fit_model <- function(predictors,
                         response,
                         method = 'lm',
                         ctrl = caret::trainControl(),
                         subset = 'full',
                         preProc = c('center', 'scale'),
                         metric = ifelse(is.factor(response), "Accuracy", "RMSE"),
                         maximize = ifelse(metric == "RMSE", FALSE, TRUE),
                         tuneLength = 3,
                         tuneGrid = NULL,
                         verbose = T,
                         withinSE = FALSE,
                         minVar = 2,
                         ...) {

  ffs <- function (predictors,
                   response,
                   seed = sample(1:1000, 1),
                   trControl = ctrl,
                   ...){

    trControl$returnResamp <- "final"

    if(class(response) == "character"){
      response <- factor(response)

      if(metric == "RMSE"){
        metric <- "Accuracy"
        maximize <- TRUE
      }

    }

    if (trControl$method == "LOOCV"){

      if (withinSE==TRUE){
        print("warning: withinSE is set to FALSE as no SE can be calculated using method LOOCV")
        withinSE <- FALSE
      }

    }

    se <- function(x){sd(x, na.rm = TRUE)/sqrt(length(na.exclude(x)))}

    n <- length(names(predictors))
    acc <- 0

    perf_all <- data.frame(matrix(ncol=length(predictors)+3,
                                  nrow=factorial(n) / (factorial(n-minVar)* factorial(minVar))+
                                    (n-minVar)*(n-minVar+1)/2))
    names(perf_all) <- c(paste0("var",1:length(predictors)),metric,"SE","nvar")

    if(maximize) evalfunc <- function(x){max(x,na.rm=TRUE)}
    if(!maximize) evalfunc <- function(x){min(x,na.rm=TRUE)}
    isBetter <- function (actmodelperf,bestmodelperf,
                          bestmodelperfSE=NULL,
                          maximization=FALSE,
                          withinSE=FALSE){
      if(withinSE){
        result <- ifelse (!maximization, actmodelperf < bestmodelperf-bestmodelperfSE,
                          actmodelperf > bestmodelperf+bestmodelperfSE)
      }else{
        result <- ifelse (!maximization, actmodelperf < bestmodelperf,
                          actmodelperf > bestmodelperf)
      }
      return(result)
    }

    #### chose initial best model from all combinations of two variables
    minGrid <- t(data.frame(combn(names(predictors),minVar)))
    for (i in 1:nrow(minGrid)){
      if (verbose){
        print(paste0("model using ",paste0(minGrid[i,],collapse=","), " will be trained now..." ))
      }
      set.seed(seed)
      #adaptations for pls:
      tuneGrid_orig <- tuneGrid
      if(method=="pls"&!is.null(tuneGrid)&any(tuneGrid$ncomp>minVar)){
        tuneGrid <- data.frame(ncomp=tuneGrid[tuneGrid$ncomp<=minVar,])
        if(verbose){
          print(paste0("note: maximum ncomp is ", minVar))
        }
      }
      #train model:
      model <- caret::train(predictors[,minGrid[i,]],
                            response,
                            method=method,
                            trControl=trControl,
                            tuneLength = tuneLength,
                            tuneGrid = tuneGrid,
                            ...)
      if(method=="pls"&!is.null(tuneGrid)&any(tuneGrid$ncomp>minVar)){
        tuneGrid <- tuneGrid_orig
      }

      ### compare the model with the currently best model
      actmodelperf <- evalfunc(model$results[,names(model$results)==metric])
      actmodelperfSE <- se(
        sapply(unique(model$resample$Resample),
               FUN=function(x){mean(model$resample[model$resample$Resample==x,
                                                   metric],na.rm=TRUE)}))
      if (i == 1){
        bestmodelperf <- actmodelperf
        bestmodelperfSE <- actmodelperfSE
        bestmodel <- model
      } else{
        if (isBetter(actmodelperf,bestmodelperf,maximization=maximize,withinSE=FALSE)){
          bestmodelperf <- actmodelperf
          bestmodelperfSE <- actmodelperfSE
          bestmodel <- model
        }
      }
      acc <- acc+1
      perf_all[acc,1:length(model$finalModel$xNames)] <- model$finalModel$xNames
      perf_all[acc,(length(predictors)+1):ncol(perf_all)] <- c(actmodelperf,actmodelperfSE,length(model$finalModel$xNames))
      if(verbose){
        print(paste0("maximum number of models that still need to be trained: ",
                     round(factorial(n) / (factorial(n-minVar)* factorial(minVar))+
                             (n-minVar)*(n-minVar+1)/2-acc,0)))
      }
    }
    #### increase the number of predictors by one (try all combinations)
    #and test if model performance increases
    selectedvars <- names(bestmodel$trainingData)[-which(
      names(bestmodel$trainingData)==".outcome")]
    if (maximize){
      selectedvars_perf <- max(bestmodel$results[,metric])
    } else{
      selectedvars_perf <- min(bestmodel$results[,metric])
    }
    selectedvars_SE <- bestmodelperfSE
    if(verbose){
      print(paste0(paste0("vars selected: ",paste(selectedvars, collapse = ',')),
                   " with ",metric," ",round(selectedvars_perf,3)))
    }
    for (k in 1:(length(names(predictors))-minVar)){
      startvars <- names(bestmodel$trainingData)[-which(
        names(bestmodel$trainingData)==".outcome")]
      nextvars <- names(predictors)[-which(
        names(predictors)%in%startvars)]
      if (length(startvars)<(k+(minVar-1))){
        if(verbose) {
        message(paste0("Note: No increase in performance found using more than ",
                       length(startvars), " variables"))
        }
        bestmodel$selectedvars <- selectedvars
        bestmodel$selectedvars_perf <- selectedvars_perf[-length(selectedvars_perf)]
        bestmodel$selectedvars_perf_SE <- selectedvars_SE[-length(selectedvars_SE)] #!!!
        bestmodel$perf_all <- perf_all
        bestmodel$perf_all <- bestmodel$perf_all[!apply(is.na(bestmodel$perf_all), 1, all),]
        bestmodel$perf_all <- bestmodel$perf_all[colSums(!is.na(bestmodel$perf_all)) > 0]
        bestmodel$minVar <- minVar
        bestmodel$type <- "ffs"
        return(bestmodel)
        break()
      }
      for (i in 1:length(nextvars)){
        if(verbose){
          print(paste0("model using additional variable ",nextvars[i], " will be trained now..." ))
        }
        set.seed(seed)
        #adaptation for pls:
        tuneGrid_orig <- tuneGrid
        if(method=="pls"&!is.null(tuneGrid)&any(tuneGrid$ncomp>ncol(predictors[,c(startvars,nextvars[i])]))){
          tuneGrid<- data.frame(ncomp=tuneGrid[tuneGrid$ncomp<=ncol(predictors[,c(startvars,nextvars[i])]),])
          if(verbose){
            print(paste0("note: maximum ncomp is ", ncol(predictors[,c(startvars,nextvars[i])])))
          }}

        model <- caret::train(predictors[,c(startvars,nextvars[i])],
                              response,
                              method = method,
                              metric=metric,
                              trControl = trControl,
                              tuneLength = tuneLength,
                              tuneGrid = tuneGrid,
                              ...)
        tuneGrid <- tuneGrid_orig
        actmodelperf <- evalfunc(model$results[,names(model$results)==metric])
        actmodelperfSE <- se(
          sapply(unique(model$resample$Resample),
                 FUN=function(x){mean(model$resample[model$resample$Resample==x,
                                                     metric],na.rm=TRUE)}))
        if(isBetter(actmodelperf,bestmodelperf,
                    selectedvars_SE[length(selectedvars_SE)], #SE from model with nvar-1
                    maximization=maximize,withinSE=withinSE)){
          bestmodelperf <- actmodelperf
          bestmodelperfSE <- actmodelperfSE
          bestmodel <- model
        }
        acc <- acc+1
        perf_all[acc,1:length(model$finalModel$xNames)] <- model$finalModel$xNames
        perf_all[acc,(length(predictors)+1):ncol(
          perf_all)] <- c(actmodelperf,actmodelperfSE,length(model$finalModel$xNames))
        if(verbose){
          print(paste0("maximum number of models that still need to be trained: ",
                       round(factorial(n) / (factorial(n-minVar)* factorial(minVar))+
                               (n-minVar)*(n-minVar+1)/2-acc,0)))
        }
      }
      selectedvars <- c(selectedvars,names(bestmodel$trainingData)[-which(
        names(bestmodel$trainingData)%in%c(".outcome",selectedvars))])
      selectedvars_SE <- c(selectedvars_SE,bestmodelperfSE)
      if (maximize){
        selectedvars_perf <- c(selectedvars_perf,max(bestmodel$results[,metric]))
        if(verbose){
          print(paste0(paste0("vars selected: ",paste(selectedvars, collapse = ',')),
                       " with ", metric," ",round(max(bestmodel$results[,metric]),3)))
        }
      }
      if (!maximize){
        selectedvars_perf <- c(selectedvars_perf,min(bestmodel$results[,metric]))
        if(verbose){
          print(paste0(paste0("vars selected: ",paste(selectedvars, collapse = ',')),
                       " with ",metric," ",round(min(bestmodel$results[,metric]),3)))
        }
      }
    }
    bestmodel$selectedvars <- selectedvars
    bestmodel$selectedvars_perf <- selectedvars_perf
    bestmodel$selectedvars_perf_SE <- selectedvars_SE
    bestmodel$perf_all <- perf_all
    bestmodel$perf_all <- bestmodel$perf_all[!apply(is.na(bestmodel$perf_all), 1, all),]
    bestmodel$minVar <- minVar
    bestmodel$type <- "ffs"
    bestmodel$perf_all <- bestmodel$perf_all[colSums(!is.na(bestmodel$perf_all)) > 0]
    return(bestmodel)
  }

  x <- as.data.frame(predictors)
  y <- response

  if (any(sapply(predictors, class) == 'factor') & method == 'lm') {

    print('Linear model with factor as predictor working??')

  }


  if (all(ctrl == 'none')) ctrl <- caret::trainControl(method = 'none')

  if (subset == 'full') {

    var_comb <- names(x)

    if (ctrl$method == 'none' & method == 'lm') {

      df <- cbind(predictors, response)
      frml <- stringr::str_c('response', '~',
                             stringr::str_c(colnames(predictors), collapse = '+'))

      model <- lm(frml, data = df)

    } else {

      if (stringr::str_detect(method, 'svm')) {

        df <- cbind(predictors, response)
        frml <- response ~ .

        model <- caret::train (form = frml, data = df,
                               method = method,
                               trControl = ctrl,
                               tuneLength = tuneLength,
                               tuneGrid = tuneGrid,
                               preProcess = preProc,
                               ...)

      } else {

        model <- caret::train (x = x,
                               y = y,
                               method = method,
                               trControl = ctrl,
                               tuneLength = tuneLength,
                               tuneGrid = tuneGrid,
                               preProcess = preProc,
                               ...)

      }


    }

  } else if (subset == 'best') {

    stopifnot(ctrl != 'none')

    var_comb <- purrr::map(1:length(names(x)), combn, x = names(x), simplify = F)
    var_comb <- purrr::flatten(var_comb)

    x_sub_list <- purrr::map(var_comb, function(predictors) {x %>% dplyr::select(predictors)})

    models_list <- purrr::map(x_sub_list, caret::train,
                              y = y,
                              method = method,
                              trControl = ctrl,
                              tuneLength = tuneLength,
                              tuneGrid = tuneGrid,
                              preProcess = preProc,
                              ...)

    models_mae <- purrr::map_dbl(models_list, function(x) x$results$MAE)

    model <- models_list[[which(models_mae == min(models_mae))]]

  } else if (subset == 'rfe') {

    stopifnot(ctrl != 'none')
    stopifnot(method %in% c('lm', 'gam', 'rf'))

    params <- switch (method,
                      'lm' = list('lm', caret::lmFuncs),
                      'gam' = list('gam', caret::gamFuncs),
                      'rf' = list('rf', caret::rfFuncs)
    )

    ctrl_rfe <- caret::rfeControl(functions = params[[2]],
                                  method = ctrl$method,
                                  number = ctrl$number,
                                  repeats = ctrl$repeats)

    if (length(preProc) > 0) {

      x_preProc_fit <- caret::preProcess(x, method = preProc)
      x_rfe <- predict(x_preProc_fit, x)

    } else {

      x_rfe <- x

    }

    model_rfe <- caret::rfe(x = x_rfe,
                            y = y,
                            sizes = 1:length(names(x_rfe)),
                            rfeControl = ctrl_rfe)

    model <- model_rfe$fit

  } else if (subset == 'ffs')  {

    stopifnot(ctrl != 'none')

    model <- ffs(predictors = x, response = y, ...)
  }

  verbose_str <- switch(subset,
                        'full' = ifelse(inherits(model, 'train'),
                                       stringr::str_c('MAE: ', round(min(model$results$MAE), 2)),
                                       stringr::str_c('R2: ', round(summary(model)$r.squared, 2))),
                        'best' = str_c('MAE: ', round(min(model$results$MAE), 2)),
                        'ffs' = str_c('MAE: ', round(min(model$results$MAE), 2)),
                        'rfe' = str_c('R2: ', round(summary(model)$r.squared, 2)))

  if (verbose) {
    print(stringr::str_c(method, ' model with ', subset, ' subset ', verbose_str))
  }
  return(model)

}

