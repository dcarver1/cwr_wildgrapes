
#' create buffers 
#'
#' @param speciesPoints 
#' @param natArea 
#' @param bufferDist 
#' @param templateRast 
#'
#' @return
#' @export
#'
#' @examples
create_buffers <- function(speciesPoints, natArea, bufferDist, templateRast){
  ## select all g points from point object
  p1 <- speciesPoints %>% filter(type == "G")
    
  #clause to test for G occurrences
  if(nrow(p1)== 0){
    print("there are no g points for this species")
  }else{
    ##buffering
    buffer <- sf::st_buffer(x = p1, dist = bufferDist )

    # set extent equal to native area
    r1 <- templateRast %>%
      terra::crop(natArea) %>%
      terra::mask(natArea)
    
    ##rasterizing and matching cells to predictor layers
    buffer_rs <- terra::rasterize(vect(buffer), r1)%>%
      terra::crop(natArea)%>%
      terra::mask(natArea)
    
    return(buffer_rs)
  }
}




#' Number of background points 
#'
#' @param natArea 
#'
#' @return
#' @export
#'
#' @examples
numberBackground <- function(natArea){
  ### need to determine what the logic behind this was. 
  ### n right now is total area in meters square, convert sq km
  n <- as.numeric(sum(sf::st_area(natArea))) * 0.000001

    if( n >= 5000){
    n <- 5000
    }else{
      n <- n
    }
  return(n)
}


#' Generate modeling dataset
#'
#' @param speciesPoints 
#' @param natArea 
#' @param bioVars 
#'
#' @return
#' @export
#'
#' @examples
generateModelData <- function(speciesPoints,natArea,bioVars,b_Number){
  
  # generate background points 
  ## format species data
  sp1 <- speciesPoints %>%
    mutate("presence" = 1)%>%
    dplyr::select(presence,geometry)
  
  
  ## this will need to be adjust if species if only present in a small area 
  bg1 <- sf::st_sample(x = natArea, size = b_Number)%>%
    sf::st_as_sf()%>%
    mutate("presence" = 0)%>%
    dplyr::select(presence,"geometry" = x)%>%
    filter(!geometry %in% sp1$geometry) # test for same coordinated between presence and background data
  # bind datasets 
  d1 <- bind_rows(sp1, bg1)
  
  # extract values from rasters 
  ## I don't know if I need this a spatial object at this point, I don't thin so
  ## Also this are no reference values for what points aligns with which input location. 
  d2 <- terra::extract(x = bioVars, y = vect(d1), bind= TRUE)%>%
    st_as_sf()
  
  # convert to sf and drop NA values
  drop <- st_drop_geometry(d2)%>%
    complete.cases()
  ### might be some issues here with droping observations, but this step is a requirement. 
  d3 <- d2[drop, ]
  return(d3) 
}





#' Run Variable Selection
#'
#' @param modelData 
#'
#' @return A csv of occurance data that has been thinned to include on primary variables. 
varaibleSelection <- function(modelData){
  # subset predictor data and presence column
  varOnly <- modelData %>% 
    st_drop_geometry() %>% 
    dplyr::select(-presence)
  # remove all na from dataframe
  test2 <-complete.cases(varOnly)
  # drop all column from bioValues set as well so the same data is used for maxnet modeling.
  bioValues <- modelData %>%
    st_drop_geometry()# [test2,] %>% st_drop_geometry()
  # redefine var select to in
  varSelect <- bioValues %>% 
    dplyr::select(-presence)
  # Maximum modelled data
  #write.csv(x = bioValues, file = paste0(sp_dir, "/modeling/maxent/bioValuesForPresencePoints.csv"))
  
  
  # # #vsurf
  ### Considered altering the number of trees, 100 is somewhat low for the
  # number of predictors used. It was a time concern more then anything.
  # vsurfThres <- VSURF_thres(x=bioValues[,1:26] , y=as.factor(bioValues$presence) ,
  #                           ntree = 100 )
  ### change for 30 arc second run 
  vsurfThres <- VSURF_thres(x=bioValues[,2:27] , 
                            y=as.factor(bioValues$presence),
                            parallel = TRUE )
  ###
  #correlation matrix
  ###
  
  # define predictor list based on Run
  inputPredictors <- vsurfThres$varselect.thres
  
  # ordered predictors from our variable selection
  predictors <- varSelect[,c(inputPredictors)]
  # Calculate correlation coefficient matrix
  correlation <-cor(predictors, method="pearson")
  #change self correlation value
  
  # #define the list of top 15 predictors
  varNames <- colnames(correlation)
  # empty list containing the variables tested
  varsTested <- c()
  #loop through the top 5 predictors to remove correlated varables.
  for( i in 1:5){
    print(varNames[i])
    if(varNames[i] %in% varNames){
      # add variable to the test list
      varsTested <- c(varsTested, varNames[i])
      # Test for correlations with predictors
      vars <- correlation[(i+1):nrow(correlation),i] > 0.7 | correlation[(i+1):nrow(correlation),i] < -0.7
      # Select correlated values names
      corVar <- names(which(vars == TRUE))
      #test is any correlated variables exist
      if(length(corVar) >0 ){
        # loop through the list of correlated variables
        varNames <- varNames[!varNames  %in% corVar]
        print(paste0("the variable ", corVar, " was removed"))
      }
    }else{
      print("this variable has been removed already")
    }
  }
  
  # include all variables that were tested.
  for(p in varsTested){
    if(p %in% varNames){
    }else{
      varNames <- c(varNames, p)
    }
  }# It's a little bit confusing why variable are being dropped after they area tested. Correlation
  # should be the same in both directs. This is just a test to make sure it works.
  
  
  #create a dataframe of the top predictors and
  rankPredictors <- data.frame(matrix(nrow = length(colnames(correlation)),ncol = 3))
  rankPredictors$varNames <- colnames(correlation)
  rankPredictors$importance <- vsurfThres$imp.varselect.thres
  rankPredictors$includeInFinal <- colnames(correlation) %in% varNames
  rankPredictors <- rankPredictors[,4:6]
  # write.csv(x = rankPredictors, file = paste0(sp_dir, "/modeling/maxent/predictorImportance.csv"))
  
  # filter the input sf object based on rank order of selected variables. 
  variblesToModel <- modelData[,c("presence",varNames)]
  
  return(list(
    rankPredictors = rankPredictors,
    variblesToModel = variblesToModel
  ))
}

cropRasters <- function(natArea,bioVars,selectVars){
  vars <- selectVars$rankPredictors %>% 
    filter(includeInFinal == TRUE)%>%
    dplyr::select(varNames)%>%
    pull()
  # generate crop raster list 
  r1 <- bioVars[[names(bioVars) %in% vars]] %>%
    terra::crop(y = natArea)%>%
    mask(mask = natArea)
  
  return(r1)
} 


runMaxnet <- function(selectVars,rasterData){
  points <- selectVars$variblesToModel %>%
    dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                  lat = sf::st_coordinates(.)[,2])%>%
    st_drop_geometry()
  
  rasters <- rasterData
  
  variblesToModel <- names(rasterData)
  
  tryCatch({
    # pull out presence points
    nPresence <- points %>%
      filter(presence == 1) %>% 
      nrow()
    
    if(nPresence <= 8 & nPresence >3){
      kfold <- 3
      feat <- "lp"
    }
    if(nPresence > 8 & nPresence <= 25){
      kfold <- 6
      feat <- "lqph"
    }
    if(nPresence > 25){
      kfold <- 10
      feat <- "lqph"
    }
    
    
    # select needed raster bands
    # rastersToModel <<- bioVars %>%
    #   raster::subset(names(variblesToModel))%>%
    #   raster::crop(nativeArea)%>%
    #   raster::mask(nativeArea)
    # develop modeling data
    # bioValuesModel <- bioValues[complete.cases(bioValues),] %>%
    #   dplyr::select(names(variblesToModel)) %>%
    #   dplyr::mutate(presence = bioValues$presence,
    #                 latitude = bioValues$latitude,
    #                 longitude = bioValues$longitude)
    
    # moving the modeling process into a for loop for better intermediate step testing
    cvfolds <- modelr::crossv_kfold(points,k = kfold)

      ####
      # I'm wrapping the modeling steps into a function because there are species which
      # can not be models via all reps because when the data is spilts, specific groups
      # do not have any presences points. If you re run the cvfolds process, you get new
      # groups of data, this sometimes produces splits in the data that allow for the
      # process to run, hence we get a successful model.
    modelsteps <- function(){
      # run model within here
      cvfolds <- modelr::crossv_kfold(points,k = kfold)
      #### 20200127, trying to get a consistent test train split. I will try running the
      # models with this method and see what happen.
      
      ### 20200128, not going to work out until i figure out how to index
      # ## Create a dataframe that records the number of test and train
      # paCount <- data.frame(matrix(nrow = kfold, ncol = 5))
      # colnames(paCount) <- c("modelRun", "presenceTrain",
      #                        "backgroundTrain", "presenceTest","backgroundTest")
      # for(i in 1:kfold){
      #   paCount$modelRun[i] <- i
      #   paCount$presenceTrain[i] <- length(which(cvfolds$train[i]$data$presence == 1))
      #   paCount$backgroundTrain[i] <- length(which(cvfolds$train[i]$data$presence == 1))
      #   paCount$presenceTest[i] <- length(which(cvfolds$test[i]$data$presence == 1))
      #   paCount$backgroundTest[i] <- length(which(cvfolds$test[i]data$presence == 1))
      # }
      #
      sdm_results <- cvfolds %>%
        dplyr::mutate(.  #train sdm models using Maxnet and train data
                      , model_train = purrr::map2(.x = train, .y = .id, function(.x, .y) {
                        cat("Training MAXNET model for fold",
                            .y,
                            ", all presence points added to background \n")
                        
                        data_train <- as.data.frame(.x)
                        #select all presence and add them as background as well.
                        pres <- data_train %>%
                          filter(presence == 1)
                        pres$presence <-
                          rep(0, length(pres$presence))
                        data_train <- rbind(data_train, pres)
                        p <- data_train$presence
                        # cat(print(p),"\n")
                        data <- data_train %>% dplyr::select(all_of(variblesToModel))
                        fit.maxent <-maxnet::maxnet(p = p,
                                         data = data,
                                         #regmult = beta,
                                         f = maxnet.formula(p, data, classes = feat))
                        
                        return(fit.maxent)
                        
                      })
                      #evaluate trained model
                      , predictions_train = purrr::pmap(list(.x = model_train,
                                                             .y = .id, .z = train), function(.x, .y, .z) {
                        cat("Predicting train data for fold", .y, "\n")
                        train <-as.data.frame(.z)
                        predictions <-terra::predict(
                            object = .x,
                            newdata = train %>% dplyr::select(all_of(variblesToModel)),
                            type = "logistic"
                          )
                        dt <-data.frame(obs = factor(train$presence), pred = predictions)

                        return(dt)
                      })
                      #calculate auc for trained model
                      , AUC_train = purrr::map2(.x = predictions_train,
                                                .y = .id,
                                                function(.x, .y) {
                        cat("Calculating AUC_train for model", .y, "\n")
                        croc <-pROC::roc(response = .x$obs, predictor = .x$pred)

                        return(as.numeric(croc$auc))
                      })
                      #calculate max preformance measures (sensitivity, specificity and Treshold) using train data
                      , evaluation_train = purrr::map2(.x = predictions_train,
                                                       .y = .id, function(.x, .y) {
                        cat("Calculating optimal threshold for model", .y, "\n")
                        croc <-pROC::roc(response = .x$obs, predictor = .x$pred)
                        croc_summ <-data.frame (
                            sensi = croc$sensitivities,
                            speci = croc$specificities,
                            threshold =  croc$thresholds
                          ) %>%
                          round(., 3) %>%
                          dplyr::mutate(., max.TSS = sensi + speci - 1) %>%
                          dplyr::mutate(., minROCdist = sqrt((1 - sensi) ^
                                                               2 + (speci - 1) ^ 2))

                        max.tss <-croc_summ %>%
                          dplyr::filter(., max.TSS == max(max.TSS)) %>%
                          dplyr::mutate(., method = rep("max(TSS)", nrow(.)))

                        minRoc <-croc_summ %>%
                          dplyr::filter(., minROCdist == min(minROCdist)) %>%
                          dplyr::mutate(., method = rep("minROCdist", nrow(.)))

                        croc_summ <-                          rbind(max.tss, minRoc) %>%
                          dplyr::filter(., speci == max(speci))  %>%
                          dplyr::sample_n(., 1)

                        return(croc_summ)
                      })
                      #Make predictions using testing data
                      , predictions_test = purrr::pmap(list(.x = test,
                                                            .y = model_train, .z = .id),
                                                       function(.x, .y, .z) {
                        cat("Using test data to predict model", .z, " \n")
                        test <-as.data.frame(.x)
                        predictions <-raster::predict(
                            object = .y,
                            newdata = test %>% dplyr::select(all_of(variblesToModel)),
                            type = "logistic"
                          )
                        dt <-data.frame(obs = factor(test$presence), pred = predictions)

                        return(dt)
                      })
                      #Calculate AUC for testing
                      , AUC = map2(.x = predictions_test, .y = .id, function(.x, .y) {
                        cat("Calculating AUC for model", .y, "\n")
                        croc <- pROC::roc(response = .x$obs, predictor = .x$pred)

                        return(as.numeric(croc$auc))
                      })
                      #calculate max preformance measures (sensitivity, specificity and Treshold) using max(TSS) criterion
                      , evaluation_test = pmap(list(.x = evaluation_train, .y = .id, .z = predictions_test), function(.x, .y, .z) {
                        cat("Calculating evaluation for model", .y, "\n")

                        thr <- .x$threshold

                        a <-
                          .z %>% dplyr::filter(., pred >= thr & obs == 1) %>% nrow()
                        b <-
                          .z %>% dplyr::filter(., pred >= thr & obs == 0) %>% nrow()
                        c <-
                          .z %>% dplyr::filter(., pred < thr & obs == 1) %>% nrow()
                        d <-
                          .z %>% dplyr::filter(., pred < thr & obs == 0) %>% nrow()

                        #senitivity and specificity
                        se <- a / (a + c)
                        es <- d / (b + d)
                        #Matthews correlation coefficient
                        den <-
                          sqrt(a + b) * sqrt(a + c) * sqrt(d + b) * sqrt(d + c)
                        den <-
                          ifelse(den  != 0 , den, 1)
                        mcc <-
                          (a * d - b * c) / den
                        #Likelyhood Ratio +
                        lr_ps <-
                          se / (1 - es)
                        #Likelihood ratio -
                        lr_ne <-
                          (1 - se) / es

                        #calculate kappa index
                        pr_a <-
                          (a + d) / (a + b + c + d)
                        pr_e <-
                          (((a + b) / (a + b + c + d)) * ((a + c) / (a + b + c + d))) + (((c + d) /
                                                                                            (a + b + c + d)) * ((b + d) / (a + b + c + d)))
                        kappa <-
                          (pr_a - pr_e) / (1 - pr_e)


                        evaluation <-
                          data.frame(
                            threshold = thr,
                            sensi = se,
                            speci = es,
                            matthews.cor = mcc,
                            LR_pos = lr_ps,
                            LR_neg = lr_ne,
                            kappa_index = kappa
                          )
                        return(evaluation)
                      })
                      #Calculate nAUC using both train and test data
                      , nAUC = pmap(list(.x = train, .y = test, .z = .id), function(.x, .y, .z) {
                        cat("calculating AUC from NULL model", .z, "\n")

                        train_dt <-as.data.frame(.x) %>% dplyr::select(., presence, lon, lat)
                        test_dt  <-as.data.frame(.y) %>% dplyr::select(., presence, lon, lat)


                        train_p <-train_dt[which(train_dt$presence == 1), 2:3]
                        train_a <-train_dt[which(train_dt$presence == 0), 2:3]

                        gd <-dismo::geoDist(p = train_p, a = train_a, lonlat = TRUE)
                        pred <-
                          dismo::predict(gd, test_dt %>% dplyr::select(lon, lat))

                        nAUC <-pROC::roc(response = test_dt$presence, predictor = pred)
                        return(as.numeric(nAUC$auc))
                      })
                      #Calculate cAUC using the formula cAUC = AUC + 0.5 - max( 0.5, nAUC)
                      , cAUC = purrr::pmap(list(.x = AUC, .y = nAUC, .z = .id), function(.x, .y, .z) {
                        cat("Calculating AUC correction using NULL model", .z, " \n")
                        cAUC = .x + 0.5 - max(0.5, .y)
                        return(cAUC)
                      })
                      #Project rasters using maxnet model for mean, median and sd
                      , do.projections =  purrr::pmap(list(.x = model_train,
                                                           .y = .id,
                                                           .z = evaluation_train) , function(.x, .y, .z) {
                        cat(">>> Proyecting MAXNET model", .y, "to a raster object \n")
                        r <-raster::predict(raster::stack(rasters), .x, type = "logistic", progress = 'text')
                        # writeRaster(
                        #   r,
                        #   paste0(
                        #     sp_dir,
                        #     "/modeling/replicates/",
                        #     species,
                        #     "_prj_rep-",
                        #     .y,
                        #     ".tif"
                        #   ),
                        #   format = "GTiff",
                        #   overwrite = TRUE
                        # )
                        #thresholding raster
                        # if(!validation){
                        #   r[which(r[] < .z$threshold)] <- NA
                        # }
                        # writeRaster(
                        #   r,
                        #   paste(
                        #     sp_dir,
                        #     "/modeling/replicates/",
                        #     species,
                        #     "_prj_th_rep-",
                        #     .y,
                        #     ".tif",
                        #     sep = ""
                        #   ),
                        #   format = "GTiff",
                        #   overwrite = TRUE
                        # )
                        return(r)
                      })



      )#end mutate
      return(sdm_results)
    }
    ## set up while loop to test for itorations and successful run via the creation of
    # the sdm object.
    sdm_results <- NULL
    attempt <- 1
    while(is.null(sdm_results) && attempt <=10){
      attempt <- attempt + 1
      try(
        sdm_results <- modelsteps()
      )# end try
    }# end while loop
    # #save all results in an .rds file
    # cat("Process Done... Saving results as .rds file in the path", paste0(sp_dir, "/sdm.rds"), " \n")
    # saveRDS(sdm_results, paste0(sp_dir, "/sdm.rds"));gc()
    # 
    # cat(" ","\n")
    # cat("Maxent model finished and saved","\n")
    # cat(" ","\n")
    return(sdm_results)
  }
  # ... but if an error occurs, tell me what happened:
  , error=function(error_message) {
    message("This species encountered an error it will be added to a list to evalualte later")
    message("And below is the error message from R:")
    message(error_message)
    return(NA)
  }
  )
}

writeProjections <- function(sdm_result,directory){
  #Convert back to Terra objects. 
  prj_stk <- sdm_results %>% 
    dplyr::select(do.projections) %>%
    unlist() %>% 
    raster::stack()%>%
    rast()
  # individual model runs 
  writeRaster(prj_stk, 
              filename = paste0(directory,"/individual_runs.tif"),
              overwrite = TRUE)
  
    # Mean 
  mean(prj_stk) %>% 
    writeRaster(filename = paste0(directory,"/prj_mean.tif"),
                overwrite = TRUE)
  # Median 
  median(prj_stk) %>% 
    writeRaster(filename =  paste0(directory,"/prj_median.tif"),
                overwrite = TRUE)
  #standard devation
  terra::stdev(prj_stk)%>%
    writeRaster(filename = paste0(directory,"/prj_stdev.tif"),
                overwrite = TRUE)
  
}

