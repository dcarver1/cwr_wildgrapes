#' Run Maxnet Model
#'
#' @param selectVars
#' @param rasterData
#'
#' @return : A complex list object with a variety of results and evaluation metrics
runMaxnet <- function(selectVars, rasterData) {
  points <- selectVars$variblesToModel |>
    as.data.frame() |>
    mutate(geometry = gsub("c\\(|\\)", "", geometry)) %>%
    separate(
      col = geometry,
      into = c("lon", "lat"),
      sep = ",\\s*",
      convert = TRUE
    )
  rasters <- rasterData
  beta <- 1

  variblesToModel <- names(rasterData)
  tryCatch(
    {
      # pull out presence points
      nPresence <- points %>%
        filter(presence == 1) %>%
        nrow()
      # predefine to capture feautes with 3 or less
      kfold <- NA
      if (nPresence <= 12 & nPresence >= 8) {
        kfold <- 3
        feat <- "lp"
      }
      if (nPresence > 12 & nPresence <= 25) {
        kfold <- 6
        feat <- "lqph"
      }
      if (nPresence > 25) {
        kfold <- 10
        feat <- "lqph"
      }

      # setting kfold to 6 for big models to handle the memory allocation
      # kfold <- 6

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
      if (is.na(kfold)) {
        sdm_results <- NULL
        return(sdm_results)
        stop()
      }
      # moving the modeling process into a for loop for better intermediate step testing
      # cvfolds <- modelr::crossv_kfold(points,k = kfold)

      ####
      # I'm wrapping the modeling steps into a function because there are species which
      # can not be models via all reps because when the data is spilts, specific groups
      # do not have any presences points. If you re run the cvfolds process, you get new
      # groups of data, this sometimes produces splits in the data that allow for the
      # process to run, hence we get a successful model.
      modelsteps <- function() {
        # run model within here
        cvfolds <- modelr::crossv_kfold(points, k = kfold)
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

        # fit the model using the automatically established feature set
        sdm_results <- try(
          cvfolds %>%
            dplyr::mutate(
              ., #train sdm models using Maxnet and train data
              model_train = purrr::map2(.x = train, .y = .id, function(.x, .y) {
                cat(
                  "Training MAXNET model for fold",
                  .y,
                  ", all presence points added to background \n"
                )

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
                fit.maxent <- maxnet::maxnet(
                  p = p,
                  data = data,
                  regmult = beta,
                  f = maxnet.formula(p, data, classes = feat)
                )

                return(fit.maxent)
              })
            )
        )

        # Test to see if the inital feat setting worked.  -------------------------
        if (class(sdm_results)[1] == "try-error") {
          feat <- "lp"
          sdm_results <- try(
            cvfolds %>%
              dplyr::mutate(
                ., #train sdm models using Maxnet and train data
                model_train = purrr::map2(
                  .x = train,
                  .y = .id,
                  function(.x, .y) {
                    cat(
                      "Training MAXNET model for fold",
                      .y,
                      ", all presence points added to background \n"
                    )

                    data_train <- as.data.frame(.x)
                    #select all presence and add them as background as well.
                    pres <- data_train %>%
                      filter(presence == 1)
                    pres$presence <-
                      rep(0, length(pres$presence))
                    data_train <- rbind(data_train, pres)
                    p <- data_train$presence
                    # cat(print(p),"\n")
                    data <- data_train %>%
                      dplyr::select(all_of(variblesToModel))
                    fit.maxent <- maxnet::maxnet(
                      p = p,
                      data = data,
                      regmult = beta,
                      f = maxnet.formula(p, data, classes = feat)
                    )

                    return(fit.maxent)
                  }
                )
              )
          )
        }

        # final test for feat setting ---------------------------------------------
        if (class(sdm_results)[1] == "try-error") {
          print("yes")
          sdm_results <- NA
          return(sdm_results)
        }

        sdm_results2 <- sdm_results |>
          dplyr::mutate(
            #evaluate trained model
            predictions_train = purrr::pmap(
              list(.x = model_train, .y = .id, .z = train),
              function(.x, .y, .z) {
                cat("Predicting train data for fold", .y, "\n")
                train <- as.data.frame(.z)
                predictions <- terra::predict(
                  object = .x,
                  newdata = train %>% dplyr::select(all_of(variblesToModel)),
                  type = "logistic"
                )
                dt <- data.frame(
                  obs = factor(train$presence),
                  pred = predictions
                )

                return(dt)
              }
            ),
            #calculate auc for trained model
            AUC_train = purrr::map2(
              .x = predictions_train,
              .y = .id,
              function(.x, .y) {
                cat("Calculating AUC_train for model", .y, "\n")
                croc <- pROC::roc(response = .x$obs, predictor = .x$pred)

                return(as.numeric(croc$auc))
              }
            ),
            #calculate max preformance measures (sensitivity, specificity and Treshold) using train data
            evaluation_train = purrr::map2(
              .x = predictions_train,
              .y = .id,
              function(.x, .y) {
                cat("Calculating optimal threshold for model", .y, "\n")
                croc <- pROC::roc(response = .x$obs, predictor = .x$pred)
                croc_summ <- data.frame(
                  sensi = croc$sensitivities,
                  speci = croc$specificities,
                  threshold = croc$thresholds
                ) %>%
                  round(., 3) %>%
                  dplyr::mutate(., max.TSS = sensi + speci - 1) %>%
                  dplyr::mutate(
                    .,
                    minROCdist = sqrt((1 - sensi)^2 + (speci - 1)^2)
                  )

                max.tss <- croc_summ %>%
                  dplyr::filter(., max.TSS == max(max.TSS)) %>%
                  dplyr::mutate(., method = rep("max(TSS)", nrow(.)))

                minRoc <- croc_summ %>%
                  dplyr::filter(., minROCdist == min(minROCdist)) %>%
                  dplyr::mutate(., method = rep("minROCdist", nrow(.)))

                croc_summ <- rbind(max.tss, minRoc) %>%
                  dplyr::filter(., speci == max(speci)) %>%
                  dplyr::sample_n(., 1)

                return(croc_summ)
              }
            ),
            #Make predictions using testing data
            predictions_test = purrr::pmap(
              list(.x = test, .y = model_train, .z = .id),
              function(.x, .y, .z) {
                cat("Using test data to predict model", .z, " \n")
                test <- as.data.frame(.x)
                predictions <- raster::predict(
                  object = .y,
                  newdata = test %>% dplyr::select(all_of(variblesToModel)),
                  type = "logistic"
                )
                dt <- data.frame(
                  obs = factor(test$presence),
                  pred = predictions
                )

                return(dt)
              }
            ),
            #Calculate AUC for testing
            AUC = map2(.x = predictions_test, .y = .id, function(.x, .y) {
              cat("Calculating AUC for model", .y, "\n")
              croc <- pROC::roc(response = .x$obs, predictor = .x$pred)

              return(as.numeric(croc$auc))
            }),
            #calculate max preformance measures (sensitivity, specificity and Treshold) using max(TSS) criterion
            evaluation_test = pmap(
              list(.x = evaluation_train, .y = .id, .z = predictions_test),
              function(.x, .y, .z) {
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
                  ifelse(den != 0, den, 1)
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
                  (((a + b) / (a + b + c + d)) * ((a + c) / (a + b + c + d))) +
                  (((c + d) /
                    (a + b + c + d)) *
                    ((b + d) / (a + b + c + d)))
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
              }
            ),
            #Calculate nAUC using both train and test data
            nAUC = pmap(
              list(.x = train, .y = test, .z = .id),
              function(.x, .y, .z) {
                cat("calculating AUC from NULL model", .z, "\n")

                train_dt <- as.data.frame(.x) %>%
                  dplyr::select(., presence, lon, lat)
                test_dt <- as.data.frame(.y) %>%
                  dplyr::select(., presence, lon, lat)

                train_p <- train_dt[which(train_dt$presence == 1), 2:3]
                train_a <- train_dt[which(train_dt$presence == 0), 2:3]

                gd <- dismo::geoDist(p = train_p, a = train_a, lonlat = TRUE)
                pred <-
                  dismo::predict(gd, test_dt %>% dplyr::select(lon, lat))

                nAUC <- pROC::roc(response = test_dt$presence, predictor = pred)
                return(as.numeric(nAUC$auc))
              }
            ),
            #Calculate cAUC using the formula cAUC = AUC + 0.5 - max( 0.5, nAUC)
            cAUC = purrr::pmap(
              list(.x = AUC, .y = nAUC, .z = .id),
              function(.x, .y, .z) {
                cat("Calculating AUC correction using NULL model", .z, " \n")
                cAUC = .x + 0.5 - max(0.5, .y)
                return(cAUC)
              }
            ),
            #Project rasters using maxnet model for mean, median and sd
            do.projections = purrr::pmap(
              list(.x = model_train, .y = .id, .z = evaluation_train),
              function(.x, .y, .z) {
                cat(">>> Proyecting MAXNET model", .y, "to a raster object \n")
                r <- raster::predict(
                  raster::stack(rasters),
                  .x,
                  type = "logistic",
                  progress = 'text'
                )
                return(r)
              }
            )
          ) #end mutate
        return(sdm_results2)
      }
      ## set up while loop to test for itorations and successful run via the creation of
      # the sdm object.
      sdm_results <- NULL
      attempt <- 1
      while (is.null(sdm_results) && attempt <= 10) {
        attempt <- attempt + 1
        try(
          sdm_results <- modelsteps()
        ) # end try
      } # end while loop

      return(sdm_results)
    },
    # ... but if an error occurs, tell me what happened:
    error = function(error_message) {
      message(
        "This species encountered an error it will be added to a list to evalualte later"
      )
      message("And below is the error message from R:")
      message(error_message)
      return(NA)
    }
  )
}
