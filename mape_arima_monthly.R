## function that cross-validates monthly data specified by an arima order and outputs MAPE of p-ahead forecasts and MAPE of entire rolling LOOCV forecasts.

MAPE_ARIMA <- function(data, train.set, test.set, model, p, arima.method) {
      n <- length(test.set) - p + 1 # set number of loops
      my_order <- arimaorder(model) # model arima order
      fcst <- matrix(0, nrow = n, ncol = p) # empty matrix
      test_end_floor <- as.numeric(floor(time(tail(train.set,1)))) # floor of most recent year
      test_end_actual <- as.numeric(time(tail(train.set,1)))       # actual year
      for(i in 1:n) {
            w <- window(data, end = test_end_floor + if(test_end_actual == test_end_floor) {
                  (i-1)/12                                               # get window reflecting correct month
            } else {
                  if(test_end_actual == test_end_floor + (1/12)) {
                        i/12
                  } else {
                        if(test_end_actual == test_end_floor + (2/12)) {
                              (i+1)/12
                        } else {
                              if(test_end_actual == test_end_floor + (3/12)) {
                                    (i+2)/12
                              } else {
                                    if(test_end_actual == test_end_floor + (4/12)) {
                                          (i+3)/12
                                    } else {
                                          if(test_end_actual == test_end_floor + (5/12)) {
                                                (i+4)/12
                                          } else {
                                                if(test_end_actual == test_end_floor + (6/12)) {
                                                      (i+5)/12
                                                } else {
                                                      if(test_end_actual == test_end_floor + (7/12)) {
                                                            (i+6)/12
                                                      } else {
                                                            if(test_end_actual == test_end_floor + (8/12)) {
                                                                  (i+7)/12
                                                            } else {
                                                                  if(test_end_actual == test_end_floor + (9/12)) {
                                                                        (i+8)/12
                                                                  } else {
                                                                        if(test_end_actual == test_end_floor + (10/12)) {
                                                                              (i+9)/12
                                                                        } else {
                                                                              if(test_end_actual == test_end_floor + (11/12)) {
                                                                                    (i+10)/12
                                                                              } else {
                                                                                    stop("Time lengths incompatible")
                                                                              }
                                                                        }
                                                                  }
                                                            }
                                                      }
                                                }
                                          }
                                    }
                              }
                        }
                  }
            })
            mod <- arima(w, order = my_order[1:3], seasonal = my_order[4:6], method = arima.method) # create arima model
            fcst[i,] <- forecast(mod, h = p)$mean # get point forecasts of p periods ahead
      }
      perc_errors <- matrix(0, nrow = n, ncol = p) # empty matrix
      for(i in 1:n) {
            perc_errors[i,] <- (test.set[i:(i+p-1)] - fcst[i,])/test.set[i:(i+p-1)] # calculate error matrix given testing data set
      }
      MAPE_mod <- mean(abs(perc_errors))*100 # calculate mean average percentage error (MAPE) for all forecasts
      MAPE_p <- mean(abs(perc_errors[,p]))*100 # calculate MAPE for all p-ahead forecasts
      assign("MAPE_mod", MAPE_mod, envir = .GlobalEnv) # create object containing overall MAPE
      assign("MAPE_p", MAPE_p, envir = .GlobalEnv) # create object containing p-ahead MAPE
}
