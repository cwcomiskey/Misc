function (formula, data, weights = NULL, subset = NULL, no.SE = 0, 
          control = rpart::rpart.control(cp = 0.001)) 
{
  if (missing(data)) {
    y <- eval(formula[[2]])
    predictors <- formula[[3]]
    if (!inherits(y, "Surv") | length(as.list(formula[[2]])) != 
        4) {
      stop(" Response must be a 'survival' object with 'Surv(time1, time2, event)' ")
    }
    Status <- y[, 3L]
    Times <- y[, 2L]
    unique.times <- sort(unique(Times[Status == 1]))
    temp <- survival::coxph(y ~ 1)
    cumhaz.table <- survival::basehaz(temp)
    if (sum(is.infinite(cumhaz.table$hazard)) != 0) {
      cumhaz.table <- cumhaz.table[cumhaz.table$hazard != 
                                     Inf, ]
    }
    cumhaz.table2 <- cumhaz.table[cumhaz.table$time %in% 
                                    unique.times, ]
    cumhaz.times <- c(0, cumhaz.table2$time[-length(cumhaz.table2$time)], 
                      max(Times))
    cumhaz <- c(0, cumhaz.table2$hazard)
    Start.cumhaz <- stats::approx(cumhaz.times, cumhaz, y[, 
                                                          1L])$y
    End.cumhaz <- stats::approx(cumhaz.times, cumhaz, y[, 
                                                        2L])$y
    Newtime <- End.cumhaz - Start.cumhaz
    Formula = formula(paste(c(paste("cbind(Newtime,", "Status)", 
                                    sep = ""), predictors), collapse = "~"))
    Fit.tree <- rpart::rpart(formula = Formula, method = "poisson", 
                             weights = weights, subset = subset, control = control)
    if (length(unique(Fit.tree$where)) != 1) {
      cventry <- which.min(Fit.tree$cptable[, "xerror"])
      if (no.SE == 0) {
        cpcv <- Fit.tree$cptable[cventry, "CP"]
        Fit.tree <- rpart::prune(Fit.tree, cp = cpcv)
      }
      else {
        xerrorcv <- Fit.tree$cptable[cventry, "xerror"]
        sexerrorcv <- xerrorcv + Fit.tree$cptable[cventry, 
                                                  "xstd"] * no.SE
        cpcvse <- Fit.tree$cptable[which.max(Fit.tree$cptable[, 
                                                              "xerror"] <= sexerrorcv), "CP"]
        Fit.tree <- rpart::prune(Fit.tree, cp = cpcvse)
      }
    }
    return(Fit.tree)
  }
  else {
    Data <- data
    Response <- formula[[2]]
    predictors <- formula[[3]]
    if (length(as.list(Response)) != 4) {
      stop(" Response must be a 'survival' object with 'Surv(time1, time2, event)' ")
    }
    y.names <- c(as.character(as.list(Response)[[2]]), as.character(as.list(Response)[[3]]), 
                 as.character(as.list(Response)[[4]]))
    y.IDs <- match(y.names, names(Data))
    y <- survival::Surv(Data[, y.IDs[1]], Data[, y.IDs[2]], 
                        Data[, y.IDs[3]])
    Status <- y[, 3L]
    Times <- y[, 2L]
    unique.times <- sort(unique(Times[Status == 1]))
    temp <- survival::coxph(y ~ 1)
    cumhaz.table <- survival::basehaz(temp)
    if (sum(is.infinite(cumhaz.table$hazard)) != 0) {
      cumhaz.table <- cumhaz.table[cumhaz.table$hazard != 
                                     Inf, ]
    }
    cumhaz.table2 <- cumhaz.table[cumhaz.table$time %in% 
                                    unique.times, ]
    cumhaz.times <- c(0, cumhaz.table2$time[-length(cumhaz.table2$time)], 
                      max(Times))
    cumhaz <- c(0, cumhaz.table2$hazard)
    Start.cumhaz <- stats::approx(cumhaz.times, cumhaz, y[, 
                                                          1L])$y
    End.cumhaz <- stats::approx(cumhaz.times, cumhaz, y[, 
                                                        2L])$y
    Data$Newtime <- End.cumhaz - Start.cumhaz
    Formula = formula(paste(c(paste("cbind(Newtime,", y.names[3], 
                                    ")", sep = ""), predictors), collapse = "~"))
    DATA = Data[, -y.IDs[1:2]]
    Fit.tree <- rpart::rpart(formula = Formula, data = DATA, 
                             method = "poisson", weights = weights, subset = subset, 
                             control = control)
    if (length(unique(Fit.tree$where)) != 1) {
      cventry <- which.min(Fit.tree$cptable[, "xerror"])
      if (no.SE == 0) {
        cpcv <- Fit.tree$cptable[cventry, "CP"]
        Fit.tree <- rpart::prune(Fit.tree, cp = cpcv)
      }
      else {
        xerrorcv <- Fit.tree$cptable[cventry, "xerror"]
        sexerrorcv <- xerrorcv + Fit.tree$cptable[cventry, 
                                                  "xstd"] * no.SE
        cpcvse <- Fit.tree$cptable[which.max(Fit.tree$cptable[, 
                                                              "xerror"] <= sexerrorcv), "CP"]
        Fit.tree <- rpart::prune(Fit.tree, cp = cpcvse)
      }
    }
    return(Fit.tree)
  }
}
<environment: namespace:LTRCtrees>