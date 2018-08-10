function (Formula, data, Control = partykit::ctree_control()) 
{
  requireNamespace("inum")
  if (length(as.list(Formula[[2]])) != 4) {
    stop(" Response must be a 'survival' object with 'Surv(time1, time2, event)' ")
  }
  Response <- as.character(Formula)[[2]]
  h2 <- function(y, x, start = NULL, weights, offset, estfun = TRUE, 
                 object = FALSE, ...) {
    if (is.null(weights)) 
      weights <- rep(1, NROW(y))
    s <- .logrank_trafo2(y[weights > 0, , drop = FALSE])
    r <- rep(0, length(weights))
    r[weights > 0] <- s
    list(estfun = matrix(as.double(r), ncol = 1), converged = TRUE)
  }
  partykit::ctree(formula = Formula, data = data, ytrafo = h2, 
                  control = Control)
}
# <environment: namespace:LTRCtrees>