
# hidden call
# load survived
call_sv <- function()
{
  data("sv")
  return(survived.test)
}


#' This function evaluates the accuracy and logloss of predictions for the Titanic test set (272 individuals).
#' @title Evaluates accuracy and logloss for the Titanic test set.
#' @author Olivier François
#' @param pred a vector with 272 numeric values representing survival probabilities for all passengers in the Titanic
#'  test data.
#' @return a list with 2 attributes: accuracy and logloss.
#' @seealso
#' @examples
#' library(isd)
#' pred <- runif(272)
#' eval_titanic(pred)
#' @export
eval_titanic <-  function(pred){
  pred <- as.numeric(pred)
  sv <- call_sv()
  if (length(pred) != length(sv)) {stop("Incorrect number of predictions.")}
  if (sum(pred < 0 | pred > 1) != 0) {stop("Incorrect predictions (probabilities expected).")}
  accuracy <- mean((pred > 0.5) == sv)
  pred[pred == 0] <- 0.0000001
  pred[pred == 1] <- 0.9999999
  log.loss <- -mean( sv*log(pred) + (1-sv)*log(1-pred))
  return(list(accuracy = accuracy, log.loss = log.loss))
}




