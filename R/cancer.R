
# hidden call
# load cancer diagnoses
call_cancer <- function()
{
  data("diagnosis")
  return(Class == "malignant")
}


#' This function evaluates the accuracy and logloss of predictions for a Breast Cancer test set (150 patients).
#' @title Evaluates accuracy and logloss for a Breast Cancer test set.
#' @author Olivier François
#' @param pred a vector with 150 numeric values representing "malignant" diagnosis probabilities for patients in the Breast Cancer
#'  test data.
#' @return a list with 2 attributes: accuracy and logloss.
#' @seealso
#' @examples
#' library(isd)
#' pred <- runif(150)
#' eval_cancer(pred)
#' @export
eval_cancer <-  function(pred){
  pred <- as.numeric(pred)
  diagnosis <- call_cancer()
  if (length(pred) != length(diagnosis)) {stop("Incorrect number of predictions.")}
  if (sum(pred < 0 | pred > 1) != 0) {stop("Incorrect predictions (probabilities expected).")}
  accuracy <- mean((pred > 0.5) == diagnosis)
  pred[pred == 0] <- 0.0000000001
  pred[pred == 1] <- 0.9999999999
  log.loss <- -mean( diagnosis*log(pred) + (1-diagnosis)*log(1-pred))
  return(list(accuracy = accuracy, log.loss = log.loss))
}




