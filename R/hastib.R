#' This function simulates training and testing data sets according to a statistical model described by Hastie and Tibshirani in their book
#' @title Simulate two-class training and testing data according to Hastie and Tibishirani's model
#' @author Olivier François
#' @param n_train an integer for the number of training samples.
#' @param n_test an integer for the number of test samples.
#' @param n_subclass an integer for the number of subclass in the Hastie and Tibishirani model.
#' @param sigma2 a numeric for the within-group variance in the Hastie and Tibishirani model.
#' @return a list with 4 attributes:
#' @return $train a matrix with n_train samples
#' @return $test  a matrix with n_test samples
#' @return $class_train a vector of class labels for the train set
#' @return $class_test  a vector of class labels for the test set
#' @seealso
#' @examples
#' library(isd)
#' X <- rhastib(n_train = 100, n_test = 100, n_subclass = 10, sigma2 = .1)
#' plot(X$train, col = X$class_train, pch = 19)
#' @export
rhastib <- function(n_train = 100,
                    n_test = 100,
                    n_subclass = 10,
                    sigma2 = .1){
  Id <- diag(1, nrow = 2) #matrice identité

  n_train <- floor(n_train/2)
  n_test <- floor(n_test/2)
  n <- n_train + n_test

  # simulation des données "bleues"
  subclass_0 <- MASS::mvrnorm(n_subclass, mu = c(1,0), Sigma = Id)
  m_0 <- subclass_0[sample(1:n_subclass, n, replace = TRUE),]
  class_0 <-  m_0 + MASS::mvrnorm(n, mu = c(0,0), Sigma = sigma2*Id)

  # simulation des données "orange"
  subclass_1 <- MASS::mvrnorm(n_subclass, mu = c(0,1), Sigma = Id)
  m_1 <- subclass_1[sample(1:n_subclass, n, replace = TRUE),]
  class_1 <-  m_1 + MASS::mvrnorm(n, mu = c(0,0), Sigma = sigma2*Id)

  # creation des données train et test
  x_train <- rbind(class_0[1:n_train,], class_1[1:n_train,])
  x_test  <- rbind(class_0[(n_train+1):n,], class_1[(n_train+1):n,])

  # creation des classes train et test
  type_train <- rep(c("lightblue","orange"), each = n_train)
  type_test <- rep(c("lightblue","orange"), each = n_test)

  return(list(train = x_train,
              test = x_test,
              class_train = type_train,
              class_test = type_test))
}
