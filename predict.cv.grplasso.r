predict.cv.grplasso <- function(object, newx, s="lambda.1se") {
  if(s=="lambda.1se") {
    predict(object$grplasso.fit, newx)[,1]
  } else if(s=="lambda.min") {
    predict(object$grplasso.fit, newx)[,2]
  } else {
    stop("s should be either lambda.1se or lambda.min")
  }
}