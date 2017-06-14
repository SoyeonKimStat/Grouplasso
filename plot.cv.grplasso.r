plot.cv.grplasso <- function(object) {
  df= data.frame(lambda=object$lambda, cvm=object$cvm, cvse=object$cvse)
  library(ggplot2)
  ggplot(df, aes(x=log(lambda), y=cvm)) + 
    geom_errorbar(aes(ymin=cvm-cvse, ymax=cvm+cvse), width=.1) +
    geom_line() +
    geom_point() +
    labs(y = "Mean-Squared Error")+
    labs(x = "log(Lambda)") +
    geom_vline(xintercept = c(log(object$lambda.min),log(object$lambda.1se)), linetype="dashed")
}