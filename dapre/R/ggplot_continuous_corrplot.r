#' Correlation plot of continuous vars
#'
#' This is a wrap of correlation matrix plot code
#'
#' The output of `ggplot_continuous_corrplot()` will be a correlation matrix plot
#'
#' @param data dataframe
#' @param vars character vector of vars
#' @param tar character of target variable name
#' @param threshold default 0.5
#'
#' @export
#' @rdname ggplot_continuous_corrplot
ggplot_continuous_corrplot <- function(data,vars,tar,threshold = 0.5){
  install_n_require('corrplot')

  if (length(vars) < 2) stop("must contain more than 1 continuous variable")

  all_numVar <- data[,vars]
  cor_numVar <- cor(all_numVar,use = "pairwise.complete.obs")
  cor_sorted <- as.matrix(sort(cor_numVar[,tar], decreasing = TRUE))
  CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>threshold)))
  cor_numVar <- cor_numVar[CorHigh, CorHigh]
  if (!is.matrix(cor_numVar)) stop("Narrow down the threshold and try again")
  corrplot.mixed(cor_numVar,
                 tl.col="black",
                 tl.pos = "lt",
                 tl.cex = 0.7,
                 cl.cex = .7,
                 number.cex=.7)
}
