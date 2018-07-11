#' Calculate model fitting index
#'
#' This will a function of computing modeling index, return such as r-square, rmse, etc.
#'
#' The output of `model_fit_index()` will be a one list of all indexes
#'
#' @param y original target value vector
#' @param yhat predicted target value vector
#' @param nbeta number of beta estimated(including constant)
#' @return
#' `model_fit_index()` return a list of all possible normally used index of evaluating a model
#' @examples
#' model_fit_index(y,yhat)
#' @export
#' @rdname model_fit_index
model_fit_index <- function(y,yhat,nbeta){
    SST = sum((y - mean(y)) ** 2)
    SSE = sum((yhat - y) ** 2)
    rsquare = 1 - (SSE / SST)
    r2 = cor(y,yhat) ** 2
    rmse = sqrt(SSE / length(y))
    mape = mean(abs((yhat - y) / y))
    AIC = length(y) * (log(2 * pi) + 1 + log((sum((yhat - y) ** 2) / length(y)))) + ((nbeta + 1) * 2)
    return(list(rsquare = rsquare,
                r2 = r2,
                rmse = rmse,
                mape = mape,
                AIC = AIC))
}
