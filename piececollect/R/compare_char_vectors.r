#' Compare the similarity of two vectors
#'
#' This is the fundamental function of comparing two vectors
#' and return cosine similarity score, suggest to use with *apply funciton
#'
#' The output of `compare_char_vectors()` will be a one number score between 0~1
#'
#' The output of `cosine_similarity()` will be a one number score between 0~1
#'
#' @param lhsv left handside character vector
#' @param rhsv right handside character vector
#' @param xvector numeric vector of x
#' @param yvector numberic vector of y
#' @return
#' `compare_char_vector()` return a one number score between 0~1 indicating
#' the cosine similarity of the two input vectors
#'
#' `cosine_similarity()` return a one number score between 0~1 indicating
#' the cosine similarity of the two input vectors
#' @examples
#' compare_char_vectors(c('one','two','three'),c('two','three','four'))
#' cosine_similarity(c(1,2,3),c(2,3,4))

#' @export
#' @rdname compare_char_vectors
compare_char_vectors <- function(lhsv,rhsv){
    if (!is.vector(lhsv)) stop(paste0(lhsv, ' is not a vector'))
    if (!is.vector(rhsv)) stop(paste0(rhsv, ' is not a vector'))
    n1 <- data.frame(x2 = lhsv,stringsAsFactors = FALSE)
    n2 <- data.frame(x2 = rhsv,stringsAsFactors = FALSE)
    n1$x1 <- 1
    n2$x1 <- 2
    n <- unique(rbind(n1,n2))
    tcount <- table(n$x1,n$x2)
    cosine_similarity(tcount[1,],tcount[2,])
}

#' @export
#' @rdname compare_char_vectors
cosine_similarity <- function(xvector, yvector){
    if (!is.vector(xvector)) stop(paste0(xvector, ' is not a numeric vector'))
    if (!is.vector(yvector)) stop(paste0(yvector, ' is not a vector'))
    sum(xvector * yvector) / sqrt(sum(xvector ** 2) * sum(yvector ** 2))
}
