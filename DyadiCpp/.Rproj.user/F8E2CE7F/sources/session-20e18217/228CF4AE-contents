#' @title Efficient factorization of a positive definite symmetrically dyadic
#' matrix.
#' @description This function implement the efficient factorization of a
#' positive definite symmetrically dyadic matrix \eqn{\boldsymbol \Sigma}.
#' It computes the vertically dyadic matrix \eqn{\mathbf P} such that
#' \eqn{\mathbf P^\top \boldsymbol \Sigma \mathbf P = \mathbf I}.
#' @param D A \code{Dyadic} object of type \code{"symm"} representing a positive
#' definite symmetrically dyadic matrix;
#' @param inv The boolean value indicate whether the inverse of
#' \eqn{\boldsymbol \Sigma} should be returned.
#' @return If \code{inv == TRUE}, then the inverse of \eqn{\boldsymbol \Sigma},
#' which is a \code{(2^(height)-1)*breadth x (2^(height)-1)*breadth} classic
#' matrix, is returned. Otherwise, the vertically \code{Dyadic} object for
#' \eqn{\mathbf P} is returned.
#' @details This function implement the efficient factorization of a
#' positive definite symmetrically dyadic matrix.
#' @export
#'
#' @seealso \code{\link{Dyadic-class}} for a description of the class;
#' @example R/Examples/ExDyadalg.R
#'

dyadalg <- function(D, inv = FALSE) {
    if (class(D)[1] != "Dyadic") {
        stop(paste("The argument does not belong to the Dyadic-class.\n"))
    }
    N <- D@height
    k <- D@breadth

    if (!(D@type == "symm")) {
        stop("Only symmetrically dyadic matrices are eligible for the dyadic factorization algorithm!")
    }

    P <- new("Dyadic", height = N, breadth = k, type = "vert", entries = rcpp_dyadalg_core(D@entries, N, k))

    if (inv) {
        return(P %*% t(P))
    } else {
        return(P)
    }
}
