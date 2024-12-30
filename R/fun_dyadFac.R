#' @title Efficient factorization of a positive definite symmetrically dyadic
#' matrix.
#' @description This function implement the efficient factorization of a
#' positive definite symmetrically dyadic matrix \eqn{\boldsymbol \Sigma}.
#' It computes the vertically dyadic matrix \eqn{\mathbf P} such that
#' \eqn{\mathbf P^\top \boldsymbol \Sigma \mathbf P = \mathbf I}.
#' @param S A \code{Dyadic} object of type \code{"symm"} representing a positive
#' definite symmetrically dyadic matrix;
#' @param inv The boolean value indicating whether the inverse of
#' \eqn{\boldsymbol \Sigma} should be returned.
#' @param band The boolean value indicating whether the input S is a band
#' matrix. If TRUE, then a optimized band-focused algorithm is called.
#' If band==TRUE, but the input matrix is not a band one, the function will
#' return the corresponding result for the band part of the input matrix.
#' @return If \code{inv == TRUE}, then the inverse of \eqn{\boldsymbol \Sigma},
#' which is a \code{(2^(height)-1)*breadth x (2^(height)-1)*breadth} classic
#' matrix, is returned. Otherwise, the vertically \code{Dyadic} object for
#' \eqn{\mathbf P} is returned.
#' @details This function implement the efficient factorization of a
#' positive definite symmetrically dyadic matrix.
#' @export
#'
#' @seealso \code{\link{Dyadic-class}} for a description of the class;
#' @example R/Examples/ExdyadFac.R
#'

dyadFac <- function(S, inv = FALSE, band = FALSE) {
    if (class(S)[1] != "Dyadic") {
        stop(paste("The argument does not belong to the Dyadic-class.\n"))
    }
    N <- S@height
    k <- S@breadth

    if (!(S@type == "symm")) {
        stop("Only symmetrically dyadic matrices are eligible for the dyadic factorization algorithm!")
    }

    if (band) {
        P <- new("Dyadic", height = N, breadth = k, type = "vert", entries = rcpp_bandalg_core(S@entries, N, k))
    } else {
        P <- new("Dyadic", height = N, breadth = k, type = "vert", entries = rcpp_dyadFac_core(S@entries, N, k))
    }


    if (inv) {
        return(P %*% t(P))
    } else {
        return(P)
    }
}
