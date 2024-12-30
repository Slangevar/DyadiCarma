#' @title Matrix representation of dyadic objects
#'
#' @description Extracting the matrix representation
#' of a \code{Dyadic}-object.
#' @param x \code{Dyadic}-object.
#' @aliases as.matrix
#' @return The result is a \code{width*(2^height-1) x width*(2^height-1)} matrix.
#' @details The dyadic structure contains information about the type of matrix and its width and height.
#' @export
#' @inheritSection Dyadic-class References
#'
#' @seealso
#' \code{\link{Dyadic-class}} for the definition of the \code{Dyadic}-class;
#' \code{\link{dyadFac}} for the dyadic decomposition of dyadic matrices;
#'
#' @example R/Examples/ExAsMatrix.R
#'
#' @export
#'

setMethod(
    "as.matrix",
    "Dyadic",
    function(x) {
        N <- x@height
        k <- x@breadth

        if (!(x@type %in% c("vert", "horiz", "symm", "asymm"))) {
            stop('Invalid type for a Dyadic object. Eligible types are "vert", "horiz", "symm", are "asymm".')
        }
        return(rcpp_as_matrix(x@entries, x@aentries, N, k, x@type))
    }
)
