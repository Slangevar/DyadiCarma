#' @title Subtraction of dyadic objects
#'
#' @description The subtraction of dyadic objects.
#' @usage \S4method{-}{Dyadic,Dyadic}(e1, e2)
#' @param e1 A \code{Dyadic}-object.
#' @param e2 A \code{Dyadic}-object.
#' @return The \code{Dyadic}-object that is the result of the subtraction.
#' @details The operations are performed in a way that is consistent with the dyadic structure of the matrices.
#' @export
#' @inheritSection Dyadic-class References
#'
#' @seealso
#' \code{\link{Dyadic-class}} for the definition of the \code{Dyadic}-class;
#' \code{\link{dyadFac}} for the dyadic decomposition of dyadic matrices;
#'
#' @example R/Examples/Ex-.R
#'
#'

#' @export
setMethod(
    f = "-",
    signature = c("Dyadic", "Dyadic"),
    definition = function(e1, e2) {
        return(e1 + (-e2))
    }
)
