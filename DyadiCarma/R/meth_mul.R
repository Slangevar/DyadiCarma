#' @title Scalar multiplication of dyadic objects
#'
#' @description The scalar multiplication of a dyadic object.
#' @aliases *,numeric,Dyadic-method *,Dyadic,numeric-method
#' @usage \S4method{*}{Dyadic,numeric}(e1, e2)
#' \S4method{*}{numeric,Dyadic}(e1, e2)
#' @param e1 A numeric scalar or a \code{Dyadic}-object.
#' @param e2 A \code{Dyadic}-object or a numeric scalar.
#' @return The \code{Dyadic}-object that is the result of the operation.
#' @details The operations are performed in a way that is consistent with the dyadic structure of the matrices.
#' @export
#' @inheritSection Dyadic-class References
#'
#' @seealso
#' \code{\link{Dyadic-class}} for the definition of the \code{Dyadic}-class;
#' \code{\link{dyadFac}} for the dyadic decomposition of dyadic matrices;
#'
#' @example R/Examples/ExMul.R
#' @export
#'

setMethod(
    f = "*",
    signature = c("Dyadic", "numeric"),
    definition = function(e1, e2) {
        e3 <- e1
        entries <- e1@entries
        aentries <- e1@aentries
        for (i in seq_along(entries)) {
            entries[[i]] <- e2 * entries[[i]]
        }
        for (i in seq_along(aentries)) {
            aentries[[i]] <- e2 * aentries[[i]]
        }
        e3@entries <- entries
        e3@aentries <- aentries

        return(e3)
    }
)

#' @rdname times-Dyadic-numeric-method
#' @example R/Examples/ExMul.R
#' @export
setMethod(
    f = "*",
    signature = c("numeric", "Dyadic"),
    definition = function(e1, e2) {
        return(e2 * e1)
    }
)
