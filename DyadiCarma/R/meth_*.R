#' @title Scalar multiplication of dyadic objects
#'
#' @description The scalar multiplication of a dyadic object.
#' @param e1 numeric scalar.
#' @param e2 \code{Dyadic}-object.
#' @return The \code{Dyadic}-object that is the result of the operation.
#' @details The operations are performed in a way that is consistent with the dyadic structure of the matrices.
#' @export
#' @inheritSection Dyadic-class References
#'
#' @seealso
#' \code{\link{Dyadic-class}} for the definition of the \code{Dyadic}-class;
#' \code{\link{dyadalg}} for the dyadic decomposition of dyadic matrices;
#'
#' @example R/Examples/Ex*.R
#'
#'
#' @export
#'

setMethod(
    f = "*",
    signature = c("Dyadic", "numeric"),
    definition = function(e1, e2) {
        e3 <- e1
        entries <- e1@entries
        for (i in seq_along(entries)) {
            entries[[i]] <- e2 * entries[[i]]
        }
        e3@entries <- entries

        return(e3)
    }
)

#' @export
setMethod(
    f = "*",
    signature = c("numeric", "Dyadic"),
    definition = function(e1, e2) {
        return(e2 * e1)
    }
)


