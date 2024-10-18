#' @title Addition of dyadic objects
#'
#' @description Addition of the dyadic objects.
#' @param e1 \code{Dyadic}-object.
#' @param e2 \code{Dyadic}-object.
#' @return The \code{Dyadic}-object that is the result of the operation.
#' @details The operations are performed in a way that is consistent with the dyadic structure of the matrices.
#' @export
#' @inheritSection Dyadic-class References
#'
#' @seealso
#' \code{\link{Dyadic-class}} for the definition of the \code{Dyadic}-class;
#' \code{\link{dyadFac}} for the dyadic decomposition of dyadic matrices;
#'
#' @example R/Examples/Ex+.R
#'
#'
#' @export
#'



setMethod(
    f = "+",
    signature = c("Dyadic", "Dyadic"),
    definition = function(e1, e2) {
        if (e1@breadth != e2@breadth || e1@height != e2@height) {
            stop(paste("Either the 'height' or 'breadth' slots of the arguments are not equal!\n"))
        }

        N <- e1@height
        k <- e1@breadth

        e3 <- e1
        if (e1@type != e2@type) {
            e3@type <- "asymm"
        }
        result_list <- add_helper(e1@entries, e1@aentries, e2@entries, e2@aentries, e1@type, e2@type, N, k)
        e3@entries <- result_list[[1]]
        e3@aentries <- result_list[[2]]

        return(e3)
    }
)
