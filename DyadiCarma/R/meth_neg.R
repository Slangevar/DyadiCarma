#' @title Negation of dyadic objects
#'
#' @name -,Dyadic-method
#' @aliases -,Dyadic-method, -,Dyadic,ANY-method
#' @usage NULL
#' @description Negates the entries and aentries of a dyadic object.
#' @param e1 A \code{Dyadic}-object.
#' @return The \code{Dyadic}-object with negated entries and aentries.
#' @details The negation is performed in a way that is consistent with
#' the dyadic structure of the matrices.
#' @export
#' @inheritSection Dyadic-class References
#'
#' @seealso
#' \code{\link{Dyadic-class}} for the definition of the \code{Dyadic}-class;
#' \code{\link{dyadFac}} for the dyadic decomposition of dyadic matrices;
#'
#' @example R/Examples/ExNeg.R
#' @export
#'
setMethod(
    f = "-",
    signature = "Dyadic",
    definition = function(e1) {
        e3 <- e1
        entries <- e1@entries
        for (i in seq_along(entries)) {
            entries[[i]] <- -entries[[i]]
        }
        aentries <- e1@aentries
        for (i in seq_along(aentries)) {
            aentries[[i]] <- -aentries[[i]]
        }
        e3@entries <- entries
        e3@aentries <- aentries
        return(e3)
    }
)
