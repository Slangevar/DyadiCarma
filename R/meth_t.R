#' @title Transpose of a \code{Dyadic} object
#'
#' @description The \code{Dyadic} object transpose of a \code{Dyadic} object: \code{t(Dyadic)}.
#' @param x \code{Dyadic}-object;
#' @return The \code{Dyadic}-object that is the result of the operation with properly defined fields.
#' @details The operations are performed in a way that is consistent with the dyadic structure of the matrices.
#' @export
#' @inheritSection Dyadic-class References
#'
#' @seealso
#' \code{\link{Dyadic-class}} for the definition of the \code{Dyadic}-class;
#' \code{\link{dyadFac}} for the dyadic decomposition of dyadic matrices;
#'
#' @example R/Examples/ExT.R
#'
#' @export
#'

# Transpose the Dyadic object.
setMethod(
    "t",
    "Dyadic",
    function(x) {
        if (x@type == "asymm") {
            N <- x@height
            k <- x@breadth
            trans_list <- asymm_trans(x@entries, x@aentries, N, k)
            result <- new("Dyadic", entries = trans_list[[1]], aentries = trans_list[[2]], type = "asymm", height = N, breadth = k)
        } else if (x@type == "vert") {
            result <- x
            result@type <- "horiz"
        } else if (x@type == "horiz") {
            result <- x
            result@type <- "vert"
        } else {
            result <- x
        }
        return(result)
    }
)
