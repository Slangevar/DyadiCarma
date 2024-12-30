#' @title Matrix multiplication of dyadic objects
#'
#' @description The standard matrix multiplication of two \code{Dyadic}-objects.
#' @param x \code{Dyadic}-object;
#' @param y \code{Dyadic}-object;
#' @return Either a \code{Dyadic}-object or a regular matrix depending on the structure type
#' of the input objects. The matrix outcome of multiplication is also
#' reported as a message in the command line.
#' @details Both orders of multiplication are implemented: \code{(scalar * dyadic)} and \code{(dyadic * scalar)}.
#' @export
#' @inheritSection Dyadic-class References
#'
#' @seealso
#' \code{\link{Dyadic-class}} for the definition of the \code{Dyadic}-class;
#' \code{\link{dyadFac}} for the dyadic decomposition of dyadic matrices;
#'
#' @example R/Examples/ExInner.R
#'
#'
#' @export
#'

setMethod(
    "%*%",
    signature(x = "Dyadic", y = "Dyadic"),
    function(x, y) {
        if (x@breadth != y@breadth || x@height != y@height) {
            stop(paste("Either the 'height' or 'breadth' slots of the arguments are not equal!\n"))
        }

        N <- x@height
        k <- x@breadth

        if (x@type == "vert" && y@type == "vert") {
            MD <- x
            MD@entries <- multiply_vv(x@entries, y@entries, N, k)
        } else if (x@type == "horiz" && y@type == "horiz") {
            MD <- x
            MD@entries <- multiply_vv(y@entries, x@entries, N, k)
        } else if (x@type == "horiz" && y@type == "vert") {
            MD <- x
            MD@type <- "asymm"
            M <- multiply_hv(x@entries, y@entries, N, k)
            MD@entries <- M[[1]]
            MD@aentries <- M[[2]]
        } else if (x@type == "symm" && y@type == "vert") {
            MD <- x
            MD@type <- "asymm"
            M <- multiply_hsv(x@entries, y@entries, N, k, "v")
            MD@entries <- M[[1]]
            MD@aentries <- M[[2]]
        } else if (x@type == "asymm" && y@type == "vert") {
            MD <- x
            MD@type <- "asymm"
            M <- multiply_hasv(x@entries, x@aentries, y@entries, N, k, "v")
            MD@entries <- M[[1]]
            MD@aentries <- M[[2]]
        } else if (x@type == "horiz" && y@type == "symm") {
            MD <- y
            MD@type <- "asymm"
            M <- multiply_hsv(x@entries, y@entries, N, k, "h")
            MD@entries <- M[[1]]
            MD@aentries <- M[[2]]
        } else if (x@type == "horiz" && y@type == "asymm") {
            MD <- y
            MD@type <- "asymm"
            M <- multiply_hasv(y@entries, y@aentries, x@entries, N, k, "h")
            MD@entries <- M[[1]]
            MD@aentries <- M[[2]]
        } else if (x@type == "vert" && y@type == "horiz") {
            message("When multiplying a vertically dyadic matrix with a horizontally dyadic one, the resulting matrix is no longer dyadic.")
            MD <- multiply_vh(x@entries, y@entries, N, k)
        } else if (x@type == "vert" && y@type == "symm") {
            message("When multiplying a vertically dyadic matrix with a symmetrically dyadic one, the resulting matrix is no longer dyadic.")
            MD <- multiply_vsh(x@entries, y@entries, N, k, "v")
        } else if (x@type == "symm" && y@type == "horiz") {
            message("When multiplying a symmetrically dyadic matrix with a horizontally dyadic one, the resulting matrix is no longer dyadic.")
            MD <- multiply_vsh(x@entries, y@entries, N, k, "h")
        } else if (x@type == "vert" && y@type == "asymm") {
            message("When multiplying a vertically dyadic matrix with a asymmetrically dyadic one, the resulting matrix is no longer dyadic.")
            MD <- multiply_vash(x@entries, y@entries, y@aentries, N, k, "v")
        } else if (x@type == "asymm" && y@type == "horiz") {
            message("When multiplying a asymmetrically dyadic matrix with a horizontally dyadic one, the resulting matrix is no longer dyadic.")
            MD <- multiply_vash(y@entries, x@entries, x@aentries, N, k, "h")
        } else if ((x@type == "symm" || x@type == "asymm") && (y@type == "symm" || y@type == "asymm")) {
            message("When multiplying a (a)symmetrically dyadic matrix with a (a)symmetrically dyadic one, the resulting matrix is no longer dyadic.")
            MD <- multiply_sas(x@entries, x@aentries, y@entries, y@aentries, N, k)
        } else {
            stop("This method is not implemented yet. 2024.10.01")
        }
        return(MD)
    }
)
