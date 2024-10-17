#' @title Matrix representation of dyadic objects
#'
#' @description Extracting the matrix representation
#' of a \code{Dyadic}-object.
#' @param x \code{Dyadic}-object.
#' @return The result is a \code{width*(2^height-1) x width*(2^height-1)} matrix.
#' @details The dyadic structure contains information about the type of matrix and its width and height.
#' @export
#' @inheritSection Dyadic-class References
#'
#' @seealso
#' \code{\link{Dyadic-class}} for the definition of the \code{Dyadic}-class;
#' \code{\link{dyadalg}} for the dyadic decomposition of dyadic matrices;
#'
#' @example R/Examples/ExAs_matrix.R
#'
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

# Transpose the Dyadic object.
# setGeneric("t", function(x) standardGeneric("t"), signature = "x")
# setGeneric("t", function(x) standardGeneric("t"))
#' @export
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


setMethod(
    f = "-",
    signature = "Dyadic",
    definition = function(e1) {
        e2 <- e1
        entries <- e1@entries
        for (i in seq_along(entries)) {
            entries[[i]] <- -entries[[i]]
        }
        e2@entries <- entries

        return(e2)
    }
)

#' @export
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


#' @export
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

#' @export
setMethod(
    f = "-",
    signature = c("Dyadic", "Dyadic"),
    definition = function(e1, e2) {
        return(e1 + (-e2))
    }
)

