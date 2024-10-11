#' @title The class to represent a dyadic matrix
#' @description The main class in the \code{Dyadic}-package used for representing three types of dyadic matrices: horizontal, vertical,
#' symmetric, and asymmetric.
#' @slot height positive integer, the number of dyadic levels;
#' @slot breadth positive integer, the breadth of the dyadic structure;
#' @slot type string, one of the following character strings: \code{horiz},\code{vert},\code{symm}, \code{asymm} which
#' indicates the type of dyadic matrix
#' \itemize{
#'     \item \code{horiz} horizontal,
#'     \item \code{vert} vertical,
#'     \item \code{symm} symmetric,
#'     \item \code{asymm} asymmetric,
#' }
#' where the last two types distinguish symmetrically dyadic matrices (they both have symmetric dyadic structure)
#' that correspond to symmetric or not symmetric matrices.
#' @slot entries list (of matrices); a list of the length \code{height} containing
#' \code{(2^(l)-1)*breadth x 2^(height-l)*breadth} matrices, where \code{l} is the index running through the list.
#' Each matrix in the list includes the entries corresponding to
#' \code{2^(height-l)} \code{(2^l-1)*breadth x breadth}-matrices
#' put side by side columnwise in the \code{l}th level of a dyadic structure.    In the 'symm'- and 'asymm'-cases, the terms below
#' diagonal on the diagonal blocks are set to zero.
#' @slot aentries list (of matrices); a list which is either empty if the slot \code{type} is not \code{'asymm'}
#' or of the length \code{height} otherwise, in which the case it contains
#' \code{(2^(l)-1)*breadth x 2^(height-l)*breadth} matrices, where \code{l} is the index running through the list.
#' Each matrix in the list includes the entries corresponding to \code{2^(height-l)}.
#' \code{(2^l-1)*breadth x breadth}-matrices
#' put side by side columnwise in the \code{l}th horizontal level of an asymmetric dyadic structure.
#' The terms above and on the diagonal in the diagonal blocks are set to zero because they are accounted in the slot \code{entries}.
#' @return running \code{new("Dyadic")} return an object that belongs to the class \code{Dyadic},
#' with the initialization of the default values for the fields.
#' @section References:
#' Kos, M., Podg\eqn{\mbox{\'o}}{o}rski, K., Wu, H. (2024) "Sparse"
#' @seealso \code{\link{plot,Dyadic-method}} for plotting methods for \code{Dyadic}-objects;
#'
#' @example R/Examples/ExDyadicObject.R
#' @useDynLib DyadiCpp, .registration=TRUE
#' @importFrom methods callNextMethod new
#' @importFrom Rcpp evalCpp


setClass(
    "Dyadic",
    representation(
        height = "numeric", breadth = "numeric", type = "character",
        entries = "list", aentries = "list"
    ),
    prototype(
        height = 1, breadth = 1, type = "symm", entries = list(as.matrix(1)), aentries = list()
    )
    # prototype sets the initial values for slots, if desired
)


setMethod("initialize", "Dyadic", function(.Object, ...) { # This will be used for the function 'new()'
    .Object <- callNextMethod() # It is not obvious what this is supposed to do but it is a standard, it somehow calls
    # the currently described method on '.Object' after finishing this method definition.
    # Not a very precise description but 'things' work with this being present.
    N <- .Object@height
    k <- .Object@breadth


    # 1) checking if the size of entries and height is matching
    if (N != length(.Object@entries)) {
        stop(paste("SLOT 'entries' should be a list that is of the length", N, "\n"))
    }

    # 2) checking if the sizes of the matrices inside SLOT 'entries' agree with the values evaluated based on SLOTS: 'breadth' and 'height'
    for (l in 1:N) {
        if ((2^l - 1) * k != dim(.Object@entries[[l]])[1]) {
            stop(paste("The", l, "th element in SLOT 'entries' should have ", (2^l - 1) * k, " rows\n"))
        }
        if (2^(N - l) * k != dim(.Object@entries[[l]])[2]) {
            stop(paste("The", l, "th element in SLOT 'entries' should have ", 2^(N - l) * k, " columns\n"))
        }
    }
    # 3) Checking 'aentries' for the asymmetric type
    if (.Object@type == "asymm") {
        for (l in 1:N) {
            if ((2^(l) - 1) * k != dim(.Object@aentries[[l]])[1]) {
                stop(paste("The", i, "th element in SLOT 'aentries' should have ", (2^l - 1) * k, " rows\n"))
            }
            if (2^(N - l) * k != dim(.Object@aentries[[l]])[2]) {
                stop(paste("The", l, "th element in SLOT 'aentries' should have ", 2^(N - l) * k, " columns\n"))
            }
        }
    }
    if (.Object@type != "horiz" & .Object@type != "vert" & .Object@type != "symm" & .Object@type != "asymm") {
        stop(paste("The SLOT 'type' is not properly specified, should be one of the following: 'horiz', 'vert', 'symm', 'asymm'\n "))
    }
    .Object
})

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


# dyad2matrix
#' @export
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

#' @export
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
