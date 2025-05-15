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
#' Kos, M., Podgórski, K., & Wu, H. (2025). Dyadic Factorization and Efficient Inversion of Sparse Positive Definite Matrices. arXiv. https://arxiv.org/abs/2505.08144
#' @example R/Examples/ExDyadicObject.R
#' @useDynLib DyadiCarma, .registration=TRUE
#' @importFrom methods callNextMethod new
#' @importFrom Rcpp evalCpp

#' @export
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
                stop(paste("The", l, "th element in SLOT 'aentries' should have ", (2^l - 1) * k, " rows\n"))
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
