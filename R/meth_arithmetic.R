#' Arithmetic methods for Dyadic objects
#'
#' Implements arithmetic operations for Dyadic objects, including negation,
#' addition, subtraction, and scalar multiplication.
#'
#' @section Methods:
#' \describe{
#'   \item{Unary `-'}{Negates a Dyadic object.}
#'   \item{`+'}{Adds two Dyadic objects.}
#'   \item{`-'}{Subtracts one Dyadic object from another.}
#'   \item{`*'}{Multiplies a Dyadic object by a scalar or vice versa.}
#' }
#'
#' @return A Dyadic object representing the corresponding result of the
#' arithmetic operation.
#'
#' @inheritSection Dyadic-class References
#'
#' @seealso
#' \code{\link{Dyadic-class}} for the definition of the \code{Dyadic}-class;
#' \code{\link[DyadiCarma]{as.matrix}} for extracting the matrix representation
#' of a \code{Dyadic}-object
#'
#' @example R/Examples/ExArithmetic.R
#' @name Dyadic Arithmetic
NULL

# Define addition
#' @rdname Dyadic-Arithmetic
#' @usage NULL
#' @export
setMethod(
    f = "+",
    signature = c("Dyadic", "Dyadic"),
    definition = function(e1, e2) {
        if (e1@breadth != e2@breadth || e1@height != e2@height) {
            stop(paste("Either the 'height' or 'breadth' slots of
            the arguments are not equal!\n"))
        }

        N <- e1@height
        k <- e1@breadth

        e3 <- e1
        if (e1@type != e2@type) {
            e3@type <- "asymm"
        }
        result_list <- add_helper(
            e1@entries, e1@aentries, e2@entries,
            e2@aentries, e1@type, e2@type, N, k
        )
        e3@entries <- result_list[[1]]
        e3@aentries <- result_list[[2]]

        return(e3)
    }
)

# Define subtraction
#' @rdname Dyadic-Arithmetic
#' @usage NULL
#' @export
setMethod(
    f = "-",
    signature = c("Dyadic", "Dyadic"),
    definition = function(e1, e2) {
        return(e1 + (-e2))
    }
)

# Define scalar multiplication
#' @rdname Dyadic-Arithmetic
#' @usage NULL
#' @export
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

#' @rdname Dyadic-Arithmetic
#' @usage NULL
#' @export
setMethod(
    f = "*",
    signature = c("numeric", "Dyadic"),
    definition = function(e1, e2) {
        return(e2 * e1)
    }
)

# Define negation
#' @rdname Dyadic-Arithmetic
#' @usage NULL
#' @export
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
