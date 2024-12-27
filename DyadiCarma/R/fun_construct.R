#' @title Construction of a \code{Dyadic} object
#'
#' @description The function constructs a \code{Dyadic} object either
#' with random entries (default) or with entries equal to one.
#' @param height positive integer, the number of dyadic levels;
#' @param breadth positive integer, the breadth of the dyadic structure;
#' @param type string, one of the following character strings:
#' \code{horiz},\code{vert},\code{symm}, \code{asymm}, which indicates the type of dyadic matrix;
#' @param distr string, if it is one the strings 'binom', 'unif', 'norm' it indicate
#' the type of the distribution used for obtaining the entries, any other string, for example 'nonrand', results in
#' non-random 1's in all entries.
#' @param param vector of two numeric values, these are parameters for the distributions used to generate
#' the entries.
#' @return A \code{Dyadic}-object.
#' @details The function constructs a generic \code{Dyadic}-object of any type and
#' in the case of the \code{symm} type with random entries the object represents a symmetric matrix.
#' @export
#'
#' @inheritSection Dyadic-class References
#'
#' @seealso \code{\link{Dyadic-class}} for a description of the class.
#' @example R/Examples/ExConstruct.R
#'
#'
construct <- function(
    height, breadth, type = "vert", distr = "nonrand", param = c(0, 1)) {
    # Building 'entries'
    EE <- rcpp_constr(height, breadth, distr, param)
    if (type == "symm" || type == "asymm") { # the case of symmetrically dyadic symmetric matrix,
        # entries in the diagonal blocks need to be symmetric matrices so only the diagonal and above terms are needed
        # below the diagonal are set to zero
        for (l in 1:height)
        {
            # Setting the central blocks to be symmetric
            ML <- matrix(1, ncol = breadth, nrow = breadth)
            for (k in 1:(breadth - 1)) {
                ML[(k + 1):breadth, k] <- rep(0, breadth - k)
            }
            UML <- matrix(1, nrow = 1, ncol = 2^(height - l)) %x% ML # Putting together upper diagonal square matrices to match the l-level structure
            CP <- (dim(EE[[l]])[1] - breadth) / 2 + (1:breadth)
            EE[[l]][CP, ] <- EE[[l]][CP, ] * UML # This will zero the terms below the diagonal
        }
    }
    AE <- list()
    if (type == "asymm") { # the case of symmetrically dyadic but asymmetric matrix, for which
        # entries in the horizontal locations are, in general, different from those in the vertical ones
        AE <- rcpp_constr(height, breadth, distr, param) #' asymmetric' entries
        for (l in 1:height)
        {
            # Setting the on and above diagonal terms of the diagonal blocks to zero as the corresponding values from
            # EE will be used for these
            ML <- matrix(1, ncol = breadth, nrow = breadth)
            for (k in 1:(breadth)) {
                ML[k:breadth, k] <- rep(0, breadth - k + 1)
            }
            UML <- matrix(1, nrow = 1, ncol = 2^(height - l)) %x% ML # Putting together upper diagonal square matrices to match the l-level structure
            CP <- (dim(AE[[l]])[1] - breadth) / 2 + (1:breadth)
            AE[[l]][CP, ] <- AE[[l]][CP, ] * UML # This will set values zero on and below diagonal to zero as needed.
        }
    }

    build <- new("Dyadic", height = height, breadth = breadth, type = type, entries = EE, aentries = AE)

    return(build)
} # The end of the function
