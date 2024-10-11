#' @title Extract a \code{Dyadic} object from a numeric matrix
#' @description This function extract a \code{Dyadic} object of
#' given height and breadth from a classic matrix. If the corresponding
#' sub-matrix extracted is not dyadic, the returned result will be wrong.
#' @param mat A dyadic matrix with the classic R matrix representation.
#' @param type string, one of the following character strings: \code{horiz},
#' \code{vert},\code{symm}, and \code{asymm}, which indicates the type of dyadic
#' object to be extracted;
#' @param height The height of the dyadic matrix.
#' @param breadth The breadth of the dyadic matrix.
#' @return A \code{Dyadic} object of the input type, height, and breadth
#' representing the input matrix.
#' @details This function converts a dyadic matrix of the classic matrix
#' form into the corresponding \code{Dyadic} object. If the input matrix is
#' not dyadic it extracts the entries for the dyadic structure of the given
#' height and breadth that fits to the upper-left hand side corner. Entries
#' outside the fitted dyadic structure are neglected even if they are not
#' equal to zero.
#' @export
#'
#' @seealso \code{\link{Dyadic-class}} for a description of the class;
#' @example R/Examples/ExAsDyadic.R
#'

as.dyadic <- function(mat, type, height, breadth) {
    d <- (2^height - 1) * breadth
    if ((dim(mat)[1] < d) || (dim(mat)[2] < d)) {
        stop("The shape of the matrix does not meet requirement!\n")
    }
    if (!type %in% c("vert", "horiz", "symm", "asymm")) {
        stop("The 'type' is not properly specified, should be one of the following: 'horiz', 'vert', 'symm', 'asymm'.\n")
    }

    if (dim(mat)[1] > d || dim(mat)[2] > d) {
        message(paste0("The input height and breadth correspond to a smaller matrix than the input one.
        The upper-left ", d, "x", d, " submatrix will be extracted."))
        mat <- mat[1:d, 1:d]
    }

    if (type != "asymm") {
        return(new("Dyadic",
            height = height, breadth = breadth, type = type,
            entries = rcpp_as_dyadic(mat, height, breadth, type)
        ))
    } else {
        result_list <- rcpp_as_dyadic(mat, height, breadth, type)
        return(new("Dyadic",
            height = height, breadth = breadth, type = type,
            entries = result_list[[1]], aentries = result_list[[2]]
        ))
    }
}
