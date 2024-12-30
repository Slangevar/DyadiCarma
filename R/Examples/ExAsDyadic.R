#-------------------------------------------------------------#
#------------Creating vertically dyadic matrices--------------#
#-------------------------------------------------------------#

N <- 4
k <- 3
d <- k * (2^N - 1)

mat1 <- matrix(0, nrow = d, ncol = d)
mat2 <- matrix(0, nrow = d, ncol = d)

for (i in 1:N) {
    st_col_id <- (2^(i - 1) - 1) * k + 1
    en_col_id <- (2^(i - 1) - 1) * k + k
    for (j in 1:2^(N - i)) {
        st_row_id <- st_col_id - (2^(i - 1) - 1) * k
        en_row_id <- en_col_id + (2^(i - 1) - 1) * k
        mat1[st_row_id:en_row_id, st_col_id:en_col_id] <-
            as.matrix(rnorm((2^i - 1) * k^2), ncol = k, nrow = (2^i - 1) * k)
        mat2[st_row_id:en_row_id, st_col_id:en_col_id] <-
            as.matrix(rnorm((2^i - 1) * k^2), ncol = k, nrow = (2^i - 1) * k)
        st_col_id <- st_col_id + 2^i * k
        en_col_id <- en_col_id + 2^i * k
    }
}

mat1
mat2

#-------------------------------------------------------------#
#----------Creating corresponding dyadic objects--------------#
#-------------------------------------------------------------#

V1 <- as.dyadic(mat1, "vert", N, k) # A "vert" dyadic object
V2 <- as.dyadic(mat2, "vert", N, k) # A "vert" dyadic object

mat1S <- t(mat1) %*% mat1 # A symmetrically dyadic matrix
mat1AS <- t(mat2) %*% mat1 # An asymmetrically dyadic matrix
S <- as.dyadic(mat1S, "symm", N, k) # A "symm" dyadic object
AS <- as.dyadic(mat1AS, "asymm", N, k) # A "asymm" dyadic object

all(as.matrix(S) == mat1S) # Should be TRUE.
all(as.matrix(AS) == mat1AS) # Should be TRUE.


#-------------------------------------------------------------#
#---------------Creating a non-dyadic matrices----------------#
#-------------------------------------------------------------#

mat3 <- diag(d + 5)
mat3[1:d, 1:d] <- mat1

V3 <- as.dyadic(mat3, "vert", N, k) # Extract the upper-left dxd dyadic sub-matrix
all(as.matrix(V3) == mat1) # Should be TRUE.
