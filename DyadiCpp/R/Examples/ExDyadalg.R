#-------------------------------------------------------------#
#---------Inverting a PD symmetrically dyadic matrix----------#
#-------------------------------------------------------------#

N <- 4
k <- 3

# A 48x48 vertically dyadic matrix
V <- construct(N, k, type = "vert", distr = "unif")
# A 48x48 symmetrically dyadic matrix
S <- t(V) %*% V
S@type <- "symm"
S@aentries = list() # Convert S from "asymm" to "symm"

# Find the vertically dyadic matrix that satisfies P^T S P = I
# using a dyadic factorization algorithm.
P <- dyadalg(S)
I1 <- as.matrix(t(P) %*% S %*% P)
I <- diag(dim(I1)[1])
max(abs(I1 - I)) # Should be trivially small

# Obtain the inverse of S via the dyadic algorithm
iS <- dyadalg(S, inv = TRUE)
I2 <- iS %*% as.matrix(S)
max(abs(I2 - I)) # Should be trivially small
