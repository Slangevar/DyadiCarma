#-------------------------------------------------------------#
#---------Inverting a PD symmetrically dyadic matrix----------#
#-------------------------------------------------------------#

N <- 4
k <- 3

# A 45x45 vertically dyadic matrix
V <- construct(N, k, type = "vert", distr = "unif")
# A 45x45 symmetrically dyadic matrix
S <- t(V) %*% V
S@type <- "symm"
S@aentries <- list() # Convert S from "asymm" to "symm"

# Check what S looks like
matS <- as.matrix(S)
matS

# Find the vertically dyadic matrix that satisfies P^T S P = I
# using a dyadic factorization algorithm.
P <- dyadalg(S)
I1 <- as.matrix(t(P) %*% S %*% P)
I <- diag(dim(I1)[1])
max(abs(I1 - I)) # Should be trivially small

# Obtain the inverse of S via the dyadic algorithm
iS <- dyadalg(S, inv = TRUE)
I2 <- iS %*% matS
max(abs(I2 - I)) # Should be trivially small


#-------------------------------------------------------------#
#-----------------Inverting a PD band matrix------------------#
#-------------------------------------------------------------#

d <- k * (2^N - 1)
half_B <- matrix(0, nrow = d, ncol = d)
for (i in 1:d) {
    half_B[i, i:min(d, (i + k - 1))] <- rnorm(min(d, (i + k - 1)) - i + 1, mean = N, sd = 1 / N)
}
matB <- t(half_B) %*% half_B # matB is a PD band matrix with half bandwidth 3.

# Convert matB into a dyadic object B
B <- as.dyadic(matB, "symm", N, k)
iB <- dyadalg(B, inv = TRUE)
I <- diag(dim(matB)[1])
max(abs(iB %*% matB - I)) # Should be trivially small

iB_band <- dyadalg(B, inv = TRUE, band = TRUE)
max(abs(iB_band %*% matB - I)) # Should be trivially small

iB <- dyadalg(B)
iB_band <- dyadalg(B, band = TRUE)

max(abs(as.matrix(iB) - as.matrix(iB_band))) # Should be trivially small
