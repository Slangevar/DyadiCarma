#--------------------------------------------------#
#------- Multiplication of dyadic matrices --------#
#--------------------------------------------------#

N <- 4
k <- 3

# Construct four types of dyadic matrices with made of 1's
V <- construct(N, k, type = "vert") # vertical
H <- construct(N, k, type = "horiz") # horizontal
S <- construct(N, k, type = "symm") # symmetric
AS <- construct(N, k, type = "asymm") # asymmetric

# Convert the dyadic matrices to matrix format
mat_V <- as.matrix(V)
mat_H <- as.matrix(H)
mat_S <- as.matrix(S)
mat_AS <- as.matrix(AS)

# Multiplication of dyadic matrices
VV <- V %*% V # vertical * vertical = vertical
HH <- H %*% H # horizontal * horizontal = horizontal
HS <- H %*% S # horizontal * symmetric = asymmetric
HV <- H %*% V # horizontal * vertical = asymmetric
ASV <- AS %*% V # asymmetric * vertical = asymmetric

VH <- V %*% H # vertical * horizontal = non-dyadic
VS <- V %*% S # vertical * symmetric = non-dyadic
VAS <- V %*% AS # vertical * asymmetric = non-dyadic

SS <- S %*% S # symmetric * symmetric = non-dyadic
ASAS <- AS %*% AS # asymmetric * asymmetric = non-dyadic
ASH <- AS %*% H # asymmetric * horizontal = non-dyadic

dim(ASAS) # regular matrix
