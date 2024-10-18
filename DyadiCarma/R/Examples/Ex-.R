#----------------------------------------------#
#------- Subtraction of dyadic objects --------#
#----------------------------------------------#

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

# Subtract two dyadic matrices

HpAS <- 3 * t(H) - 6 * t(AS) # transpose and linear combination
HpAS@type # check the type of the result
