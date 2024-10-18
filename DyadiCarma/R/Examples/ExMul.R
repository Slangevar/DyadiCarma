#--------------------------------------------------------#
#------- Scalar multiplication of dyadic objects --------#
#--------------------------------------------------------#

N <- 4
k <- 3

# Construct four types of dyadic matrices with made of 1's
V <- construct(N, k, type = "vert") # vertical

mat_V <- as.matrix(V)

as.matrix(2 * V) == 2 * mat_V
as.matrix(V * 2) == 2 * mat_V
