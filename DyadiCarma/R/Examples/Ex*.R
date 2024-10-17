#--------------------------------------------------------#
#------- Scalar multiplication of dyadic objects --------#
#--------------------------------------------------------#

# Construct four types of dyadic matrices with made of 1's
V <- construct(N, k, type = "vert") # vertical

mat_V <- as.matrix(V)

as.matrix(2*V)==2*mat_V

