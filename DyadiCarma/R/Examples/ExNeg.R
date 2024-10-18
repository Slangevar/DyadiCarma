#----------------------------------------------#
#--------- Negation of dyadic objects ---------#
#----------------------------------------------#

N <- 4
k <- 3

# Construct four types of dyadic matrices with made of 1's
V <- construct(N, k, type = "vert") # vertical
NV <- -V

mat_V <- as.matrix(V)
mat_NV <- as.matrix(NV)

max(abs(mat_V + mat_NV)) == 0 # Should be 0
