#--------------------------------------------#
#-------Transpose of a dyadic object --------#
#--------------------------------------------#

N <- 4
k <- 3

# Construct four types of dyadic matrices with made of 1's
V <- construct(N, k, type = "vert") # vertical
H <- construct(N, k, type = "horiz") # horizontal
S <- construct(N, k, type = "symm", distr = "unif") # symmetric


V@type
H@type
t(H)@type
as.matrix(S)
as.matrix(t(S))
