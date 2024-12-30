#--------------------------------------------#
#-------Transpose of a dyadic object --------#
#--------------------------------------------#

N <- 4
k <- 3

# Construct four types of dyadic matrices with made of 1's
V <- construct(N, k, type = "vert") # vertical
H <- construct(N, k, type = "horiz") # horizontal
S <- construct(N, k, type = "symm", distr = "unif") # symmetric

t(V)@type # The transpose of a vertical dyadic matrix is horizontal
t(H)@type # The transpose of a horizontal dyadic matrix is vertical

all(as.matrix(t(V)) == t(as.matrix(V))) # Should be TRUE
all(as.matrix(S) == as.matrix(t(S))) # Should be TRUE
