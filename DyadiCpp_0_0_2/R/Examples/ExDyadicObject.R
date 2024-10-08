#-------------------------------------------------------------#
#-------Generating an object from the class 'Dyadic'--------#
#-------------------------------------------------------------#

# The most generic generation of an object of class 'Dyadic':
D <- new("Dyadic") # a generic format for 'Splinets' object
D
# The SLOTs of 'Dyadic' - the default values
D@height
D@breadth
D@type
D@entries[[1]]
D@aentries

N <- 4
k <- 3 # the height and breadth of a dyadic matrix

# The construction of a horizontally dyadic matrix with height 4 and breadth 3.

E <- list()
for (i in 1:4) {
    E[[i]] <- matrix(1, nrow = (2^(i) - 1) * 3, ncol = 2^(4 - i) * 3)
}


DD <- new("Dyadic", height = N, breadth = k, type = "horiz", entries = E)

DD

# The classic R matrix representation of DD.
mat_DD <- as.matrix(DD)
View(mat_DD)


#-------------------------------------------------------------#
#--------------Transpose of the 'Dyadic' objects--------------#
#-------------------------------------------------------------#

# Construct four types of random dyadic matrices with the same shape.
V <- construct(N, k, type = "vert", distr = "unif")
H <- construct(N, k, type = "horiz", distr = "unif")
S <- construct(N, k, type = "symm", distr = "unif")
AS <- construct(N, k, type = "asymm", distr = "unif")
mat_V <- as.matrix(V)
mat_H <- as.matrix(H)
mat_S <- as.matrix(S)
mat_AS <- as.matrix(AS)

# Transpose of the dyadic object
VT <- t(V)
VT@type # should be 'horiz'
max(abs(as.matrix(VT) - t(mat_V))) # Should be 0

HT <- t(H)
HT@type # should be 'horiz'
max(abs(as.matrix(HT) - t(mat_H))) # Should be 0

ST <- t(S)
ST@type # will still be 'symm'
max(abs(as.matrix(ST) - mat_S)) # Should be 0 due to symmetry

AST <- t(AS)
AST@type # will still be 'asymm'
max(abs(as.matrix(AST) - t(mat_AS))) # Should be 0


#-------------------------------------------------------------#
#-----------Multiplications of the 'Dyadic' objects-----------#
#-------------------------------------------------------------#

# Any pairs of the four types are supported.

# The multiplication of two vertically dyadic matrix,
# which will result in a vertically dyadic matrix
VV <- V %*% V
VV@type # Should be "vert"

# The multiplication of a horizontally dyadic matrix with a vertically dyadic one,
# which will result in an asymmetrically dyadic matrix
HV <- H %*% V
HV@type # Should be "asymm"

# The multiplication of a horizontally dyadic matrix with a symmetrically dyadic one,
# which will result in an asymmetrically dydaic matrix
HS <- H %*% S
HS@type # Should be "asymm"

# The multiplication of a vertically dyadic matrix with a horizontally dyadic one,
# the result is no longer a dyadic object but a dense d x d matrix, where d = k * (2^N - 1)
VH <- V %*% H

# The multiplication of a symmetrically dyadic matrix with a symmetrically dyadic one,
# the result is no longer a dyadic object but a dense d x d matrix, where d = k * (2^N - 1)
SS <- S %*% S

# The multiplication of a symmetrically dyadic matrix with an asymmetrically dyadic one,
# the result is no longer a dyadic object but a dense d x d matrix, where d = k * (2^N - 1)
SAS <- S %*% AS
