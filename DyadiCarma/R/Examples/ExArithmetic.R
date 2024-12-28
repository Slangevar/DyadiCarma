#--------------------------------------------------------#
#-------- Arithmetic methods for dyadic objects ---------#
#--------------------------------------------------------#

N <- 4
k <- 3

# Construct four types of dyadic matrices with made of 1's
V <- construct(N, k, type = "vert") # vertical
H <- construct(N, k, type = "horiz") # horizontal
S <- construct(N, k, type = "symm") # symmetric
AS <- construct(N, k, type = "asymm") # asymmetric

# Negation of dyadic objects (matrices)
NegV <- -V
NegV@type
all(as.matrix(NegV) == -as.matrix(V)) # Should be TRUE

# Addition of dyadic objects (matrices)
HpV <- H + V # horizontal + vertical = asymmetric
HpV@type

# Subtraction of dyadic objects (matrices)
SmAS <- S - AS # symmetric - asymmetric = asymmetric
SmAS@type

# Scalar multiplication of dyadic objects (matrices)
DoubleV <- 2 * V # Scalar multiplication does not change the type
VDouble <- V * 2 # Scalar multiplication does not change the type
DoubleV@type
VDouble@type
all(as.matrix(DoubleV) == 2 * as.matrix(V)) # Should be TRUE
all(as.matrix(VDouble) == as.matrix(DoubleV)) # Should be TRUE

# Linear combination
linearComb <- -S + 3 * H - 6 * AS + V # linear combination of dyadic matrices
linearComb@type # "asymm"
