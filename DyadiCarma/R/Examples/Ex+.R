#--------------------------------------------------------#
#------- Algebraic operations on dyadic matrices --------#
#--------------------------------------------------------#

# Construct four types of dyadic matrices with made of 1's
V <- construct(N, k, type = "vert") # vertical
H <- construct(N, k, type = "horiz") # horizontal
S <- construct(N, k, type = "symm") # symmetric
AS <- construct(N, k, type = "asymm") # asymmetric

# Addition of dyadic matrices

HpV=H+2*V # horizontal + vertical = asymmetric
HpV@type
HpAS=3*t(H)-6*t(AS) #transpose and linear combination


