#-------------------------------------------------------------#
#---Building 'Dyadic' objects of arbitrary types and sizes ---#
#-------------------------------------------------------------#
N <- 4
k <- 3 # the height and breadth of a dyadic matrix

# Nonrandom vertical dyadic matrix with entries equal to 1
S <- construct(N, k)

S@entries[[N]] # The top level entries
S@entries[[1]] # The bottom level entries

S@type <- "horiz"
# 'S' becomes horizontaly dyadic matrix,
# which is the transpose of the original object

# Symmetric dyadic with entries equal to 1
SS <- construct(N, k, type = "symm")
SS@entries[[2]] # The second bottom level entries

SS@aentries # This list is empty whenever the type is not 'asymm'

# Asymmetric dyadic with entries equal to one
AS <- construct(N, k, type = "asymm")
AS@entries[[2]] # The second bottom level entries
AS@aentries[[2]]
# The asymmetric version
# (which happens to be also symmetric in this case)

# Truly asymmetric
AS <- construct(N, k, type = "asymm", distr = "unif")
AS@entries[[2]] # The second bottom level entries
AS@aentries[[2]] # The second bottom level asymmetric entries
