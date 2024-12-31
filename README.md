# DyadiCarma

An efficient R package for working with dyadic matrices, implemented using Rcpp and RcppArmadillo. The package provides basic arithmetic and matrix operations for dyadic matrices and implements a fast dyadic algorithm that exploits the dyadic (or band) structure of specific matrices. This enables efficient factorization and inversion of dyadic matrices, offering a powerful tool for computational applications involving sparse positive definite matrices.

**Installation**

```R
library(devtools)
install_github("slangevar/DyadiCarma")
```

**Bibliography**

Kos, M., Podgórski, K., & Wu, H. (2024). Efficient inversion of sparse positive definite matrices through a dyadic factorization. Manuscript in preparation.

**Authors**

- Hanqing Wu (Maintainer)
- Krzysztof Podgórski
