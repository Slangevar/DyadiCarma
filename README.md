# DyadiCarma

An efficient R package for working with dyadic matrices, implemented using Rcpp and RcppArmadillo. The package provides basic arithmetic and matrix operations for dyadic matrices and implements a fast dyadic algorithm that exploits the dyadic (or band) structure of specific matrices. This enables efficient factorization and inversion of dyadic matrices, offering a powerful tool for computational applications involving sparse positive definite matrices.

### Installation

```R
library(devtools)
install_github("slangevar/DyadiCarma")
```

### Update Log

- **v1.0.1** (2025-05-27): Fixed undefined-behavior in `rcpp_bandalg_core.cpp` by guarding against negative shift counts in the dyadic loop. No user-facing changes.

### Bibliography

Kos, M., Podgórski, K., & Wu, H. (2025). Dyadic Factorization and Efficient Inversion of Sparse Positive Definite Matrices. arXiv. https://arxiv.org/abs/2505.08144

### Authors

- Hanqing Wu (Maintainer)
- Krzysztof Podgórski
