# DyadiCarma

## Configuration of the NAMESPACE

It may not be a good pratice to use `exportPattern("^[[:alpha:]]+"` in the NAMESPACE file since it will export everything, including C++ auxiliary functions that we want to hide. It is better to configure the NAMESPACE file manually to control what functions to export. If we remove the comment on the first line of the NAMESPACE file, then roxygen2 will not modify it in later compilation.
