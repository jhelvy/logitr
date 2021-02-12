# logitr 0.1.0.9000

## Summary of larger updates:



## Summary of smaller updates:



## Bugs

- If tidyverse library is loaded, data frames were getting converted to tibbles, which broke some things. Fixed this by forcing the input data to be a data.frame()
