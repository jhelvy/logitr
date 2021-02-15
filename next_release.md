# logitr 0.1.1.9000

## Summary of larger updates:



## Summary of smaller updates:

- Re-defined the wtp space utility models as B*X - p. Before it was p + B*X and p was re-defined as -1*p.


## Bugs

- If tidyverse library is loaded, data frames were getting converted to tibbles, which broke some things. Fixed this by forcing the input data to be a data.frame()
