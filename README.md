# scbwranglr

**This is early work. I do not recommend using this package. Come back later.** 

Collection of functions for wrangling data from SCB (Swedish Statistics). Based on what I repeatedly  need to do myself but don't want to repeat... Ever... Currently the package loads directories with .px and .csv files into data frames. Converts data frame variable names into English (or sother language as you can define a custom dictionary); reclasses variables based on a definable "dictionary" (since SCB has a proclivity for factors in their .px files). 

Future plans: Add a capacity for handling missing and common incosistencies in data as well as some standard validation. The goal is to get some super-easy tools that automates actions that while no 
big deal for the more experience is tricky for novices.
    
## Installing

Run the following in R:

```
install.packages("devtools")
devtools::install_github("username/packagename")
```

### Prerequisites

Relies broadly on the tidyverse also uses here and pxR , see DESCRIPTION.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details


