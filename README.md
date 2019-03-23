# scbwrangler

**This is early work. I do not recommend using this package. But come back later!** 

Collection of functions for wrangling data from SCB (Swedish Statistics). Based on what I repeatedly  need to do myself but don't want to repeat... Ever... Currently the package: Loads directories with .px and .csv files into data frames. Converts variable names of data frames into English (or other languages, you can define a custom dictionary). Reclasses variables based on a supplied "dictionary" (since SCB has a proclivity for factors, (but I sure don't), in their .px files). 

Future plans: Add a capacity for handling missing and common inconsistencis in data as well as some standard validation. Convenience functions for generating useful codebooks. Functions that make the labelling of standard data more convenient - espcecially for cases where there is a need to cooperate with SPSS or STATA usesrs. 

Overall the aim is to get some super-easy tools that automates actions that, while no big deal for those with more experience, are major hurdles for novices simply looking to run some regressions. 
    
## Installing

Run the following in R:
```
install.packages("devtools")
devtools::install_github("pfesjostrand/scbwrangler")
```

### Prerequisites

Makes a broad use of the tidyverse. The package also use here and pxR. see DESCRIPTION.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details. 


