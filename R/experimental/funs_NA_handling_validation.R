#*****************************************************************************#
# EXPERIMENT
#
# FILE DESCRIPTION: FUNCTIONS FOR VALIDATION AND HANDLING MISSING VALUES
#
# TODO convenience functions for validating common SCB data
#
# Author: Filip Sjöstrand (pfesjostrand@gmail.com , +46-702-745911)
# Last edit: 25/03/2019
#
#*****************************************************************************#


## EXPERIMENTAL DON'T EXPORT
#' ccheck (for count check)
#'
#' Function meant to be used alongside other validation tools in a pipe such as
#' assortr. ccheck counts a specified 'noun' (currently NA, rows or zero values)
#' in a inputed dlist or data frame. By default the count is then stored in the
#' parent enviroment (i.e. as a side effect) and the input is returned (passed)
#' along the pipe. The count canbe checked down the pipe by simple expressions,
#' for example has the number of NA increased after some operations?
#'
#' @param input Either a dlist object or a data frame.
#' @param exportname The name, in quotes, of the export variable.
#' @param pipe  True by default, set to false to use outside of pipe.
#' @param sum   True by default, set to false to return element wise count.
#'
#' @return returns the input and, as a side-effect, the CCMEM var with count.
ccset <- function(input, exportname = "CCMEM", pipe = T, sum = T) {

  stopifnot(is.data.frame(input) | is.list(input))

  # function definitions:

  # counts a specified element (this) in a vector.
  count_this       <- function(vector, this) {
    x <- vector[vector == this]
    x <- x[!is.na(x)]
    length(x)
  }

  # counts the number of zeros in df by mapping count_this with arg 0
  count_df_zero    <- function(data) sum(map_int(data, count_this, 0))
  count_dlst_zero <- function(input) purrr::map_int(input, count_df_zero)

  # counts the number of NAs in data frame using base functions
  count_df_NA      <- function(data) sum(is.na.data.frame(data))
  count_dlst_NA   <- function(input) purrr::map_int(input, count_df_NA)

  # counts the number of rows in all data frames using base functions
  count_dlst_nrow <- function(input) purrr::map_int(input, nrow)

  # execution of functions:

  # output is a list
  output <- vector("list", length = 3)
  names(output) <- c("nNA", "nzero", "nrow")

  # handles both tables and lists
  if(class(input)[1] == "tbl_df" | class(input)[1] == "data.frame") {
      output$nNA   <- count_df_NA(input)
      output$nzero <- count_df_zero(input)
      output$nrow  <- nrow(input)

  }
  if(all((!is.data.frame(input[1])) & (class(input[1]) == "list"))) {
      output$nNA   <- count_dlst_NA(input)
      output$nzero <- count_dlst_zero(input)
      output$nrow  <- count_dlst_nrow(input)
  }

  # by default the sum is calculated giving a response for the whole data
  # frame or the whole list rather than a column or data frame specifc count.
  if(sum  == TRUE) {
    output$nNA   <- sum(output$nNA)
    output$nzero <- sum(output$nzero)
    output$nrow  <- sum(output$nrow)
  }

  # By default pipe setting is TRUE and the output is exported as a side effect
  # if pipe set to FALSE the output is returned ordinarilit.
  if(pipe == FALSE) {
    return(output)} # << Function stops here if pipe == FALSE

  # Exports outputlist to a variable (name set bt exportname) in parent frame.
  assign(exportname, output, pos = parent.frame())

  # Passes along unchanged input in pipe
  return(input)

}
