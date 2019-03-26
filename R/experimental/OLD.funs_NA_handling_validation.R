#*****************************************************************************#
# FILE DESCRIPTION: FUNCTIONS FOR VALIDATION AND HANDLING MISSING VALUES
#
# TODO convenience functions for validating common SCB data
#
# Author: Filip Sjöstrand (pfesjostrand@gmail.com , +46-702-745911)
# Last edit: 25/03/2019
#
#*****************************************************************************#

# EXPERIMENTAL DON'T EXPORT
#' ccheck (for count check)
#'
#' Function meant to be used alongside other validation tools in a pipe such as
#' assortr. ccheck counts a specified 'noun' (currently NA, rows or zero values)
#' in a inputed dlist or data frame. By default the count is then stored in the
#' parent enviroment (i.e. as a side effect) and the input is returned (passed)
#' along the pipe. The count canbe checked down the pipe by simple expressions,
#' for example has the number of NA increased after some operations?
#'
#' @param input Either a dlist object or a data frame
#' @param noun  nNA for NA count, nrows for row count, nzero for zero count.
#' @param pipe  True by default, set to false to use outside of pipe.
#' @param sum   True by default, set to false to return element wise count.
#'
#' @return returns the input and, as a side-effect, the CCMEM var with count.
ccheck <- function(input, exportname = "CCMEM", pipe = T, sum = T) {

  # FUNCTION DEFINITIONS

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

  # # functions for doing all
  # all_df <- function(input) {
  #   outputlist$nNA    <- count_df_NA(input)
  #   outputlist$nzero  <- count_df_zero(input)
  #   outputlist$nrow   <- nrow(input)
  #   return(outputlist)
  #   }
  # all_dlist <- function(input) {
  #   outputlist$nNA    <- count_dlst_NA(input)
  #   outputlist$nzero  <- count_dlst_zero(input)
  #   outputlist$nrow   <- count_dlst_nrow(input)
  #   return(outputlist)
  # }

  # EXECUTION

  # The options available by inserting different arguments into noun
  options <- c("nNA", "nrows", "nzero")
  stopifnot(!is.na((match(noun, options))))
  stopifnot(is.data.frame(input) | is.list(input))

  # Uses switch to call the correct function based on if the input is a
  # list or a data frame and the option entered into noun.
  if(class(input)[1] == "tbl_df" | class(input)[1] == "data.frame") {
    switch(match(noun, options),
      output <- count_df_NA(input),
      output <- nrow(input),
      output <- count_df_zero(input),
    )
  }
  if(all((!is.data.frame(input[1])) & (class(input[1]) == "list"))) {
    switch(match(noun, options),
      output <- count_dlst_NA(input),
      output <- count_dlst_nrow(input),
      output <- count_dlst_zero(input),
    )
  }

  # By default the sum is calculated giving a response for the whole data
  # frame or the whole list rather than a column or data frame specifc count.
  if(sum  == TRUE) {
    output <- sum(output)
  }

  # By default pipe setting is TRUE the output is returned as SIDE EFFECT in a
  # list which can be checked and the input is returned (passed along the pipe)
  if(pipe == FALSE) {
    return(output)} # << Function stops here if pipe == FALSE

  outputlist <- vector("list", length = 3)
  names(outputlist) <- c("nNA", "nrow", "nzero")
  switch(match(noun, options),
    outputlist$nNA   <- output,
    outputlist$nrow  <- output,
    outputlist$nzero <- output
  )

  # Exports outputlist to a variable (name set bt exportname) in parent frame.
  assign(exportname, outputlist, pos = parent.frame())

  # Passes along unchanged input in pipe
  return(input)

  }
