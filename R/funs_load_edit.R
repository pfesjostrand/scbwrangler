#*****************************************************************************#
# Script adapted from SCB scripts and various things I had coded earlier. But
# completly rewritten on 23 march. Depends on stringr, dplyr, purrr, pxR, here
# readR . This is a "early version" and work in progress.
#
# TODO: Check, test and write error handling, collect warnings in a sensible way.
# TODO: Make dlist into its own class https://www.datamentor.io/r-programming/s3-class/ etc.
# TODO: Make fix_factors capable of automatically using "Swedish" if no "english" match exist.
#
# Author: Filip Sjöstrand (pfesjostrand@gmail.com ; 46-702-745911 )
#*****************************************************************************#

# FUN: load_directory ---------------------------------------------------------------
#' load_directory
#'
#' Read .px and .csv files in a directory into a list of data frames.
#' @param path The path to the directory that should be read.
#'
#' @return Returns a list of data frames (tibbles) containing file contents.
#' @export
load_directory <- function(path) {

  # list path of items in directory, subset those ending with .px or.csv
  dirlist <- paste(path, list.files(path), sep="/")
  dirlist_px  <- stringr::str_subset(dirlist, "\\.px$")
  dirlist_csv <- stringr::str_subset(dirlist, "\\.csv$")

  # get the names of items in directory that end with .px or .csv
  namelist_px  <- stringr::str_subset(list.files(path), "\\.px$")
  namelist_csv <- stringr::str_subset(list.files(path), "\\.csv$")

  # define two functions for reading files and transforming into tibble
  # and a list vector that will be part of the function return.
  pxread  <- function(path) tibble::as_tibble(pxR::read.px(path))
  csvread <- function(path) tibble::as_tibble(readr::read_csv(path))
  data <- tibble::lst()

  # read all .px files into data and check all .px items are read then read
  # all .csv files onto the end of data and check all .csv imes are read.
  if (length(dirlist_px) != 0) {
    data[1:length(dirlist_px)] <- purrr::map(dirlist_px, pxread)
    names(data)[1:length(dirlist_px)] <- namelist_px
    if (all(names(data) != namelist_px)) {
    warning("Warning: Some .px files seem to have not been read. Check.")
    }
  }

  if (length(dirlist_csv) != 0)  {
    ind1 <-  length(data) + 1
    ind2 <- (ind1 + length(dirlist_csv) - 1)
    data[ind1:ind2] <- purrr::map(dirlist_csv, csvread)
    names(data)[ind1:ind2] <- namelist_csv
    if (all(names(data[ind1:ind2]) != namelist_csv)) {
      warning("Warning: Some .csv files seem to have not been read. Check.")
    }
  }

  return(data)

}

# FUN: translate_names --------------------------------------------------------

#' translate_names
#'
#' Translates the names of a data_frame using a specified dictionary. The
#' dictionary can be either a data frame or a path to a .csv file. In either
#' case the dictionary should be made up of two colums the first with the
#' language to translate from and the second with the language to translate
#' into.
#'
#' @param data Data frame object of which the names are to be translated
#' @param dictionary Either a path to a .csv file or a data frame object. If no
#' dictionary is specified a fallback dictionary is used with a warning.
#' @param reverse Setting this to true everses the order of the languages,
#' works only if there is a 1:1 mapping, not recommended.
#'
#' @return returns the data_frame, hopefully with translated names.
#' @export
translate_names <- function(data, dictionary = "default", reverse = FALSE) {

  # if dictionary is default a fallback set is used, if it is a character this is
  # treated as a path if it is a data_frame this is used directly as dictionary.
  if (is.data.frame(dictionary) == FALSE) {

    if (dictionary == "default") {
      dictionary <- suppressMessages(readr::read_csv(here::here(
        "data", "translate_names_default_dictionary_swe_to_eng.csv")))
      warning("No dictionary specified, fallback to default")

    } else {
      stopifnot(class(dictionary) == "character")
      stopifnot(file.exists(dictionary) == TRUE)
      dictionary <- suppressMessages(readr::read_csv(dictionary))
    }
  }

  # reverse allows reversing the dictionary if there is a 1:1 mapping between
  # languages. There should be a error check for this but there isn't atm.
  if (reverse == FALSE) {
    fr <- 1L
    to <- 2L
  } else {
    fr <- 2L
    to <- 1L
  }

  # retrive the names of the data, match these to the dictionary column[fr]
  # if no match exists NA will be entered instead.
  varnames <- names(data)
  translat <- dictionary[[to]][match(varnames, dictionary[[fr]])]

  # map2_chr walks along translat applying nareplace to each element, which
  # checks if element is NA and in that case replace with original name.
  nareplace <- function(arg1, arg2) {
    if(is.na(arg1)) {
      return(arg2)
    } else {
      return(arg1)
    }
  }

  translat <- purrr::map2_chr(translat, varnames, nareplace)

  # replace names with translated names and retrun data frame
  names(data) <- translat
  return(data)
}

# FUN: fix_factors ------------------------------------------------------------
# Intended to take output from translate_names. Data in SCB .px files generally
# comes in factors if not strictly numeric; years are for example factors not
# integers. This is at times a nuisence, fix_factors recode common variables
# into appropiate class. Also fixes removes "år" in age data. TODO: add a dic
# funcionality where you can input a custom tibble or csv with name and class.

#' fix_factors
#'
#' Data in SCB .px files often comes in factors if the data is not strictly
#' numeric. E.g years are coded as factors not integers. This at times (often)
#' a nuisance. fix_factors change the class based on the names of the variable
#' using a dictionary mapping class to name. The "classdictionary" can either
#' have two colums named english (due to this function being intended to be
#' applied after translate_names) and one called class or define which colums
#' should be used with fr (from) and to; either by index or by column name.
#'
#' This function works but could do with a little more work...
#'
#' @param data Data frame object of which the vars are to be reclassed.
#' @param classdic Either a path to a .csv file or a data frame object.
#' @param fr The column in classdic where the names are found
#' @param to The column to classdic where class mappings are found
#'
#' @return Returns a reclassed data frame
#' @export
fix_factors <- function(data, classdic = "default",  fr = 2, to = 3) {

  # Optionally a data_frame can be used for class defintions; if not a classdic
  # is treated as a path , if set to default it uses the same small dictionary
  # as translate_names, which contain class settings for some common SCB vars.
  if (is.data.frame(classdic) == FALSE) {
    if (classdic == "default") {
      classdic <- suppressMessages(readr::read_csv(here::here(
        "data", "translate_names_default_dictionary_swe_to_eng.csv")))
      warning("No dictionary specified, fallback to default")
    } else {
      stopifnot(class(classdic) == "character")
      stopifnot(file.exists(classdic) == TRUE)
      classdic <- suppressMessages(readr::read_csv(classdic))
    }
  }

  # this enables some variation in spelling etc. I would prefer it could handle
  # more misspellings; that could be possible using stringr but atm this is OK.
  date <- c("da", "dat", "Date", "date", "dates", "Dates")
  intr <- c("i", "int", "Int", "integer", "Integer", "integers", "Integers")
  numr <- c("n", "num", "Num", "numeric", "Numeric", "numerics", "Numerics")
  dubl <- c("d", "dbl", "Dbl", "dubble", "dubbl",
    "Dubbl", "Dubble", "dubbles", "Dubbles")
  fact <- c("f", "fct", "fctr", "factor", "Factor", "factors", "Factors")
  ordr <- c("o", "ord", "ordr", "order", "ordered", "Ordered")
  chrs <- c("c", "chr", "Chr", "character", "Character", "characters",
    "Characters", "str", "Str", "string", "String" )

  # retrive the names of the data, match these to the classdic column given
  # by "to", if no match exists NA will be entered instead, store in classmatch
  varnames <- names(data)
  classmatch <- classdic[[to]][match(varnames, classdic[[fr]])]

  # hardcoded thing to fix "1 år", "2 år" ("1 years", "2 years") in age var.
  fix_age_char <- function(y) {

    if(!is.character(y)){
      return(y)
    }

    if(all(str_detect(y[1:3], "år")) == TRUE) {
      y <- str_trim(str_remove(str_remove(y, "år"), "\\+"))
      return(y)
    }
    if(all(str_detect(y[1:3], "years")) == TRUE) {
      y <- str_trim(str_remove(str_remove(y, "years"), "\\+"))
      return(y)
    }

    return(y)
  }

  # check if a classmatch element is found in any of the variation of class
  # names defined above. If so the the data column corresponding to that
  # classmatch is converted to the appropiate class. Long function follow.
  classconvert <- function(x, y) {

    ctrT  <- 0

    if(is.na(x)) {
      warning("Instance of no match occured due to NA in classmatch")
      return(y)
    }

    if(class(y) == "factor" & (x %in% fact == FALSE)) {
      y <- as.character(y)
    }

    # fixes "1 år", "2 years" probably not super robust.
    y <- fix_age_char(y)

    if(x %in% intr) {
      ctrT <- ctrT + 1
      y <- as.integer(y)
    }

    if(x %in% numr) {
      ctrT <- ctrT + 1
      y <- as.numeric(y)

    }

    if(x %in% chrs) {
      ctrT <- ctrT + 1
      y <- as.character(y)
    }

    if(x %in% dubl) {
      ctrT <- ctrT + 1
      y <- as.double(y)
    }

    if(x %in% fact) {
      ctrT <- ctrT + 1
      y <- as.factor(y)
    }

    if(x %in% ordr) {
      ctrT <- ctrT + 1
      y <- as.ordered(y)
    }

    if(x %in% date) {
      ctrT <- ctrT + 1
      y <- as.Date(y)
    }

    if(ctrT < 1) {
      warning("Instance of no match occured but no NA in classmatch")
    }

    if(ctrT > 1) {
      stop("Error: A double match occured")
    }

    return(y)
  }

  # in this case it seems to be better to use a for loop than purrr: loop over
  # the length of classmatch applying classconvert() to correspond col in data.
  i <- 0
  for(i in seq_along(classmatch)) {
    data[[i]] <- classconvert(classmatch[i], data[[i]])
  }

  return(data)

}



## EXPERIMENTAL NOT EXPORTED

# FUN: load_directory_dlist ---------------------------------------------------
#' load_directory
#'
#' Read .px and .csv files in a directory into a list of data frames.
#' @param path The path to the directory that should be read.
#'
#' @return Returns a list of data frames (tibbles) containing file contents.
load_directory <- function(path, dlist = FALSE) {

  # list path of items in directory, subset those ending with .px or.csv
  dirlist <- paste(path, list.files(path), sep="/")
  dirlist_px  <- stringr::str_subset(dirlist, "\\.px$")
  dirlist_csv <- stringr::str_subset(dirlist, "\\.csv$")

  # get the names of items in directory that end with .px or .csv
  namelist_px  <- stringr::str_subset(list.files(path), "\\.px$")
  namelist_csv <- stringr::str_subset(list.files(path), "\\.csv$")

  # define two functions for reading files and transforming into tibble
  # and a list vector that will be part of the function return.
  pxread  <- function(path) tibble::as_tibble(pxR::read.px(path))
  csvread <- function(path) tibble::as_tibble(readr::read_csv(path))
  data <- tibble::lst()

  # read all .px files into data and check all .px items are read then read
  # all .csv files onto the end of data and check all .csv imes are read.
  if (length(dirlist_px) != 0) {
    data[1:length(dirlist_px)] <- purrr::map(dirlist_px, pxread)
    names(data)[1:length(dirlist_px)] <- namelist_px
    if (all(names(data) != namelist_px)) {
      warning("Warning: Some .px files seem to have not been read. Check.")
    }
  }

  if (length(dirlist_csv) != 0)  {
    ind1 <-  length(data) + 1
    ind2 <- (ind1 + length(dirlist_csv) - 1)
    data[ind1:ind2] <- purrr::map(dirlist_csv, csvread)
    names(data)[ind1:ind2] <- namelist_csv
    if (all(names(data[ind1:ind2]) != namelist_csv)) {
      warning("Warning: Some .csv files seem to have not been read. Check.")
    }
  }

  # By default function returns here; I was experimenting with the below
  if(dlist == FALSE) {
    return(data)
  }

  # constructs a list and attaches data and additional records
  output <- tibble::lst()

  # data
  output[[1]]        <- data
  names(output)[1]   <- "data"

  # empty used by other functions
  output[[2]]        <- tibble::lst()
  names(output)[2]   <- "changed_vars"
  output[[2]][[1]]      <- tibble::lst()
  names(output[[2]])[1] <- "unique"
  output[[2]][[2]]      <- tibble::lst()
  names(output[[2]])[2] <- "details"

  # record of original variables
  output[[3]]        <- tibble::lst()
  names(output)[3]   <- "original_vars"
  output[[3]][[1]]      <- unique(unlist(purrr::map(data, names)))
  names(output[[3]])[1] <- "unique"
  output[[3]][[2]]      <- purrr::map(data, function(x) purrr::map_chr(x, class))
  names(output[[3]])[2] <- "details"

  # record of source paths
  output[[4]]        <- tibble::lst()
  names(output)[4]   <- "sources"
  output[[4]][[1]]      <- dirlist_px
  names(output[[4]])[1]  <- "paths_px"
  output[[4]][[2]]      <- dirlist_csv
  names(output[[4]])[2]  <- "paths_csv"

  # return the list
  return(output)
}


# FUN: translate_names_dlist --------------------------------------------------------
#' translate_names_dlist
#'
#' dlist version of translate_names
#'
#' @param dlist dlist object such as returned by load_directory
#' @param dictionary Either a path to a .csv file or a data frame object. If no
#' dictionary is specified a fallback dictionary is used with a warning.
#' @param reverse Setting this to true everses the order of the languages,
#' works only if there is a 1:1 mapping, not recommended.
#'
#' @return updated dlist
translate_names_dlist <- function(dlist, dictionary = "default", reverse = FALSE) {
  dlist$data <- purrr::map(dlist$data, translate_names, dictionary, reverse)

  dlist[[2]][[1]] <- unique(unlist(purrr::map(dlist$data, names)))
  dlist[[2]][[2]] <- purrr::map(dlist$data, function(x) purrr::map_chr(x, class))

  return(dlist)
}

# FUN: fix_factors_dlist --------------------------------------------------------
#' fix_factors_dlist
#'
#' dlist version of fix_factors
#'
#' @param dlist dlist object such as returned by load_directory and translate_names_dlist
#' @param classdic Either a path to a .csv file or a data frame object.
#' @param fr The column in classdic where the names are found
#' @param to The column to classdic where class mappings are found
#'
#' @return updated dlist
fix_factors_dlist <- function(dlist, classdic = "default",  fr = "english", to = "class") {
  dlist$data <- purrr::map(dlist$data, fix_factors, classdic,  fr, to)

  dlist[[2]][[2]] <- purrr::map(dlist$data, function(x) purrr::map_chr(x, class))

  return(dlist)
}

