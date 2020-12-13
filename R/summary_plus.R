#' Advanced Summary for A Data Frame
#'
#' @description An integrated solution for descriptive analysis.
#'     This function provides three main kinds of results:
#'     1. Size, Numbers of missing value, Rows, Columns of the data frame
#'     2. Numbers and percentage of missing value for each variable.
#'     3. Other descriptive statistics for each variable.
#'
#' @param df This should be a date frame
#'
#' @return Returns Size, Numbers of missing value, Rows, Columns of the data frame.
#'     Returns missing value number and percentage and descriptive statistics for each variable.
#' @export
#'
#' @importFrom dplyr summarise_all
#' @importFrom tidyr gather
#' @importFrom tibble glimpse
#' @importFrom psych describe
#' @importFrom utils object.size



summary_plus = function(df){
  cat("Size: ")
  print(object.size(df), units = "auto")
  cat("Numbers of NA:",sum(is.na(df)),"\n")
  missing_data.p = dplyr::summarise_all(df,dplyr::funs(sum(is.na(df))/length(df)))
  missing_data.n = dplyr::summarise_all(df,dplyr::funs(sum(is.na(df))))
  missing_data.p = tidyr::gather(missing_data.p, key = "variables", value = "percent_of_missing")
  missing_data.n = tidyr::gather(missing_data.n, key = "variables", value = "numbers_of_missing")
  missing_data = merge(missing_data.n,missing_data.p)
  print(missing_data)

  cat("","\n")

  tibble::glimpse(df)
  cat("","\n")

  cat("Descriptive Statistics: ","\n")
  psych::describe(df)
}



