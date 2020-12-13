
#' Calculate percentage and number of missing values for each variables
#'
#' @param df This is a dataframe
#' @export
#' @return The percentage and number of NAs for each variable
#' @importFrom dplyr summarise_all
#' @importFrom tidyr gather

CalculateNA = function(df){
  missing_data.p = dplyr::summarise_all(df,dplyr::funs(sum(is.na(df))/length(df)))
  missing_data.n = dplyr::summarise_all(df,dplyr::funs(sum(is.na(df))))
  missing_data.p = tidyr::gather(missing_data.p, key = "variables", value = "percent_of_missing")
  missing_data.n = tidyr::gather(missing_data.n, key = "variables", value = "numbers_of_missing")
  missing_data = merge(missing_data.n,missing_data.p)
  print(missing_data)
}
