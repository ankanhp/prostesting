#' Return pricing guidance
#'
#' @param input Input data frame.
#' @return Output list.
#'
#' @import caret
#' @import data.table
#' @import dplyr
#' @import purrr
#' @import readr
#' @import stringr
#' @import tidyr
#' @import xgboost
#' @import zeallot
#' @export


predict <- function(input) {
  t0 <- Sys.time()
  
  input <- load_input(input)
  c(country, customer_segment, currency, cpq) %<-% profile_input(input)
  c(currency, input, lookup_eu_er, lookup_promo, lookup_np_override) %<-% convert_currency_to_usd(input, currency, cpq, country)
  df <- process_input(input, country)
  df <- consolidate_configurations(df)
  df <- process_configurations(df, country)
  c(df_in_scope, df_out_of_scope) %<-% split_by_scope(df)
  df_in_scope <- add_limits(df_in_scope, country)
  guidance <- create_guidance(input, df_in_scope, df_out_of_scope, country)
 

  t1 <- Sys.time()
  duration <- t1 - t0
  message(duration, ' ', units(duration))

  return(guidance)
}
