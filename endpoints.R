
#' Ping to show server is there
#' @get /ping
function() {
    return('Alive')
}


#' Parse input and return prediction from model
#' @param req The http request sent
#' @post /invocations
# set up the environment
load("/opt/ml/02_code/sysdata.rda")
source("/opt/ml/02_code/scripts.R")
source("/opt/ml/02_code/main.R")
function(request) {

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
    guidance <- data.frame(matrix(unlist(guidance), ncol=13, byrow=TRUE),stringsAsFactors=FALSE) %>% `colnames<-`(.[1, ]) %>% .[-1, ]


    t1 <- Sys.time()
    duration <- t1 - t0
    message(duration, ' ', units(duration))

    return(guidance)

}


