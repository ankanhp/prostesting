
#' Ping to show server is there
#' @get /ping
function() {
    return('Alive')
}


#' Predict Pricing Guidance
#' @param input json
#' @post /Predict
function(req,res) {
  predict(input = data.frame(req$body))
}

# function(req) {

#     # Read in data
#     input_json <- fromJSON(req$postBody)
#     output <- inference(input_json$features)
#     # Return prediction
#     return(output)

# }


