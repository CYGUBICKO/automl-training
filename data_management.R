#### ---- Data transformation ----------------------------------------- #####

data_management = TRUE
data_transform_logs = list()
data_transform_logs$outputs = ""
data_transform_logs$history = list()

##### Include all the DM scripts here.
##### To run default, without DM, set data_management = FALSE

if (!data_management) {
  if (problem_type=="classification") {
    l = unique(df[[outcome_var]])
    if (any(class(l) %in% c("numeric", "integer", "double"))) {
      f = paste0("level_", l)
      b = f[1]
      df = (df
        |> mutate_at(outcome_var, function(x){
          x = factor(x, levels=l, labels = f)
          x =  relevel(x, ref=b)
        })
      )
      request_text = paste0("The outcome variable", outcome_var, " was converted factor ")
      data_transform_logs = gemini_chat(
        prompt = paste0("The following data management steps were performed. Write a detailed methods section for the manuscript. ", paste0(request_text, collapse = ""))
        , history = introduction_logs$history
      )
    }
  }
} else {
  df = (df
    |> mutate_at(outcome_var, function(x){
      x = as.factor(x)
      x =  fct_recode(x, "Yes" = "1", "No" = "0")
      x =  relevel(x, ref="Yes")
    })
    |> mutate_at("sex", function(x){
      x = as.factor(x)
      x =  fct_recode(x, "Male" = "1", "Female" = "0")
    })
  )
  
  #### Add any data management steps here as a vector
  request_text = c("Changed class to factor and assigned 1 to yes and 0 to no"
    , "In sex variable, assigned 1 to male and 0 to female"
  )
  data_transform_logs = gemini_chat(
    prompt = paste0("The following data management steps were performed. Write a detailed methods section for the manuscript. ", paste0(request_text, collapse = ""))
    , history = introduction_logs$history
  )
}
