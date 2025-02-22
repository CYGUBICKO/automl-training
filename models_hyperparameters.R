library(readxl)
source("funs.R")

file_name <- "models_hyperparameters.xlsx"
sheets <- excel_sheets(file_name)
models_hyperparameters <- sapply(sheets, function(x){
	readxl::read_excel(file_name, sheet = x)
}, simplify = FALSE)

models_hyperparameters <- get_excel_param_all(models_hyperparameters)
default_params <- as.logical(models_hyperparameters$default)

#### ---- Random Forest -------------
rf_param <- models_hyperparameters$rf
num.trees <- rf_param$num.trees
regularization.factor <- rf_param$regularization.factor

if (default_params) {
   rf_tunegrid <- NULL
} else {
   mtry <- rf_param$mtry
   min.node.size <- rf_param$min.node.size
   splitrule <- rf_param$splitrule
   rf_tunegrid <- expand.grid(
      mtry = floor(seq(mtry[1], mtry[2], length.out=mtry[3]))
      , splitrule = splitrule
      , min.node.size = min.node.size
   )
}
