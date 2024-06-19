library(caret)
library(recipes)

library(forcats)
library(dplyr)

#### ---- Set up ------------------------------------------------- ####

set.seed(911)

problem_type = "classification"
train_test_ratio = 0.75
corr_threshold = 0.8
performance_metric = "Accuracy"
outcome_var = "class"
model_form = as.formula(paste0(outcome_var, "~."))

#### ---- Load data ---------------------------------------------- ####

df = readr::read_csv("data/heart_data.csv")
head(df)


#### ---- Data management ---------------------------------------------- ####

df = (df
	|> mutate(class = as.factor(class)
		, class = fct_recode(class, "Yes" = "1", "No" = "0")
		, class = relevel(class, ref="Yes")
		, sex = fct_recode(as.factor(sex), "Male" = "1", "Female" = "0")
	)
)



#### ---- Data partitioning ------------------------------------------- ####

index = createDataPartition(df[[outcome_var]]
	, p = train_test_ratio
	, list = FALSE
)


##### ---- Training data ------------------------------------------- ####
train_df =  df[index, ]

##### ---- Testing data ------------------------------------------- ####
test_df = df[-index, ]

#### ---- Data preprocessing ------------------------------------------- ####

#### TODO: Handle missing values

preprocessfun = function(df, model_form) {
	x = (recipe(model_form, data=df)
##		|> step_unknown(all_nominal(), new_level = "_Missing_")
##		|> step_impute_mean(all_numeric())
##		|> step_impute_mode(all_nominal())
##		|> step_impute_median(all_numeric())
##		|> step_impute_bag(all_predictors(), all_outcomes())
		|> step_center(all_numeric_predictors())
		|> step_scale(all_numeric_predictors())
		|> step_nzv(all_predictors())
		|> step_corr(all_numeric_predictors(), threshold=corr_threshold)
		|> prep()
		|> bake(new_data=df)
	)
	return(x)
}

train_df = preprocessfun(train_df, model_form)
test_df = preprocessfun(test_df, model_form)

#### ---- Training control params ----------------------------------- ####
training_control = trainControl(method = "cv"
	, repeats = 5
	, number = 10
	, search = "grid"
	, classProbs = ifelse(problem_type=="classification", TRUE, FALSE)
	, summaryFunction = ifelse(problem_type=="classification", twoClassSummary, defaultSummary)
	, seeds = NULL
)

#### ---- Models -------------------------------------------------- ####

##### ---- Logistic model ----------------------------------------- ####

ols_train = train(model_form
	, data = train_df
	, method = ifelse(problem_type=="classification", "glm", "lm")
	, metric = performance_metric
	, family = ifelse(problem_type=="classification", "binomial", "gaussian")
	, trControl = training_control
)
ols_train


##### ---- Random Forest ---------------------------------------- ####

#### TODO: Hyperparameter tuning

rf_train <- train(model_form
	, data = train_df
	, method = "ranger"
	, metric = performance_metric
	, trControl = training_control
	, importance = 'permutation'
	, regularization.usedepth = FALSE
	, save.memory=TRUE
)
print(rf_train)
