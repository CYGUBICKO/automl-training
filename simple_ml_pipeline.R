library(caret) ## ML piple
library(recipes) 

library(forcats)
library(dplyr)

library(GGally)
library(ggplot2)

library(MLmetrics)
library(ROCR)

library(gemini.R)

#### ---- Set up ------------------------------------------------- ####

set.seed(911)

study_description = "Heart disease. Collected in the US. In 1991."
problem_type = "classification"

data_path = "https://raw.githubusercontent.com/i-dair-tech/i-dair-codex-data/refs/heads/main/simulate_data/heart_data.csv"

train_test_ratio = 0.75
corr_threshold = 0.8

performance_metric = "Accuracy"

report_metric = "AUCROC"

outcome_var = "class"

model_form = as.formula(paste0(outcome_var, "~."))

nreps = 200
top_n_best = 2
top_n_varimp = 3
top_n_rank = 5
total_n_rank = 20

request_text = "Help me write a description in paragraph form. Provide details."

source("funs.R")
ggtheme()

#### ---- Set API KEY -------------------------------------------- #####
## API key
## file.show("~/.Renviron")
base::readRenviron("~/.Renviron")
api_key = Sys.getenv("GEMINE_API_KEY")
setAPI(api_key)

#### Project title
project_title_logs_GAI = gemini_chat(paste0("Help improve the below, as a title for a journal submission: ", study_description))
project_title_logs_GAI_out = gemini_chat("Pick the best. Only give the answer and format as .md title, centered.", history = project_title_logs_GAI$history)

#### ---- Load data ---------------------------------------------- ####

df = readr::read_csv(data_path)
head(df)

##### ----- Data dimension --------------------------------------- ####
df_dim = dim(df)
df_dim = list("Observations"=df_dim[[1]], "Features"=df_dim[[2]])
context_logs = collect_logs(desc = project_title_logs_GAI_out$outputs
  , extract_list_logs(df_dim)
  , add_info = ""
)

#### ---- Data management ---------------------------------------------- ####

#### ---- Explore data ------------------------------------------------- ####
skim = skimr::skim(df)
summ = summ_data(df)

desc_stats_logs = collect_logs(desc = paste0(context_logs, "With the following descriptives generated from R: ")
  , log = extract_list_logs(summ)
  , add_info = request_text
)
introduction_logs = gemini_chat(
  paste0('Help me write an introduction for the journal manuscript, based on the following information. Make it very detailed and include literature. Only give the answer'
    , project_title_logs_GAI_out$outputs
  )
)

introduction_logs_improved = gemini_chat("The main aim of the study is to predict heart condition based on the clinical and demographic variables. Incorporate this in the the output.", history = introduction_logs$history)

# c1 = gemini_chat(desc_stats_logs_GAI)
# c1 = gemini_chat("Help me generate table", c1$history)
# print(c1)

#### ---- Data transformation ----------------------------------------- #####
df = (df
	|> mutate(class = as.factor(class)
		, class = fct_recode(class, "Yes" = "1", "No" = "0")
		, class = relevel(class, ref="Yes")
		, sex = fct_recode(as.factor(sex), "Male" = "1", "Female" = "0")
	)
)

##### ---- Data visualization ------------------------------------------ ####
all_vars = colnames(df)
var_labels = all_vars
## ggbivariate(df, outcome = outcome_var)
compare_plot = sapply(unique(all_vars), function(x) {
  index = all_vars %in% x
  .labs = var_labels[index]
  if (x != outcome_var) {
    p = (ggbivariate(df %>% select_at(all_of(c(x, outcome_var)))
        , outcome=outcome_var
        , columnLabelsY = ""
      )
      + theme(legend.position="bottom")
      + labs(title=.labs)
    )
    print(p)
  }
}, simplify = FALSE)


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
#	, repeats = 5
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
model_name_ = "Logistic regression"
ols_train$model_name_ = model_name_


##### ---- Random Forest ---------------------------------------- ####

#### TODO: Hyperparameter tuning

rf_train = train(model_form
	, data = train_df
	, method = "ranger"
	, metric = performance_metric
	, trControl = training_control
	, importance = 'permutation'
	, regularization.usedepth = FALSE
	, save.memory=TRUE
)
print(rf_train)
model_name_ = "Random forest"
rf_train$model_name_ = model_name_


##### ---- GBM ---------------------------------------- ####
gbm_train = train(model_form
	, data = train_df
	, method = "gbm"
	, distribution = ifelse(problem_type=="classification", "bernoulli", "gaussian")
	, metric = performance_metric
	, trControl = training_control
)
gbm_train
model_name_ = "Gradient boosting"
gbm_train$model_name_ = model_name_


#### ---- Predictive performance ------------------------------------------####

##### ------ Get metrics for all models ----------------------------------#####
scores = sapply(ls(pattern = "_train$"), function(x){
	x = get(x)
	modname = x$model_name_
	out = bootEstimates(df=test_df, outcome_var=outcome_var, problem_type=problem_type, model=x, nreps=nreps, report=report_metric)
	out$specifics$model = modname
	out$all$model = modname
	out$roc_df$model = modname
	return(out)
}, simplify=FALSE)
scores = do.call("rbind", scores)
metric_df = do.call("rbind", scores[, "specifics"])
all_metrics_df = do.call("rbind", scores[, "all"])

if (problem_type=="classification") {
	roc_df = do.call("rbind", scores[, "roc_df"])
	positive_class = do.call("rbind", scores[, "positive_cat"])
} else if (problem_type=="regression") {
	roc_df = NULL 
	positive_class = NULL 
}
metric_df[, c("lower", "estimate", "upper")] = sapply(metric_df[, c("lower", "estimate", "upper")], function(x){round(x, 3)})

# metric_df$model_score = paste0(metric_df$model, " with ", metric_df$metric, " score of ", metric_df$estimate, "[", metric_df$lower, ", ", metric_df$upper, "]")

##### -------------------------- Plot metrics ----------------------------- ####
pos = position_dodge(0.05)
metric_plot = (ggplot(metric_df, aes(x=reorder(model, -estimate), y=estimate))
	+ geom_point(position = pos)
	+ geom_errorbar(aes(ymin = lower, ymax = upper)
		, position = pos
		, width = .2
	)
	+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
	+ labs(y = report_metric
		, x = "Model"
	)
)
print(metric_plot)

##### ----- ROC Curve -------------------------------------------------- ####
if (problem_type=="classification") {
	roc_df = (roc_df
		%>% left_join(
			metric_df %>% select(estimate, model)
			, by = c("model")
		)
		%>% mutate(model_new = paste0(model, " (", round(estimate,3), ")"))
	)

	ll = unique(roc_df$model)
	roc_plot = (ggplot(roc_df, aes(x = x, y = y, group = model, colour = model))
		+ geom_line()
		+ scale_x_continuous(limits = c(0, 1))
		+ scale_y_continuous(limits = c(0, 1))
	# 	+ scale_color_manual(breaks = ll, values = rainbow(n = length(ll))
	# 		, guide = guide_legend(override.aes = list(size=1), reverse=TRUE)
	# 	)
	# 	+ scale_linetype_discrete(guide = guide_legend(override.aes = list(size=1), reverse=TRUE))
	  + geom_abline(intercept = 0, slope = 1, colour = "darkgrey", linetype = 2)
		+ labs(x = "False positive rate"
			, y = "True positive rate"
			, colour = "Model"
		)
		+ theme(legend.position="right")
	)
	print(roc_plot)
}


#### ---- Best model ----------------------------------------------------#####
best_df = (metric_df
	%>% arrange(desc(estimate))
	%>% mutate(.n = 1:n())
	%>% filter(.n <= top_n_best)
	%>% select(-.n)
)
print(best_df)


#### ---- Variable importance --------------------------------------------####

nreps = min(c(nreps/2, 100))
varimp_df = sapply(ls(pattern = "_train$"), function(x){
	x = get(x)
	modname = x$model_name_
	out = get_vimp(x
		, type="perm"
		, newdata=train_df
		, outcome_var = outcome_var
		, problem_type=problem_type
		, estimate = "quantile"
		, nrep=nreps
		, modelname=modname
		, parallelize=FALSE
	)
	return(out)
}, simplify=FALSE)

varimp_all_df = do.call("rbind", varimp_df)

best_model = best_df$model
varimp_topn_df = (varimp_all_df
	%>% filter(model %in% best_model)
)

p1 = plot(varimp_topn_df)
print(p1)

best_vars = (varimp_topn_df
	%>% group_by(model)
	%>% arrange(desc(Overall), .by_group=TRUE)
	%>% mutate(.n = 1:n())
	%>% filter(.n <= min(top_n_varimp, length(unique(varimp_topn_df$terms))))
	%>% pull(terms)
	%>% unique()
)
top_n = length(best_vars)


## Mostly frequently identified variables
varfreq_df = (varimp_all_df
	%>% group_by(model)
	%>% arrange(desc(Overall), .by_group=TRUE)
	%>% mutate(pos = 1:n())
	%>% ungroup()
	%>% mutate(NULL
		, pos=ifelse(pos<=top_n_rank, pos, top_n_rank+1)
		, new_terms=fct_reorder(terms, pos, mean)
	)
	%>% filter(as.numeric(new_terms) <= total_n_rank)
	%>% group_by(new_terms, pos)
	%>% count()
	%>% droplevels()
)
print(varfreq_df)

varfreq_plot = (ggplot(varfreq_df, aes(x=pos, y=fct_reorder(new_terms, -pos, .fun=mean), fill=n))
	+ geom_tile(color="black")
	+ scale_fill_distiller(palette = "Greens", direction=1)
	+ scale_y_discrete(expand=c(0,0))
	+ scale_x_continuous(
		breaks=function(x){1:max(x)}
		, labels=function(x){
			m = max(x)
			v = as.character(1:m)
			v[[m]] = paste0(">", m-1)
			return(v)
		}
		, expand=c(0,0)
	)
	+ labs(y="", x="Rank", fill="Frequency")
	+ theme_bw(base_size=12)
	+ theme(
		strip.background = element_blank()
		, panel.border = element_rect(colour = "grey"
			, fill = NA
			, size = 0.8
		)
		, strip.text.x = element_text(size = 11
			, colour = "black"
			, face = "bold"
		)
	)
)
print(varfreq_plot)



#### ---- Prediction -------------------------------------------------- #####
if (!file.exists("prediction_template.xls")) {
  prediction_template = (train_df
    |> sample_n(2)
    |> select(-all_of(outcome_var))
  )
  readr::write_csv(prediction_template, file = "prediction_template.csv")
}
pred_df = readr::read_csv("prediction_template.csv")

preds = sapply(ls(pattern = "_train$"), function(x){
	modname = gsub("\\_train", "", x)
	x = get(x)
	out = predict(x, newdata=pred_df)
	pred_df[[outcome_var]] = out
	pred_df$model = modname
	return(pred_df)
}, simplify=FALSE)
preds = do.call("rbind", preds)


#### ---- Generate Report ------------------------------------------------ ####

rmarkdown::render(
  input = "draft_report.Rmd",
  output_file = "draft_report.docx",
  envir = parent.frame()
)