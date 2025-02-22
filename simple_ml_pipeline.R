library(caret) 
library(recipes) 

library(forcats)
library(dplyr)

library(gtsummary)

library(GGally)
library(ggplot2)

library(MLmetrics)
library(ROCR)

library(gemini.R)

#### ---- Set up ------------------------------------------------- ####

set.seed(911)
current_path = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_path)

#### ---- Load data ---------------------------------------------- ####
source("upload_data.R")

model_form = as.formula(paste0(outcome_var, "~."))

## Read data
class(data_path) = Rautoml::get_file_ext(data_path)
df = Rautoml::upload_data(data_path)
head(df)

#### ---- Load data ---------------------------------------------- ####

source("model_config.R")

## Useful functions
source("funs.R")
ggtheme()

#### ---- Set API KEY -------------------------------------------- #####
## API key
## file.show("~/.Renviron")
base::readRenviron("~/.Renviron")
api_key = Sys.getenv("GEMINE_API_KEY")
setAPI(api_key)

## Create outputs folder
output_path = paste0(current_path, "/", paste0("outputs_", experiment_name))
create_dir(output_path)

#### Project title
project_title_logs_GAI = gemini_chat(paste0("Help improve the below, as a title for a journal submission: ", study_description))
project_title_logs_GAI_out = gemini_chat("Pick the best. Only give the answer and format as markdown title. Do not format the code", history = project_title_logs_GAI$history)


##### ----- Data dimension --------------------------------------- ####
df_dim = dim(df)
df_dim = list("Observations"=df_dim[[1]], "Features"=df_dim[[2]])
context_logs = collect_logs(desc = paste0(project_title_logs_GAI_out$outputs, "The dataset has ")
  , extract_list_logs(df_dim)
  , add_info = ""
)

##### ----- Variables in the data --------------------------------------- ####
predictor_vars = colnames(df)[!colnames(df) %in% outcome_var]
vars_df = list("Outcome" = outcome_var, "Predictors" = paste0(predictor_vars, collapse = ","))
context_logs = collect_logs(desc = paste0(context_logs, "With the following ")
  , extract_list_logs(vars_df)
  , add_info = paste0("This is a ", problem_type, " problem to predict ", outcome_var)
)

introduction_logs = gemini_chat(paste0("Based on the information below, write the introduction for the manuscript. Make it very detailed, including background, justification, objectives, methodology, etc ", context_logs), maxOutputTokens = max_tockens)

#### ---- Data management ---------------------------------------------- ####

##### ---- Explore data ------------------------------------------------- ####

# skim = (df
#   |> skimr::skim()
# )
# summ = summ_data(df)
summ = (df
  |> tbl_summary(by=all_of(outcome_var))
)
tab1_name = paste0(output_path, "/table1.png")
save_tab_image(summ, tab1_name)

request_text = "Use the image from gtsummary package attached. Create a caption and .md table 1 for manuscript submission. Provide only the output"
table1_logs = gemini_image(image = tab1_name, prompt = request_text, maxOutputTokens = 10000)
table1_interpete_logs = gemini_chat(prompt = paste0("Provide detailed interpretation of Table 1 for manuscript submission. Make it detailed only give the answer : ", table1_logs))


#### ---- Data transformation ----------------------------------------- #####
source("data_management.R")

##### ---- Missing values ----------------------------------------- #####
miss_df = (df
  |> missing_prop()
)

if (NROW(miss_df)) {
  miss_prop = paste0(miss_df$variable, " is ", miss_df$missing, collapse = ", ")
} else {
  miss_prop = "There were no missing values"
}
data_transform_logs = gemini_chat(prompt = paste0("Update the methods based on the following, make it detailed and provide final answer", miss_prop)
  , history = data_transform_logs$history
  , maxOutputTokens = max_tockens
)

##### ---- Data visualization ------------------------------------------ ####

##### -- Auto plots -----
descriptive_auto_plots = ggbivariate(df, outcome = outcome_var)
image_path = paste0(output_path, "/descriptive_auto_plots.png")
ggsave(image_path
  , plot = descriptive_auto_plots
  , width = 20
  , height = 50
  , units = "cm"
)
context_logs = "Provide a detailed interpretaion of the image. Call it Figure 1."
descriptive_auto_plots_logs = gemini_image(image = image_path, prompt = context_logs, maxOutputTokens = max_tockens)
descriptive_auto_include_plots_logs = gemini_chat(paste0("Insert the Figure 1 into .md file. Include caption. Resize figure height by 50% to fit the page. ", image_path, "Avoid code formating. Provide answer only."))

##### -- Individual plots -----
all_vars = colnames(df)
var_labels = all_vars
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
    fname = paste0(output_path, "/", x, "_", outcome_var, ".png")
    ggsave(filename = fname, plot = p)
    return(p)
  }
}, simplify = FALSE)


#### ---- Data partitioning ------------------------------------------- ####

index = createDataPartition(df[[outcome_var]]
	, p = train_test_ratio
	, list = FALSE
)

context_logs = paste0("The data was partitioned into training and test based on the "
  , train_test_ratio
  , " proportion. "
  , "The analysis is conducted in "
  , R.version$version.string
)
data_transform_logs = gemini_chat(prompt = paste0("Update the methods based on the following, make it detailed and provide final answer", context_logs)
  , history = data_transform_logs$history
  , maxOutputTokens = max_tockens
)

##### ---- Training data ------------------------------------------- ####
train_df =  df[index, ]

##### ---- Testing data ------------------------------------------- ####
test_df = df[-index, ]

#### ---- Data preprocessing ------------------------------------------- ####

train_objs = preprocessFun(df = train_df
  , model_form
  , corr = corr_threshold
  , handle_missing = handle_missing
)
train_df = train_objs$df_processed
preprocess_result = paste0(train_objs$preprocess_steps, collapse = "; ")

if (!is.null(train_objs$preprocess_steps$removed_vars)) {
  excluded_vars = train_objs$preprocess_steps$removed_vars
  excluded_vars = strsplit(excluded_vars, ",")[[1]]
} else {
  excluded_vars = NULL
}

test_objs = preprocessFun(df = test_df
  , model_form
  , corr = corr_threshold
  , handle_missing = handle_missing
  , exclude = excluded_vars
)
test_df = test_objs$df_processed

data_transform_logs = gemini_chat(prompt = paste0("Update the methods based on the following, make it detailed and provide final answer", preprocess_result)
  , history = data_transform_logs$history
  , maxOutputTokens = max_tockens
)

#### ---- Hyperparameters ----------------------------------- ####
source("models_hyperparameters.R")

#### ---- Training control params ----------------------------------- ####
training_control = trainControl(method = cv_method
	, repeats = cv_repeats
	, number = n_folds
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
	, tuneGrid = rf_tunegrid
	, regularization.factor = regularization.factor
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

#### Add models here

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
metric_df$model_score = paste0(metric_df$model, " with ", metric_df$metric, " score of ", metric_df$estimate, "[", metric_df$lower, ", ", metric_df$upper, "]")

context_logs = paste0("The following models were trained: ", paste0(unique(metric_df$model), collapse = ", "), ". We applied ", n_folds, " cross-validation and used ", performance_metric, " to pick the best performing model.")
context_logs = paste0(context_logs, " To evaluate the sensitivity and uncertainty of the predictive performance measures, we applied bootstrap resampling to estimate the 2.5%, 50% and 97.5% quantiles of the distribution of the scores. We used ",  nreps, " bootstrap resamples of the test data", ".")
data_transform_logs = gemini_chat(prompt = paste0("Update the methods based on the following, make it detailed and provide final answer", context_logs)
  , history = data_transform_logs$history
  , maxOutputTokens = max_tockens
)

context_logs = paste0("We applied and compared the following models: ", paste0(unique(metric_df$model_score), collapse=", "), ".")
trained_models_logs = gemini_chat(prompt = paste0("Write Model comparison section. Make it very detailed. ", context_logs)
  , history = table1_interpete_logs$history
  , maxOutputTokens = max_tockens
)

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

image_path = paste0(output_path, "/metric_plot_", performance_metric, ".png")
ggsave(image_path
  , plot = metric_plot
  , height = 12
  , width = 15
  , units = "cm"
)
context_logs = "Provide a detailed interpretaion of the image. Call it Figure 2."
trained_models_metric_logs = gemini_image(image = image_path, prompt = context_logs, maxOutputTokens = max_tockens)
trained_models_metric_include_logs = gemini_chat(paste0("Insert the Figure 2 into .md file. Include caption. Resize figure height by 50% to fit the page. ", image_path, "Avoid code formating. Provide answer only."))


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


  image_path = paste0(output_path, "/metric_plot_", "roc", ".png")
  ggsave(image_path
    , plot = roc_plot
    , height = 12
    , width = 15
    , units = "cm"
  )
  context_logs = "Provide a detailed interpretaion of the image. Call it Figure 3."
  trained_models_roc_logs = gemini_image(image = image_path, prompt = context_logs, maxOutputTokens = max_tockens)
  trained_models_roc_include_logs = gemini_chat(paste0("Insert the Figure 3 into .md file. Include caption. Resize figure height by 50% to fit the page. ", image_path, "Avoid code formating. Provide answer only."))
} else {
  trained_models_roc_logs = ""
  trained_models_roc_include_logs = list()
  trained_models_roc_include_logs$outputs = ""
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

varimp_all_plot = plot(varimp_topn_df)
print(varimp_all_plot)

image_path = paste0(output_path, "/varimp_plot_best_model.png")
ggsave(image_path
  , plot = varimp_all_plot
  , height = 12
  , width = 15
  , units = "cm"
)

context_logs = paste0("Interpret the permutation-based variable importance plot attached. Call it Figure 4. The performance measure metric is ", performance_metric)
varimp_best_model_logs = gemini_image(image = image_path, prompt = context_logs, maxOutputTokens = max_tockens)
varimp_best_model_include_logs = gemini_chat(paste0("Insert the Figure 4 into .md file. Include caption.", image_path, "Avoid code formating. Provide answer only."))

best_vars = (varimp_topn_df
	%>% dplyr::group_by(model)
	%>% dplyr::arrange(desc(Overall), .by_group=TRUE)
	%>% dplyr::mutate(.n = 1:n())
	%>% dplyr::filter(.n <= min(top_n_varimp, length(unique(varimp_topn_df$terms))))
	%>% dplyr::pull(terms)
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

image_path = paste0(output_path, "/varfreq_plot.png")
ggsave(image_path
  , plot = varfreq_plot
  , height = 15
  , width = 15
  , units = "cm"
)

context_logs = paste0("Interpret the variable importance ranking plot based on the fitted models: ", paste0(unique(metric_df$model), collapse = ", "))
varfreq_plot_logs = gemini_image(image = image_path, prompt = context_logs, maxOutputTokens = max_tockens)
varfreq_plot_include_logs = gemini_chat(paste0("Insert the Figure 5 into .md file. Include caption.", image_path, "Avoid code formating. Provide answer only."))

#### ---- Discussion -------------------------------------------------- #####

discussion_logs = gemini_chat(prompt = "Write a detailed discussion for manuscript"
  , history = data_transform_logs$history
)
discussion_logs = gemini_chat(prompt = paste0("Based on the attached result. Update the discussion.", varimp_best_model_logs[[1]])
  , history = discussion_logs$history
  , maxOutputTokens = max_tockens
)

#### ---- Conclussion -------------------------------------------------- #####
conclussion_logs = gemini_chat(prompt = "Write conclussion for the manuscript"
  , history = discussion_logs$history
  , maxOutputTokens = max_tockens
)


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

#### Gather all
# combined_output = paste0(project_title_logs_GAI_out$outputs
#   , introduction_logs$outputs
#   , data_transform_logs$outputs
#   , table1_interpete_logs$outputs
#   , table1_logs, descriptive_auto_plots_logs
#   , descriptive_auto_include_plots_logs$outputs
#   , trained_models_logs$outputs
#   , collapse = " "
# )

#### ---- Generate Report ------------------------------------------------ ####
rmarkdown::render(
  input = "draft_report.Rmd",
  output_file = paste0(output_path, '/draft_report.docx'),
  envir = parent.frame()
)


