## AUTOML Training Repo

The pipeline integrates gemini API via [gemini.R package](https://cran.r-project.org/web/packages/gemini.R/index.html) into the auto-ml pipeline. The integration allows for automated interpretation of key outputs.

## Illustrations

- DSWB AGM 2025 (2025 Feb 22 - 23)
- Data Community meeting at APHRC on 2024 Jun 20 (Thur)


### Requirements

- Get [Gemini API](https://makersuite.google.com/app/apikey)
- Create a file in the path `~/.Renviron` and paste the API key, i.e.,

```
GEMINE_API_KEY=YOUR_API_KEY
```
- Save and close

###  Usage

- Open `simple_ml_pipeline.R` from your preferred editor (Rstudio recommended) and run line by line for default usage. Once you familiarize with the codebase, you can change the data via `data_path`, etc.

#### Key components

- You can modify the following variables depending on the task:
	- `experiment_name` - This gives a tag to the current experiment. The output folder which contains all the files related to the current experiment will be tagged this, i.e., `outputs_your_experiment_name`.
	- `study_description` - The description of the study or current dataset. You can also include study objective.
	- `data_path` - URL or path to the dataset. Supports .csv, .xlsx, .xls, .dta, .rda, .rds, .spss files.
	- `problem_type` - Whether "classification" or "regression" problem
	- `outcome_var` - The outcome of interest. The variable should be in the dataset
	- `train_test_ratio` - Training-test data ratio for partitioning
	- `corr_threshold` - Correlation threshold, ranges `[0, 1]`, for bivariate comparison of variables. One of the highly correlated variables is dropped based on the threshold
	- `handle_missing` - How to hand missing values. It can either be "omit", "missing_mean", "missing_median", "mode_median", "mode _mean", "bag", "knn", or "knn_linear"
	- `cv_method` - Cross-validation method. It can be "CV ", "repeatedcv ", "adaptive_cv", "LOOCV ", "LGOCV ", "adaptive_LGOCV ", "boot", "boot632", "optimism_boot", "boot_all", or "adaptive_boot"
	- `n_folds` - Number of cross-validation folds. Either 3, 5, or 10.
	- `cv_repeats` - Number of repeats in cross-validation
	- `performance_metric` - Measure of model performance
		- For regression: "RMSE", "Rsquared", "MAE"
		- For classification: "AUCROC", "Accuracy", "AUCRecall", "Sens", "Spec", "Precision", "Recall", "F"
	- `report_metric` - Depending on the task, it can be any of the above

	

### Advance pipeline

- See [AUTOML Pipeline](https://github.com/CYGUBICKO/automl-pipeline)

