
##### ---- Model configuration ------------------------------- ####

## Data partition
train_test_ratio = 0.75

## Feature engineering
corr_threshold = 0.8
handle_missing = "omit"

## Cross-validation
cv_method = "cv"
cv_repeats = 5
n_folds = 10

## Performance measures
performance_metric = "Accuracy"
report_metric = "AUCROC"

## Bootstrapping
nreps = 200
top_n_best = 2
top_n_varimp = 3
top_n_rank = 5
total_n_rank = 20

## Tokens in Gemini
max_tockens = 10000
