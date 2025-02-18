#### ggplot theme

ggtheme <- function(){
   theme_set(theme_bw() +
      theme(panel.spacing=grid::unit(0,"lines")
      	, plot.title = element_text(hjust = 0.5)
			, legend.position = "bottom"
			, axis.ticks.y = element_blank()
			, axis.text.x = element_text(size = 12)
			, axis.text.y = element_text(size = 12)
			, axis.title.x = element_text(size = 12)
			, axis.title.y = element_text(size = 12)
			, legend.title = element_text(size = 13, hjust = 0.5)
			, legend.text = element_text(size = 13)
			, panel.grid.major = element_blank()
			#, panel.grid.minor = element_blank()
			, legend.key.size = unit(0.8, "cm")
			, legend.key = element_rect(fill = "white")
			, panel.spacing.y = unit(0.3, "lines")
			, panel.spacing.x = unit(1, "lines")
			, strip.background = element_blank()
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
}

### Summary data
summ_data = function(df) {
  out = lapply(df, function(x){
    if (is.numeric(x) | is.integer(x)) {
      out = summary(x)
    } else {
      out = table(x, useNA = "always")
    }
    out
  })
  return(out)
}

### Create directories
create_dir = function(name = "dir_name") {
  dir.create(name, showWarnings = FALSE)
}

### Save table as image
save_tab_image = function(gt_tab, path) {
  gt::gtsave(as_gt(gt_tab), file = path)
}

### Collect logs
extract_list_logs = function(log) {
  xx = lapply(names(log), function(l) {
    x = paste0(l, ": ", log[l])
    x = unlist(x)
  })
  xx
}

collect_logs = function(desc="Help me describe the R output: ", log, add_info="Write in paragraph and provide details") {
  out = paste0(desc, " ", paste0(log, collapse = "; "), ". ", add_info)
  out
}


#### ---- Prediction uncertainities ----
bootMeasures <- function(df, model, outcome_var, problem_type){
	x_df <- df[, colnames(df)[!colnames(df) %in% outcome_var]]
	y <- df[, outcome_var, drop=TRUE]
	
	if (problem_type=="classification") {
		preds <- predict(model, x_df, type = "prob")
		preds$pred <- factor(apply(preds, 1, function(x)colnames(preds)[which.max(x)]), levels=levels(y))
		preds$obs <- y
		ss <- twoClassSummary(preds, lev = levels(preds$obs))
		pp <- prSummary(preds, lev = levels(preds$obs))
		aa <- confusionMatrix(preds$pred, preds$obs)$overall[["Accuracy"]]
		scores_df <- data.frame(Accuracy = aa
			, AUCROC = ss[["ROC"]]
			, AUCRecall = pp[["AUC"]]
			, Sens = ss[["Sens"]]
			, Spec = ss[["Spec"]]
			, Precision = pp[["Precision"]]
			, Recall = pp[["Recall"]]
			, "F" = pp[["F"]]
		)

		## ROCs
		base_lev <- levels(preds$pred)[1]
		rocr_pred <- prediction(preds[[base_lev]]
			, preds$obs
		)
		model_roc <- performance(rocr_pred, "tpr", "fpr")
		roc_df <- data.frame(x = model_roc@x.values[[1]], y = model_roc@y.values[[1]])
	} else if (problem_type=="regression") {
		preds <- predict(model, x_df)
		scores_df = data.frame(as.list(postResample(pred = preds, obs = y)))
		roc_df = NULL
		base_lev = NULL
	}
	return(list(scores_df=scores_df, roc_df=roc_df, positive_cat = base_lev))
}

bootEstimates <- function(df, model, outcome_var, nreps = 500, problem_type, report = c("Accuracy", "AUCROC", "AUCRecall", "Sens", "Spec", "Precision", "Recall", "F", "RMSE", "Rsquared", "MAE")) {
	if (problem_type=="classification") {
		all <- c("Accuracy", "AUCROC", "AUCRecall", "Sens", "Spec", "Precision", "Recall", "F") 
	} else if (problem_type=="regression") {
		all <- c("RMSE", "Rsquared", "MAE") 
	}
	if (!any(all %in% report)) {
		stop(c("The report options are ", paste0(all, collapse=", ")))
	}
	resamples <- createResample(1:nrow(df), times = nreps, list = TRUE)
	est <- lapply(resamples, function(x){
		bootMeasures(df[x, ], model, outcome_var, problem_type)$scores_df
	})
	out <- do.call(rbind, est)
	out <- sapply(out, function(x){quantile(x, c(0.025, 0.5, 0.975), na.rm=TRUE)})
	out_metric <- out[, report, drop = FALSE]
	out_metric <- t(out_metric)
	colnames(out_metric) <- c("lower", "estimate", "upper")
	out_metric <- as.data.frame(out_metric)
	out_metric$metric <- rownames(out_metric)
	out <- t(out)
	colnames(out) <- c("lower", "estimate", "upper")
	out <- as.data.frame(out)
	out$metric <- rownames(out)
	out <- list(out_metric, out)
	names(out) <- c("specifics", "all")
	## Generate ROC
	roc <- bootMeasures(df, model, outcome_var, problem_type)
	roc_df <- roc$roc_df
	out$roc_df <- roc_df
	positive_cat <- roc$positive_cat
	out$positive_cat <- positive_cat
	return(out)
}

#### Variable importance

## Functions for permutation-based variable importance
vimp_model <- function(object, ...) {
	new_args <- list(...)
	new_args$object <- object
	out <- do.call("varImp", new_args)$importance
	out <- data.frame(Overall = out)
	return(out)
}


vimp_perm <- function(model, newdata, nrep = 20, estimate = c("mean", "quantile"), parallelize = TRUE, outcome_var, problem_type, nclusters = parallel::detectCores(), ...){

	estimate <- match.arg(estimate)
	# Overall score
	xvars <- colnames(newdata)[!colnames(newdata) %in% outcome_var]
	y <- newdata[, outcome_var, drop=TRUE]

	if (problem_type=="classification") {
		overall_c <- confusionMatrix(predict(model, newdata, type="raw"), y)$overall[["Accuracy"]]
	} else if (problem_type=="regression") {
		overall_c <- postResample(pred=predict(model, newdata), obs=y)[["RMSE"]]
	}
	N <- NROW(newdata)
	newdata <- newdata[, xvars, drop=FALSE]

	if (parallelize) {
		## Setup parallel because serial takes a lot of time. Otherwise you can turn it off
		nn <- min(parallel::detectCores(), nclusters)
		if (nn < 2){
			foreach::registerDoSEQ()
		} else{
			cl <-  parallel::makeCluster(nn)
			doParallel::registerDoParallel(cl)
			on.exit(parallel::stopCluster(cl))
		}

		x <- NULL
		vi <- foreach(x = xvars, .export = "confusionMatrix", .packages=c("ranger", "naivebayes", "caret")) %dopar% {
			set.seed(991)
			permute_df <- newdata[rep(seq(N), nrep), ]
			if (is.factor(permute_df[,x])) {
				permute_var <- as.vector(replicate(nrep, sample(newdata[[x]], N, replace = FALSE)))
				permute_var <- factor(permute_var, levels = levels(permute_df[,x]))
			} else {
				permute_var <- as.vector(replicate(nrep, sample(newdata[[x]], N, replace = FALSE)))
			}
			index <- rep(1:nrep, each = N)
			permute_df[, x] <- permute_var
			if (problem_type=="classification") {
				pred <- predict(model, newdata = permute_df, type = "raw")
				perm_c <- tapply(pred, index, function(r){
					confusionMatrix(r, y)$overall[["Accuracy"]]
				})
			} else if (problem_type=="regression") {
				pred <- predict(model, newdata = permute_df)
				perm_c <- tapply(pred, index, function(r){
					postResample(pred=r, obs=y)[["RMSE"]]
				})
			}
			if (estimate=="mean") {
				est <- mean((overall_c - perm_c)/overall_c)
				names(est) <- x
			} else {
				est <- quantile(abs(overall_c - perm_c)/overall_c, probs=c(0.025, 0.5, 0.975), type=8)
			}
			est
		}
		if (estimate=="quantile") {
			names(vi) <- xvars
			vi <- do.call("rbind", vi)
		}
	} else {
		set.seed(991)
		vi <- sapply(xvars, function(x){
			permute_df <- newdata[rep(seq(N), nrep), ]
			if (is.factor(permute_df[,x])) {
				permute_var <- as.vector(replicate(nrep, sample(newdata[[x]], N, replace = FALSE)))
				permute_var <- factor(permute_var, levels = levels(permute_df[,x]))
			} else {
				permute_var <- as.vector(replicate(nrep, sample(newdata[[x]], N, replace = FALSE)))
			}
			index <- rep(1:nrep, each = N)
			permute_df[, x] <- permute_var
			if (problem_type=="classification") {
				pred <- predict(model, newdata = permute_df, type = "raw")
				perm_c <- tapply(pred, index, function(r){
					confusionMatrix(r, y)$overall[["Accuracy"]]
				})
			} else if (problem_type=="regression") {
				pred <- predict(model, newdata = permute_df)
				perm_c <- tapply(pred, index, function(r){
					postResample(pred=r, obs=y)[["RMSE"]]
				})
			}
			if (estimate=="mean") {
				est <- mean((overall_c - perm_c)/overall_c)
			} else {
				est <- quantile(abs(overall_c - perm_c)/overall_c, probs=c(0.025, 0.5, 0.975), na.rm=TRUE)
			}
			return(est)
		}, simplify=TRUE)
		if (estimate=="quantile") {
			vi <- t(vi)
		}
	}
	if (estimate=="mean") {
		vi <- unlist(vi)
	} else {
		colnames(vi) <- c("lower", "Overall", "upper")
		vi <- data.frame(vi)
	}
	return(vi)
}

get_vimp <- function(model, type = c("model", "perm"), estimate=c("mean", "quantile"), relative=TRUE, newdata, nrep = 20, modelname="model", parallelize = TRUE, outcome_var, problem_type, nclusters = parallel::detectCores(), ...){
	type <- match.arg(type)
	if (type == "perm") {
		out <- vimp_perm(model, newdata, nrep, estimate=estimate, parallelize = parallelize, outcome_var=outcome_var, problem_type=problem_type, nclusters = nclusters, ...)
		if (estimate=="mean") {
			out <- data.frame(Overall = out)
			out$terms <- rownames(out)
			out <- out[, c("terms", "Overall")]
		} else {
			out$terms <- rownames(out)
			out <- out[, c("terms", "lower", "Overall", "upper")]
		}
	} else {
		out <- vimp_model(model, ...)
		out$terms <- rownames(out)
		out <- out[, c("terms", "Overall")]
	}
	if (type=="model" | estimate=="mean") {
		out$sign <- sign(out$Overall)
		out$Overall <- abs(out$Overall)
	}
	out$model <- modelname
	rownames(out) <- NULL
	if (relative){
		if (estimate=="mean") {
			out$Overall <- out$Overall/sum(out$Overall, na.rm = TRUE)
		}
	}
	class(out) <- c("varimp", class(out))
	if (type=="perm") {
		attr(out, "estimate") <- estimate
	} else {
		attr(out, "estimate") <- "mean"
	}
	return(out)
}

plot.varimp <- function(x, ..., pos = 0.5, drop_zero = TRUE, top_n=NULL){
	xsign <- x$sign
	if (!is.null(xsign)) {
		x$sign <- ifelse(xsign==1, "+", ifelse(xsign==-1, "-", "0"))
	} else {
		xsign <- 1
	}
	est <- attr(x, "estimate")
# 	if (est=="quantile") {
# 		x[ "Overall"] <- x$estimate
# 	}
	x <- x[order(x$Overall), ]
	if (drop_zero){
		x <- x[x$Overall!=0, ]
	}
	x <- x[order(x$Overall, decreasing=TRUE), ]
	if (!is.null(top_n)) {
		x <- x[1:top_n, ]
	}
	x <- droplevels(x)

	Overall <- NULL
	lower <- NULL
	upper <- NULL
	nsigns <- unique(xsign)
   nmods <- unique(x$model)
   nsigns <- unique(x$sign)
   pos <- position_dodge(width = pos)
   if (length(nmods)==1) {
      p0 <- ggplot(x, aes(x = reorder(terms, Overall), y = Overall))
#      p0 <- ggplot(x, aes(x = terms, y = Overall))
   } else {
      p0 <- (ggplot(x, aes(x = reorder(terms, Overall), y = Overall, colour = model))
         + labs(colour = "Model")
      )
   }

	if (est=="quantile") {
		if (length(nsigns)>1) {
			p0 <- (p0
				+ geom_point(aes(shape=sign), position = pos)
				+ scale_shape_manual(name = "Sign", values=c(1,16, 15))
				+ geom_linerange(aes(ymin=lower, ymax=upper, lty = sign), position = pos)
				+ labs(linetype = "Sign")
			)
		} else {
			p0 <- (p0
				+ geom_point(position = pos)
				+ geom_linerange(aes(ymin=lower, ymax=upper), position=pos)
			)
		}
	} else {
		if (length(nsigns)>1) {
			p0 <- (p0
				+ geom_point(aes(shape=sign), position = pos)
				+ scale_shape_manual(name = "Sign", values=c(1,16, 15))
				+ geom_linerange(aes(ymin = 0, ymax = Overall, lty = sign), position = pos)
				+ labs(linetype = "Sign")
			)
		} else {
			p0 <- (p0
				+ geom_point(position = pos)
				+ geom_linerange(aes(ymin=0, ymax=Overall), position=pos)
			)
		}
	}
	p1 <- (p0
		+ scale_colour_viridis_d(option = "inferno")
		+ labs(x = "", y = "Importance")
		+ coord_flip(clip = "off", expand = TRUE)
	)
	return(p1)
}

#### Missing values

missing_prop = function(df) {
  df = (df
        |> dplyr::ungroup()
        |> dplyr::summarise_all(~sum(is.na(.)|. %in% c("", " "))/n())
        |> tidyr::pivot_longer(everything())
        |> dplyr::arrange(desc(value))
        |> dplyr::rename("variable"="name", "missing"="value")
        |> filter(missing>0)
        |> dplyr::mutate(missing = scales::percent(missing))
        |> as.data.frame()
  )
  return(df)
}


#### Preprocessing

preprocessFun <- function(df, model_form, corr, handle_missing, exclude=NULL) {
	df_out = recipe(model_form, data=df)
	preprocess_result = list()
	if (anyNA.data.frame(df)) {
		if (handle_missing == "omit") {
			df_out = (df_out
				%>% step_naomit(all_predictors())
			)
			preprocess_result$handle_missing = "Delete missing values"
		} else if (handle_missing== "missing_mean") {
			df_out = (df_out
				%>% step_unknown(all_nominal(), new_level = "_Missing_")
				%>% step_impute_mean(all_numeric())
			)
			preprocess_result$handle_missing = c("missing values in categorical variables recorded as _Missing_", "missing values in numeric variables replaced by the mean.")
		} else if (handle_missing=="missing_median") {
			df_out = (df_out
				%>% step_unknown(all_nominal(), new_level = "_Missing_")
				%>% step_impute_median(all_numeric())
			)
			preprocess_result$handle_missing = c("missing values in categorical variables recorded as _Missing_", "missing values in numeric variables replaced by the median.")
		} else if (handle_missing=="mode_median") {
			df_out = (df_out
				%>% step_impute_mode(all_nominal())
				%>% step_impute_median(all_numeric())
			)
			preprocess_result$handle_missing = c("missing values in categorical variables replacesd by modal value", "missing values in numeric variables replaced by the median.")
		} else if (handle_missing=="mode_mean") {
			df_out = (df_out
				%>% step_impute_mode(all_nominal())
				%>% step_impute_mean(all_numeric())
			)
			preprocess_result$handle_missing = c("Categorical -> Mode", "Numeric -> Mean")
			preprocess_result$handle_missing = c("missing values in categorical variables replacesd by modal value", "missing values in numeric variables replaced by the mean.")
		} else if (handle_missing=="bag") {
			df_out = (df_out
				%>% step_impute_bag(all_predictors(), all_outcomes())
			)
			preprocess_result$handle_missing = c("Bagging")
		} else if (handle_missing=="knn") {
			df_out = (df_out
				%>% step_impute_knn(all_predictors(), all_outcomes())
			)
			preprocess_result$handle_missing = c("missing values imputed using KNN.")
		} else if (handle_missing=="knn_linear") {
			df_out = (df_out
				%>% step_impute_knn(all_nominal())
				%>% step_impute_linear(all_numeric())
			)
			preprocess_result$handle_missing = c("missing values in categorical variables replacesd by KNN", "missing values in numeric variables replaced by linear reg/ression.")
		}
	}
	df_out = (df_out
		%>% step_center(all_numeric_predictors())
		%>% step_scale(all_numeric_predictors())
	)
	if (is.null(exclude)) {
		df_out = (df_out
			%>% step_nzv(all_predictors())
		)
	} else {
		df_out = (df_out
			%>% step_nzv(all_predictors(), -all_of(exclude))
		)
	}
	preprocess_result$handle_predictors = c("All numeric variables were centered and scaled and variables with near zero variance removed.")
	if (corr > 0) {
		if (is.null(exclude)) {
			df_out = (df_out
				%>% step_corr(all_numeric_predictors(), threshold=corr)
			)
		} else {
			df_out = (df_out
				%>% step_corr(all_numeric_predictors(), -all_of(exclude), threshold=corr)
			)
		}
		preprocess_result$correlated_predictors = paste0("One of the variables which had a bivariate correlation of at least ", corr, " were droped.")
	}
	df_out = (df_out
		%>% prep()
		%>% bake(new_data=df)
	)
	removed_vars = colnames(df)[!colnames(df) %in% colnames(df_out)]
	preprocess_result$n_removed_vars = if(length(removed_vars)) paste0(length(removed_vars), " variabels were removed after preprocessing.") else  NULL 
	preprocess_result$removed_vars = if(length(removed_vars)) paste0(removed_vars, collapse=",") else NULL
	preprocess_result$predictors_for_analysis = paste0(outcome_var, " was used as an outcome variable, while the following were used as predictors: ", paste0(colnames(df_out)[!colnames(df_out) %in% outcome_var], collapse=", "), ".")
	return(list(df_processed=df_out, df_original=df, preprocess_steps=preprocess_result))
}

# preprocessfun = function(df, model_form, corr_threshold) {
# 	x = (recipe(model_form, data=df)
# 	  |> step_unknown(all_nominal(), new_level = "_Missing_")
#     # |> step_impute_mean(all_numeric_predictors())
#     # |> step_impute_mode(all_nominal_predictors())
#     # |> step_impute_median(all_numeric_predictors())
#     # |> step_impute_bag(all_predictors(), all_outcomes())
# 		|> step_center(all_numeric_predictors())
# 		|> step_scale(all_numeric_predictors())
# 		|> step_nzv(all_predictors())
# 		|> step_corr(all_numeric_predictors(), threshold=corr_threshold)
# 		|> prep()
# 	  |> bake(new_data=df)
# 	)
# 	return(x)
# }
