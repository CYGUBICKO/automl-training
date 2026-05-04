
#### ---- DS Training -----

##### ---- Date: 04/05/2026 ------

#### ---- Set up ------

# install.packages("readr")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("gtsummary")

library(readr)
library(ggplot2)
library(dplyr)
library(gtsummary)

yes_no_fun = function(x) {
  if (x==0) {
    return("No")
  } else if (x==1) {
    return("Yes")
  } else {
    return("Missing")
  }
}

missing_pattern = function(x, pattern = c("", " ", "Missing", "Unknown")) {
  sum(is.na(x) | x %in% pattern)/n()
}

miissing_prop = function(df) {
  df = (df
    |> ungroup()
    |> summarise_all(~missing_pattern(x=.))
    |> tidyr::pivot_longer(everything())
    |> arrange(desc(value))
    |> rename("variable"="name", "missing"="value")
    |> mutate(missing = scales::percent(missing))
    
  )
  return(df)
}

#### ---- Upload data -----

df = read_csv("https://raw.githubusercontent.com/i-dair-tech/i-dair-codex-data/refs/heads/main/simulate_data/heart_data.csv")

#### ---- Data management ----
df2 = (df
  |> mutate(
    class = if_else(class==1, "Yes", if_else(class==0, "No", "Missing"))
    , sex = if_else(sex==0, "Female", "Male")
    , sex = relevel(as.factor(sex), ref = "Female")
    , class = relevel(as.factor(class), ref = "No")
  )
)

#### --- Explore data -----

summ = (df2
  |> tbl_summary(by = class, percent = "row")
)
summ

#### ---- Check for missing values -----
miss_df = (df2
  |> miissing_prop()
)


#### ---- Visualization ------
age_hist = (ggplot(df2, aes(x = age))
  + geom_histogram(fill="green")
  + theme_bw()
)
print(age_hist)

age_class = (ggplot(df2, aes(x=class, y=age))
  + geom_boxplot(fill="tomato")
  + theme_minimal()
)
print(age_class)

### Correlation

numeric_vars = (df2
  |> select(where(is.numeric))
)

corr_mat = (numeric_vars
  |> cor()
)

clean_corr = (corr_mat
  |> as.data.frame()
  |> tibble::rownames_to_column(var="Var1")
  |> tidyr::pivot_longer(-Var1, names_to = "Var2", values_to = "correlation")
)

corr_plot = (ggplot(clean_corr, aes(x=Var1, y=Var2, fill=correlation))
  + geom_tile()
  + scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)
  + theme_minimal()
  + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  + labs(title = "Correlation", x = "", y="")
)
print(corr_plot)
