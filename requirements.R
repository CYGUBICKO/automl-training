libs = c(
	"caret"
	, "recipes"
	, "forcats"
	, "dplyr"
	, "gtsummary"
	, "GGally"
	, "ggplot2"
	, "MLmetrics"
	, "ROCR"
	, "gemini.R"
	, "naniar"
	, "webshot2"
	, "remotes"
	, "DatabaseConnector"
)


for (lib in libs) {
  if(!require(lib, character.only = TRUE)){
    install.packages(lib, dependencies = TRUE)
  }
  library(lib, character.only = TRUE)
}


install_github_if_missing <- function(pkg, repo) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    remotes::install_github(repo)
    library(pkg)
  }
}

install_github_if_missing("Rautoml", "aphrc-nocode/Rautoml")
