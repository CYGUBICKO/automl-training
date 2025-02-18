## AUTOML Training Repo

The pipeline integrates gemini API via (gemini.R package)[https://cran.r-project.org/web/packages/gemini.R/index.html] into the auto-ml pipeline. The integration allows for automated interpretation of key outputs.

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

### Advance pipeline

- See [AUTOML Pipeline](https://github.com/CYGUBICKO/automl-pipeline)

