## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## tar_plan supports drake-style targets and also tar_target()
tar_plan(
  tar_target(bcus_full_data_file, "data/full-data.csv", format = "file"),
  tar_target(bcus_data, read_csv(bcus_full_data_file)),
  tar_target(bcus_modeling_data, clean_bcus_data(bcus_data)),
  tar_target(urchin_statistics_table, summary_statistics_table(bcus_modeling_data)),
  tar_target(urchin_models, fit_urchin_models(bcus_modeling_data)),
  tar_target(aic_table, get_aic_table(urchin_models)),
  tar_target(fig2, urchin_model_timeseries_plot(bcus_modeling_data, urchin_models$env_m1I)),
  tar_target(urchin_model_summary_table, model_summary_table(urchin_models$env_m1I)),
  tar_target(fig3, new_urchin_model_smooth_plot(urchin_models$env_m1I, bcus_modeling_data)),
    
  tar_render(analysis, "doc/analysis.Rmd")
  
)
