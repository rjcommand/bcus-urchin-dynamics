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
  tar_target(aic_table, get_aic_table(urchin_models))
# target = function_to_make(arg), ## drake style

# tar_target(target2, function_to_make2(arg)) ## targets style

)
