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
    
  # Oxygen models
  tar_target(chla_clean_file, "data/chla_clean.csv", format = "file"), 
  tar_target(chla_clean, read_csv(chla_clean_file)),
  # tar_target(chla_clean, clean_chla_data()),
  tar_target(chla_prepared, chla_model_data(chla_clean)),
  tar_target(oxygen_binned, 
             clean_and_prepare_weekly_oxygen_data(path_to_oxygen_files = "data/Search_28111952 (1)", 
                                                  chla_gam = chla_prepared)),
  tar_target(oxygen_and_chla_data, combine_oxygen_and_chla(oxygen_binned, chla_prepared)),
  tar_target(sstp_clean, clean_and_prepare_buoy_data(buoy_raw_file = "data/c46206.csv",
                                                     chla_prepared)),
  tar_target(oxygen_model_data, combine_oxygen_chla_buoy_data(oxygen_and_chla_data, sstp_clean)),
  tar_target(omno_doy, prepare_oxygen_model_data(oxygen_model_data)),
  tar_target(oxygen_models, fit_oxygen_models(omno_doy)), 
  tar_target(fig4_data, make_figure_4_data(oxygen_models$omt3_env_AR2, omno_doy)),
  tar_target(plot_fig4, fig4(fig4_data)),
  tar_target(plot_fig5, fig5(oxygen_models$omt3_env_AR2)),
  tar_target(fig6_data, make_figure_6_data(oxygen_models$omt3_env_AR2, omno_doy)),
  tar_target(plot_fig6, fig6(fig6_data)),
  
  # Chlorophyll-a models
  tar_target(chla_prepared2, pre_prepare_chlorophyll_model_data(chla_clean)),
  tar_target(sstp_clean2, prepare_buoy_data_for_chlorophyll_model(buoy_raw_file = "data/c46206.csv",
                                                                  chla_prepared2)),
  tar_target(chla_and_buoy_data, combine_chla_and_buoy_data(chla_prepared2, sstp_clean2)),
  tar_target(chlorophyll_model_data, prepare_chlorophyll_model_data(chla_and_buoy_data)),
  tar_target(chla_models, fit_chlorophyll_models(chlorophyll_model_data)),
  tar_target(plot_fig7, fig7(chla_models$m1c_AR1)),
  
  # WCVI analysis
  
  tar_render(analysis, "doc/analysis.Rmd")
  
)
