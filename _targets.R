# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed.
library(worcs)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed.
set.seed(7998)
worcs::load_data()
# Replace the target list below with your own:
list(
  tar_target(
    name = tab_descriptives,
    command = brabant_psychometrics(df, dict)
  )
  , tar_target(
    name = df_anal,
    command = data_preprocessing(df, dict, tab_descriptives)
  )
  , tar_target(
    name = df_withmiss,
    command = impute_data(df_anal, dict)
  )
  , tar_target(
    name = df_features,
    command = keep_analysis_vars(df_withmiss, dict)
  )
  , tar_target(
    name = growth_model,
    command = do_growthcurves(df_features)
  )
  , tar_target(
    name = forest_files,
    command = plant_forests(df_features, growth_model)
  )
  , tar_target(
    name = merged_summarydata,
    command = merge_forests(forest_files)
  )
  , tar_target(
    name = vimps,
    command = aggregate_vim(merged_summarydata)
  )
  , tar_target(
    name = pdps,
    command = create_pdp(merged_summarydata, df_features, vimps)
  )
  , tar_target(
    name = pdps_plots,
    command = plot_pdps(pdps)
  )
  , tar_target(
    name = vimps_plots,
    command = plot_vimps(pdps)
  )
  , tarchetypes::tar_render(manuscript, "dashboard.Rmd", cue = tar_cue("always"), priority = 0.5)
  , tar_file (
    name = create_index,
    command = { file.rename("dashboard.html", "index.html"); return("index.html")},
    cue = tar_cue("always"),
    priority = 0
  )
)
