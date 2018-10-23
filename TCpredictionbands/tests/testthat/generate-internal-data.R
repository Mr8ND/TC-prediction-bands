#' This file generated the internal data to carry out the tests in R CMD check

# PB Simulation pipeline
load(system.file("data", "sample_sim.rda", package="TCpredictionbands"))
load(system.file("data", "sample_tc.rda", package="TCpredictionbands"))
load(system.file("data", "sample_tc_name.rda", package="TCpredictionbands"))
load(system.file("data", "sample_output_pipeline.rda", package="TCpredictionbands"))

# Simulation of 2 curves per AR/NonAR and Kernel/Logistic Death Type
load(system.file("data", "test_sims.rda", package="TCpredictionbands"))

internal_data <- list(sample_sim, sample_tc, sample_tc_name, 
                      sample_output_pipeline, test_sims)
names(internal_data) <- c("sample_sim", "sample_tc", "sample_tc_name",
                          "sample_output_pipeline", "test_sims")

devtools::use_data(internal_data, internal = TRUE, overwrite = TRUE)
