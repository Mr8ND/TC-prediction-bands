all:
	make create_data
	make simulate_tcs
	make create_pbs
	make validate_pbs
	make create_figs
	make create_diagrams

create_data:
	Rscript main/0-save_train_test_names.R

simulate_tcs:
	Rscript main/2-simulate_test_paths.R
	Rscript main/3-simulate_100_75_addl_paths.R
	
create_pbs:
	Rscript main/4-prediction_band_pipeline.R 1 25
	Rscript main/4-prediction_band_pipeline.R 26 50
	Rscript main/4-prediction_band_pipeline.R 51 75
	Rscript main/4-prediction_band_pipeline.R 76 100
	Rscript main/4-prediction_band_pipeline.R 101 125
	Rscript main/4-prediction_band_pipeline.R 126 150
	Rscript main/4-prediction_band_pipeline.R 151 175
	Rscript main/4-prediction_band_pipeline.R 176 200
	Rscript main/4-prediction_band_pipeline.R 201 225	
	Rscript main/4-prediction_band_pipeline.R 226 250
	Rscript main/4-prediction_band_pipeline.R 251 275
	Rscript main/4-prediction_band_pipeline.R 276 306
	
	Rscript main/4.1-collect_parallel_data_files.R output_pipeline_alphalevel0.1 output_pipeline _all

validate_pbs:
	Rscript main/5-simulation_validation_pipeline.R Test_Sims_100_addl.Rdata
	Rscript main/5-simulation_validation_pipeline.R Test_Sims_75_addl.Rdata

create_figs:
	Rscript main/R0-aic_AR_nonAR.R
	Rscript main/R1-resids_vs_fits.R
	Rscript main/R2-acc_vs_area_result.R
	Rscript main/R3-pb_validation_result.R
	Rscript main/R4-pb_gallery_for_paper.R
	Rscript main/R5-discussion_visuals.R
	Rscript main/R6-tc_survival_times_dist_supplement_vis.R

create_diagrams:
	Rscript main/F0-spherical_pb_vis.R
	Rscript main/F1-delta_ball_vis.R
