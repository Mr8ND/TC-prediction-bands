create_data:
	Rscript main/0-save_train_test_names.R

simulate_tcs:
	Rscript main/2-simulate_test_paths.R
	Rscript main/3-simulate_100_75_addl_paths.R
	
creation_pbs:
	Rscript main/4-prediction_band_pipeline.R
	