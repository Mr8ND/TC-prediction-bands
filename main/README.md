How to run files (currently a dumping ground for thoughts).

## File naming
Files are indexed by the order that they should be run in. As noted in the parent directory, all these files should be run in the parent directory.

Ordering: 
- integers: main scripts
- R[integers]: scripts to peform results analysis, and well as scripts to create results and discussion figures and tables
- F[integers]: files to create figures that demonstrate PB approaches (example figures)
- S0: script to create png files to aid in Author's diagonstics of the implimented framework

## Prediction Interval Pipeline

To run the `prediction_band_pipeline.R` we would do the following from the `TC-prediction-bands` folder:

```{bash}
Rscript main/prediction_band_pipeline.R a b
```
where `a` and `b` are integer values (`a <= b`). This allows of the creation of the prediction interval objects to be created in "parallel".

##  Merging files

To run `collect_parallel_data_files.R` we would run the following from the `Hurriances_701` folder:

```{bash}
Rscript main/collect_parallel_data_files.R a b c
```

where 

 + `a` is the string that all files to merge should have at the beginning (note if you files with the same beginning that you don't wish to merger you'll need to change there names), e.g. `output_pipeline_alphalevel0.1` (already assumes location is `main/data`).

 + `b` the variable name you'd like to save the final list object as (e.g. `output_list_pipeline`).

 + `c` the final part the string for the `.Rdata` file that will save the object named `b`, this needs to exclude `.Rdata` in the end. (e.g. `_complete_2018-07-02`)



# For running of files:
To run the full analysis we provide a `makefile` containing recipes to preform
all final analysis and reproduce tables and figures for the paper.

**Note: would take multiple days to run if you do the full analysis without 
"psuedo-parallelization".** 

+ `make all`:
+ `make 



We ran this analysis on a department server (lerna) with the following specs:

...

### make simulate_tcs

For complete 
```{bash}
make simulate_tcs
```
it took around 43 hours to run on a server. Where it took
```{bash}
Rscript main/2-simulate_test_paths.R
```
the larger part of the process 29 hours to run.

### make create_pbs

Within the creation of the pbs (see `makefile`) we break up the process into 
blocks making PBs for 25 TCs in each block. A single block takes around ____.
When we ran the code we broke up the analysis on multiple servers with multiple 
blocks running at the same time. One should expect around ____ hours of 
processing time (excluding the recombining of all the PBs into 1 location with
```{bash}
	Rscript main/4.1-collect_parallel_data_files.R output_pipeline_alphalevel0.1 output_pipeline _all
```
which is the final call in `make create_pbs`).


