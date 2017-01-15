# Hurricanes_701

10-701: Introduction to Machine Learning

Carnegie Mellon University

Fall 2016

This repository stores the documentation of our analysis ... (blah blah blah ... fill in)


## Replication of Results
As is generally the case in papers today, our analysis is replicable. Follow the
below instructions to do so.

### Location of Important Code Files
Files in `code/final_script` directory can replicate the work presented in the 
paper. No initial `set seed` was used in the initial segmentation of the 
tropical storms (TCs), and as such, to completely replicate the work, you'd need
to gather the split from Ben LeRoy, otherwise, go into 
`cleaning_data_test_train_subset.py` and change line 95 to have 
`run_initial_time = True`.

### Running files
All files (`python` and `R`) should be run from the upper projection location (
`Hurricanes_701`) which we notate as the `project_location`. Python file 
(`.py`) should be run using `python3`.

The `python` file requires the following packages:
- `numpy`: general data structure
- `pickle`: saving files (although all important files are saved as `csv`s for 
`R`)
- `re`: general expression manipulations
- `collections`: table and counting structure
- `matplotlib`,`matplotlib.pyplot`: plotting
- `sys`: file location

`R` files require the following libraries:
- `geosphere`: distances and bearing on spheres (for the earth: which is non euclidean)
- `plyr`: 
- `datamart`: 
- `kknn`: kNN algorithm, and kNN density estimator
- `rworldmap`: plotting 2d surfaces of the earth
- `plot3D`: plotting 3d compressions
- `xtable`: creating latex tables for presentation

### Order of Files to Run
- `cleaning_data_test_train_subset.py`:
- `initial_pipeline.R`:
- `loocv.R`
- ...


## Contributors 
- Nic Dalmasso ([`Mr8ND`](https://github.com/Mr8ND))
- Robin Dunn  ([`RobinMDunn`](https://github.com/RobinMDunn))
- Benjamin LeRoy ([`benjaminleroy`](https://github.com/benjaminleroy))

This repository is public and owned by Nic Dalmasso, Robin Dunn and Ben LeRoy.

