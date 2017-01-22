# Hurricanes_701

10-701: Introduction to Machine Learning

Carnegie Mellon University

Fall 2016

This repository stores the documentation of our new approach to predicting the 
full paths of tropical storms (TCs) using just a few initial points by 
leveraging extensions on regression models to generate curves, combined with 
updates to use weighting metrics from a compression of the training data 
using spectral analysis to get a density estimation for the training data,
and two dimensional confidence bands. This approach was specifically designed
with the use of the 
[*HURDAT 2*](http://www.aoml.noaa.gov/hrd/hurdat/hurdat2-1851-2015-070616.txt) 
data set from the 
[*National Oceanic and Atmospheric Administration*](http://www.aoml.noaa.gov/hrd/hurdat/Data_Storm.html). 

## Replication of Results
As is generally the case in papers today, our analysis is replicable. Follow the
below instructions to do so.

### Initial Folder Creation 
could do a `make` file to get the data from the website and put it in a data folder

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
- `cleaning_data_test_train_subset.py`: python process to clean and prepare data for R code
- `create_folders.sh`: bash script to make the correct subdirectories for simulated data created in `Simulate_Validation_Paths.R`
- `Simulate_Validation_Paths.R`: created simulated paths for the training data$^*$
- `initial_pipeline.R`: Creates Distance matrices, Markov Matrices and more on the test data (Spectral Analysis).$^*$
- `loocv.R`: Tune parameters for density map estimation using LOOCV $^{**}$
- `second_pipeline`: Estimate likelihood of generated paths using Spectral Analysis

$^*$ means it takes a long time to run

## Contributors 
- Nic Dalmasso ([`Mr8ND`](https://github.com/Mr8ND))
- Robin Dunn  ([`RobinMDunn`](https://github.com/RobinMDunn))
- Benjamin LeRoy ([`benjaminleroy`](https://github.com/benjaminleroy))

This repository is public and owned by Nic Dalmasso, Robin Dunn and Ben LeRoy.

