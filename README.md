# TC-prediction-bands

This repository stores the documentation for new approaches to predicting 
prediction bands for full paths of tropical storms (TCs) using just a few initial 
points. We leveraging extensions on regression models to generate curves, 
combined with multiple approaches to create prediction bands for the full paths. 
These approaches utilizes, kernel density estimation, utilize depth-based 
relationships between curves, and also leverage geometric properties of the 
space. This project was specifically designed with the use of the 
[*HURDAT 2*](http://www.aoml.noaa.gov/hrd/hurdat/hurdat2-1851-2015-070616.txt) 
data set from the [*National Oceanic and Atmospheric Administration*](
http://www.aoml.noaa.gov/hrd/hurdat/Data_Storm.html). 

**Table of Contents:**

1. [Replication of Results](#1-replication-of-results)

    1.1. [`makefile` usage](#11-makefile-usage)
    - [Server Specs](#server-specs)
    
    1.2. [Special Script Guidelines](#12-special-script-guidelines)

2. [`TCpredictionbands` package](#2-tcpredictionbands-package)
3. [Comments](#3-comments)
4. [Contributors](#4-contributors)

## 1 Replication of Results

As is generally the case in papers today, our analysis is replicable. The files
in the `main/` folder store the necessary files to run our analysis. The viewer 
should note that the analysis pipeline takes a long while and should decide if 
they would like the full results or just test out the analysis on a few samples. 
Additionally, the R package `TCpredictionbands` (see 
[associated section](#2-tcpredictionbands-package)), which is included in this
repo, provides the user with the ability to analysis different generated
curves with our prediction band approaches and also TC paths that were not
available when we developed with package.

Files in `main/` are titled in the following format:
```{r}
__-name_of_script.R
```
where the `__` can be in the following forms:

- {integers}: main scripts
- R{integers}: scripts to peform results analysis, and well as scripts to 
    create results and discussion figures and tables
- F{integers}: files to create figures that demonstrate PB approaches 
    (example figures)
- S0: script to create png files to aid in author's diagonstics of the 
    implimented framework
    
The above ordering (with integer ordering within each subsection) can be used
to completely replicate our analysis. More specifically we provide a `makefile`
to add in the replication of our work (see 
(following subsection)[#11-makefile-usage]).

### 1.1 `makefile` usage
To run the full analysis we provide a `makefile` containing recipes to preform
all final analysis and reproduce tables and figures for the paper.

**Note: would take multiple days to run if you do the full analysis without 
"psuedo-parallelization". See notes in the following list.** 

+ `make all`: runs the full analysis 
+ `make create_data`: download, clean and create raw data for analysis
+ `make simulate_tcs`: create simulated curves for PB creation and PB validation
    analysis

    For this complete process (`make simulate_tcs`) it took around 43 hours to 
    run on a server ([specs](#server-specs)). It takes 29 hours to run the 
    creation of 350 simulated paths for each TC (309) of all simulation types
    using the following line of code:
    ```{bash}
    Rscript main/2-simulate_test_paths.R
    ```
+ `make create_pbs`: analysis to create PBs
    
    Within the creation of the pbs (completed in this `make` command) we break up the process into 
    blocks making PBs for 25 TCs in each block. A single block takes around 23 hours.
    When we ran the code we broke up the analysis on multiple servers with multiple 
    blocks running at the same time (with similar to those mentioned [here](#server-specs)). One should expect around 276 hours of 
    processing time for this step (excluding the recombining of all the PBs into 1 location with
    ```{bash}
    Rscript main/4.1-collect_parallel_data_files.R output_pipeline_alphalevel0.1 output_pipeline _all
    ```
    which is the final call in `make create_pbs`). See comments in 
    (Special script guidelines)[#12-special-script-guidelines] for running of either 
    file associated with this section (i.e. `4-prediction_band_pipeline.R` or 
    `4.1-collect_parallel_data_files.R`).

+ `make validate_pbs`: validation analysis for PBs
    This call runs the validation script `5-simulation_validation_pipeline.R` twice for the sets of (100) and (75)     new simulated curves. See [special script guidelines](#12-special-script-guidelines) if you'd like to run the     analysis yourself.
+ `make create_figs`: create figures and tables for paper
+ `make create_diagrams`: create diagrams for the paper

#### Server Specs

We ran this analysis on a Statistics and Data Science department server 
(`lerna`) with the following specs:

`Intel(R) Xeon(R) CPU X5680 @ 3.33GHz`

### 1.2 Special Script Guidelines
Two of the files in the `main/` folder, `4-prediction_band_pipeline.R` and 
`4.1-collect_parallel_data_files.R`, require input parameters of the following 
structure:

#### 1.2.1 `4-prediction_band_pipeline.R`

To run the `4-prediction_band_pipeline.R` we would do the following from the 
`TC-prediction-bands` folder:
```{bash}
Rscript main/4-prediction_band_pipeline.R a b
```
where `a` and `b` are integer values (`a <= b`). This call will create PBs for 
TCs in the testing data with indices `a` to `b` and store them in an `.Rdata` 
file labeled `'output_pipeline_alphalevel0.1_[a]_[b].Rdata` (with the `[a]` 
subsituted for the string represented of the integer `a`). This allows of the 
creation of the PB objects to be created in "parallel". 

#### 1.2.2 `4.1-collect_parallel_data_files.R`

To run `4.1-collect_parallel_data_files.R` we would run the following from the 
`TC-prediction-bands` folder:

```{bash}
Rscript main/4.1-collect_parallel_data_files.R a b c
```

where 

 + `a` is the string that all files to merge should have at the beginning. 
 Note 1: if you files with the same beginning that you don't wish to merger 
 you'll need to change there names (e.g. `output_pipeline_alphalevel0.1`).
 Note 2: this file already assumes the location of the data files to merger is 
 `main/data`.

 + `b` the variable name you'd like to save the final list object as 
 (e.g. ` output_pipeline`).

 + `c` the final part the string for the `.Rdata` file that will save the 
 object named `b`, this needs to exclude `.Rdata` in the end. (e.g. `_all` or 
 `_all_2018-07-02`)
 
*If we followed all the "e.g"s then we would be saving a object called 
`output_pipeline` to the file*
```{bash}
output_pipeline_alphalevel0.1_all.Rdata
```

#### 1.2.3. `5-simulation_validation_pipeline.R`

To run `5-simulation_validation_pipeline.R` we would run the following from the
`TC-prediction-bands` folder:

```{bash}
Rscript 5-simulation_validation_pipeline.R a
```

where 

  + `a` is the string for the file name in `main/data/` folder that contains an
  environment that contains lists of simulated curves for the test curves.
  
Note that we save the output in a file called `sim_validation_resultsXX.Rdata`
where `XX` is the number of simulated curves for each test curve.

### 2 `TCpredictionbands` package

To install the latest version please do

```r
library(devtools)
devtools::install_github(repo = 'Mr8ND/TC-prediction-bands/TCpredictionbands')
library(TCpredictionbands)
```

## 3 Comments
This project started in Carnegie Mellon University's 10-701: Introduction to 
Machine Learning in the Fall of 2016. After this class, we have worked with 
support from [Professor Chad Schafer](http://www.stat.cmu.edu/~cschafer/).

## 4 Contributors 
- Nic Dalmasso ([`Mr8ND`](https://github.com/Mr8ND))
- Robin Dunn  ([`RobinMDunn`](https://github.com/RobinMDunn))
- Benjamin LeRoy ([`benjaminleroy`](https://github.com/benjaminleroy))

This repository is public and owned by Nic Dalmasso, Robin Dunn and Benjamin 
LeRoy.

