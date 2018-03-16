# Hurricanes_701

This repository stores the documentation for new approaches to predicting 
credible bands for full paths of tropical storms (TCs) using just a few initial 
points. We leveraging extensions on regression models to generate curves, 
combined with multiple approaches to create credible bands for the full paths. 
These approaches utilizes, kernel density estimation, utilize depth-based 
relationships between curves, and also leverage geometric properties of the 
space. This project was specifically designed with the use of the 
[*HURDAT 2*](http://www.aoml.noaa.gov/hrd/hurdat/hurdat2-1851-2015-070616.txt) 
data set from the [*National Oceanic and Atmospheric Administration*](
http://www.aoml.noaa.gov/hrd/hurdat/Data_Storm.html). 

## Replication of Results
As is generally the case in papers today, our analysis is replicable. The files
in the `main/` folder store the necessary files to run our analysis. The viewer 
should note that the analysis pipeline takes a long while and should decide if 
they would like the full results or just test out the analysis on a few samples. 
Additionally, the R package `TCcrediblebands`, which is included in this repo 
provides the user with the ability to analysis different generated curves with 
our credible band approaches and also TC paths that were not available when 
we developed with package.

### Running files
All files in `main` are meant to be run from the upper projection location 
(`Hurricanes_701`).

### `TCcrediblebands` package

To install the latest version please do

```r
library(devtools)
devtools::install_github(repo = 'Mr8ND/Hurricanes_701/TCcrediblebands')
library(TCcrediblebands)
```

## Comments
This project started in Carnegie Mellon University's 10-701: Introduction to 
Machine Learning in the Fall of 2016. After this class, we have worked with 
support from [Professor Chad Schafer](http://www.stat.cmu.edu/~cschafer/).

## Contributors 
- Nic Dalmasso ([`Mr8ND`](https://github.com/Mr8ND))
- Robin Dunn  ([`RobinMDunn`](https://github.com/RobinMDunn))
- Benjamin LeRoy ([`benjaminleroy`](https://github.com/benjaminleroy))

This repository is public and owned by Nic Dalmasso, Robin Dunn and Benjamin 
LeRoy.

