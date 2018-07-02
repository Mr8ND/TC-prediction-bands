How to run files (currently a dumping ground for thoughts).

## Credible Interval Pipeline

To run the `credible_interval_pipeline.R` we would do the following from the `Hurricanes_701` folder:

```{r}
Rscript main/credible_interval_pipeline.R a b
```
where `a` and `b` are integer values (`a <= b`). This allows of the creation of the credible interval objects to be created in "parallel".

##  Merging files

To run `collect_parallel_data_files.R` we would run the following from the `Hurriances_701` folder:

```{r}
Rscript main/collect_parallel_data_files.R a b c
```

where 

 + `a` is the string that all files to merge should have at the beginning (note if you files with the same beginning that you don't wish to merger you'll need to change there names), e.g. `output_pipeline_alphalevel0.1` (already assumes location is `main/data`).

 + `b` the variable name you'd like to save the final list object as (e.g. `output_list_pipeline`).

 + `c` the final part the string for the `.Rdata` file that will save the object named `b`, this needs to exclude `.Rdata` in the end. (e.g. `_complete_2018-07-02`)

