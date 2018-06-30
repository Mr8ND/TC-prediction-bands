How to run files (currently a dumping ground for thoughts).

## Credible Interval Pipeline

To run the `credible_interval_pipeline.R` we would do the following from the `Hurricanes_701` folder:

```{r}
Rscript main/credible_interval_pipeline.R a b
```
where `a` and `b` are integer values (`a <= b`). This allows of the creation of the credible interval objects to be created in "parallel".

