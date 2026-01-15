# cyCombine operator

##### Description

The `cyCombine operator` is a package for combining single-cell cytometry datasets, which increases the analytical flexibility and the statistical power of the analyses while minimizing technical noise.

##### Usage

Input projection|.
---|---
`y-axis`        | The value of measurement signal of the channel/marker
`row`           | Observations (event ID)
`column`        | Represents the variables (e.g. channels, markers)
`labels`        | Batch identifier (required); additional labels for covariates and anchors

Input parameters|.
---|---
`seed`        | Seed for random number generation. (if less than 0, set a Random seed)
`norm_method` | Standardization method used within each batch. (default= scale)

Output relations|.
---|---
`value`        | The corrected value

##### Details

CyCombine offers a method to robustly integrate cytometry data from different batches, experiments, or even different experimental techniques.
It reduce technical variance between datasets using the [ComBat](https://www.rdocumentation.org/packages/sva/versions/3.20.0/topics/ComBat) package. 

##### See Also

[cyCombine_Nature_article](https://www.nature.com/articles/s41467-022-29383-5)
, [cyCombine_manual](https://biosurf.org/cyCombine_ref_manual.html)

