This is the R package created as supplementary material to the manuscript entitled "Impact of dominant competitor on the aggressiveness of the host ants toward slave-makers"
# Usage
Install package
```
if(!require(devtools)) install.packages("devtools")
devtools::install_github("TomVuod/fusca_counteradaptation", build_vignette = TRUE)
```
Load package
```
library(fusca.defence)
```
See the report with statistical analyses
```
vignette("Statistical_tests", package = "fusca.defence")
```
Generate figures 3 and 4
```
generate_fig3()
generate_fig4()
```
Check available datasets
```
data(package = "fusca.defence")
```

