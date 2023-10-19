This is the R package created as supplementary material to the following publication:
Włodarczyk T. (2023). Impact of the dominant competitor on the aggressiveness host ants toward slave-makers. Behaviour 160: 911-933. DOI: 10.1163/1568539X-bja10236

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

