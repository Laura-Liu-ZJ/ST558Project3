# ST558Project3

### Files
If you want to run this app in your Rstudio you need to copy and the following code in your R console.

```
library(shiny)

# Use runGitHub to get repo
runGitHub("ST558Project3", "rstudio")

# Run a tar file directly
runUrl("https://github.com/Laura-Liu-ZJ/ST558Project3/archive/master.tar.gz")
```

The dataset is `heart_failure_clinical_records_dataset.csv`, you can also get it from this [link](http://archive.ics.uci.edu/ml/datasets/Heart+failure+clinical+records).

The original static file is `project3_static.Rmd`. I transformed this static file into an interactive application by building the app frame by `ui.R` as well as`server.R` and filling it by `info` and `tab` folders. The `info` folder includes all the introductions with `.md` files and the `tab` folder includes all the codes for each page with `.R` files.

If you want to look at and try to use this app, you can click this [link](https://zhijun-liu.shinyapps.io/st558project3/).

### Packages

This app needs `shiny`, `shinydashboard`, `dashboardthemes`, `DT`, `ggplot2`, `GGally`, `tidyverse`, `shinythemes`, `plotly`, `rgl`, `pca3d` and `tree` packages. If you haven't installed these packages before, you have to install these packages first.

### Thank you!
