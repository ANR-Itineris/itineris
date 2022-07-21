# R functions for Itineris

Before the R package release, download the functions from https://github.com/ANR-Itineris/itineris/tree/main/functions in a folder and run from the same folder

```
source("setup.R")
source("isotop_dataframe.R")
source("isotop_3d.R")
```

Now load the dataset

```
path.data <- "https://raw.githubusercontent.com/ANR-Itineris/itineris/main/data/"
df.isotop <- read.table(paste0(path.data, "isotop_results.tsv"), sep = "\t", header = T)
```

Create the interactive data table from this dataset

```
isotop_dataframe(df.isotop)
```

Create the 3D plot from the same dataset

```
isotop_3d(df.isotop)
```
