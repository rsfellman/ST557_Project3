ST558 Project 3
================
Rachel Fellman & Sabrina Dahl

# Description

This repository is where Sabrina Dahl and Rachel Fellman will be working
together on project 3 for ST 558. We will analyze data using different
models and create 5 output files that will be linked below.

# R Packages

`tidyverse` `caret` `rmarkdown` ’pwalk\`

# Code to Render Files

To render the files we first have to create a data frame that has the
file names to output to and a list with each name for using in the
`render` function.

``` r
#get education levels
ed.levels <- unique(diabetes.1$Education)
#create filenames
output_file <- paste0(ed.levels, ".md")
#create a list which just the education level parameter
params = lapply(ed.level, FUN = function(x){list(ed = x)})
#put into data frame
reports <- tibble(output_file, params)
```

Next, using the `pwalk` function, we will knit the documents.

``` r
pwalk(reports, render, input = "project3.rmd")
```

# Links to files
[ed level 1](Education_level1.html)
