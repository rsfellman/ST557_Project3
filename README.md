ST558 Project 3
================
Rachel Fellman & Sabrina Dahl

# Description
This repository is where Sabrina Dahl and Rachel Fellman will be working together on project 3 for ST 558. We will analyze diabetes data using 6 different model types and create 5 output files fo different levels of the `Education` variable that will be linked below.

# R Packages
The following packages were used to in the creation of this project:

`tidyverse`  
`caret`  
`rmarkdown`  

Used in conjunction with caret to create the models:  
`Rweka`  
`rjava`  
`randomForest`  
`rpart`  
`glmnet`  
`Matrix`


# Code to Render Files

To render the files we first have to create a data frame that has the file names to output to and a list with each name for using in the `render` function. 
```{r, eval =FALSE}
#get education levels
ed.levels <- unique(diabetes.1$Education)
#create filenames
output_file <- paste0("Education_level", ed.levels, ".md")
#create a list which just the education level parameter
params = lapply(ed.levels, FUN = function(x){list(Ed = x)})
#put into data frame
reports <- tibble(output_file, params)
```

Next, using the `apply` and `render` functions, we will knit the documents.
```{r}
apply(reports, MARGIN = 1, 
      FUN = function(x){
        render(input = "project3.rmd", output_file = x[[1]], params = x[[2]]) 
      })
```


# Links to files

[Education Level 1](Education_level1.md)  
[Education Level 3](Education_level3.md)  
[Education Level 4](Education_level4.md)  
[Education Level 5](Education_level5.md)  
[Education Level 6](Education_level6.md)

