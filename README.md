
<!-- README.md is generated from README.Rmd. Please edit that file -->
blockrand2
==========

This R package helps you to create block randomised treatment lists for clinical studies.

Installation
------------

You can install the latest development version of the package `blockrand2` from github with

``` r
# install.packages("devtools")
devtools::install_github("hsonne/blockrand2")
```

Usage
-----

First of all, you need to load the package:

``` r
library(blockrand2)
```

### Define Document Header

Start by defining the title of the study, the author's name and the date in form of a list with list elements `title`, `author` and `date`. These information will appear on top of the created documents.

``` r
# Define the title of the study, the author and the date
header <- list(
  title = "Randomisation lists for the study: *Jokes against craziness*",
  author = "Hauke Sonnenberg",
  date = "2016-08-27"
)
```

### Define Stratum Variables and Treatments

Next, define the names and possible values of the variables that define different strata into which to group the patients that take part in your study:

``` r
# Define stratum variables
strataVars <- list(
  sex = c("male", "female"),
  crazyness = c("weak", "medium", "strong")
)
```

Define the different treatments that are to be applied to the patients with a short name and a title in the form of a named character vector:

``` r
# Define the treatments
treatments <- c(
  joke = "Tell funny jokes",
  nojoke = "Keep serious!"
)
```

### Provide Patient's Data

Of course, we need information on our patients to which we want to assign the different treatments. Let's create some random testdata based on the stratum variables already defined. Therefore, the package provides the function `createTestdata()` to which you pass the definition of stratum variables as well as the number of records to be created:

``` r
# Create some patient's testdata using the stratum variables already defined
patients <- createTestdata(strataVars, 40)
```

Let's have a look at the first lines of the created table:

``` r
head(patients)
#>   patient    sex crazyness
#> 1    P001   male    medium
#> 2    P002 female      weak
#> 3    P003 female    medium
#> 4    P004 female    strong
#> 5    P005   male      weak
#> 6    P006 female    strong
```

Once you have prepared a file (e.g. in CSV format) containing the real data you will replace the above line with something like this:

``` r
# Read patient's data from a CSV file
patients <- read.csv("patients.csv")
```

Make sure that the number of columns and the column names in your file are the same as are provided by `createTestdata()` (here: *patient*, *sex*, *crazyness*). Otherwise the following steps will not work properly.

### Create the Randomisation List Document(s)

That's more or less it. All you have to do now is to call the function `createRandomisationDoc()` to which you have to pass the `patient`'s data, the definition of `straVars` and the `treatments` as well as the information going into the `header` of the documents to be created. The argument `newpage` controls (only for output format "pdf") whether a new page is to be started after each list showing the patient's and their assigned treatment for one stratum.

``` r
# Create the randomisation list documents
files <- createRandomisationDoc(
  patients = patients,
  strataVars = strataVars,
  treatments = treatments,
  header = header,
  newpage = TRUE
)
```

The full paths to the generated files are returned invisibly. Since we stored these paths in the variable `files` we can not review these paths

``` r
# Show the paths of the created files
files
#>                                         docx 
#> "/tmp/RtmpPvYKHB/blockrand2/randomList.docx" 
#>                                         html 
#> "/tmp/RtmpPvYKHB/blockrand2/randomList.html" 
#>                                          pdf 
#>  "/tmp/RtmpPvYKHB/blockrand2/randomList.pdf"
```

but also use them from within `R` to open the created files with their appropriate applications (if defined in the `options()`):

``` r
# Open the html file in the default browser
getOption("browser")(files["html"])

# Open the pdf file in the default PDF viewer
system(paste(getOption("pdfviewer"), files["pdf"]))
```

By default, the randomisation list document is created in three formats: `"html", "word"` and `"pdf"`. This is controlled by the formal argument `format` of `createRandomisationDoc()`. If, for example you do not have LaTeX installed which is the requirement to create the document in PDF format, you may select to e.g. only generate `"word"` format:

``` r
# Create the randomisation list documents
createRandomisationDoc(
  patients = patients,
  strataVars = strataVars,
  treatments = treatments,
  header = header,
  format = "word"
)
```
