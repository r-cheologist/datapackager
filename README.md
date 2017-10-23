# README
## Introduction
Many (if not all) data analytical projects integrate raw input data from
upstream producers with tools and scripts aiding in their interpretation.
Where [`R`](https://r-project.org) is used as the platform for such analysis,
the canonical `R` packaging infrastructure (as e.g. described
[here](https://cran.r-project.org/doc/manuals/r-release/R-exts.html) in
detail) is well suited for this integration and the representation of a
package tracking input data and its analysis parallely might
(minimally) look something like this in the file system:

- `analysis.of.x.2017` (top level directory named as the resulting package)
    - `DESCRIPTION` ([standardized](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#The-DESCRIPTION-file)
	  file containing basic information about the package)
	- `NAMESPACE` (file describing what functions/objects the resulting package
	  provides and requires from other packages; see 
	  [here](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Package-namespaces) for details)
	- `R` (Directory containing `R` functions)
	    - `Generalizable_Function_A.R`
		- `Generalizable_Function_B.R`
		- ...
	- `data` (directory containing `R` objects saved using `save(...)` and
	  [lazy loaded](https://cran.r-project.org/doc/manuals/r-release/R-ints.html#Lazy-loading) on attaching the resulting package)
	    - `raw_data.Rda`
	- `inst` (directory holding other material **inst**alled into `R`s infrastructure
	  along with the package)
	    - `extdata` (directory holding compressed **ext**ernal raw **data**)
		    - `raw_data.csv.zip`
	    - `scripts` (directory holding work flow-documenting scripts)
		    - `00_Script_Documenting_Data_Import.R`
			- `01_Script_Documenting_Exploratory_Preliminary_Analysis.R`
			- ...

After building and loading, such a package makes readily available from within `R`
a) raw data (`inst/extdata/raw_data.csv.zip`), b) its parsed, immediately `R`-accessible
counterpart (`data/raw_data.Rda`, respectively a `raw_data` object),
c) generalizable custom `R` functionality (`R/...`), as well as d) work flow
documentation (`inst/scripts/...`) in a neat, integrated and easily
distributed unit. The structure is easily extended and further customized to
e.g. include a manuscript when using [`rmarkdown`](https://cran.r-project.org/web/packages/rmarkdown)
and/or [`bookdown`](https://cran.r-project.org/web/packages/bookdown)
(e.g. in `inst/mansucript/manuscript.Rmd`).

When using `R`'s packaging infrastructure in this manner, two major challenges are

1. The complexity of establishing the project-specific infrastructure and

2. Obstacles in distributing the data at the basis of the documented project, which
   may, among other issues, be rooted in
    - Size restrictions of shipped data in a targeted package repository
       ([CRAN](https://cran.r-project.org), for example, as of 23.10.2017 states "...
	   As a general rule, neither data nor documentation should exceed 5MB.").
    - The need to enforce data onwnership/licensing in the context of freely providing
       peer review of analytical methodology and strategies employed. 


Building heavily on the toolkit provided by the excellent
[`devtools`](https://cran.r-project.org/web/packages/devtools) package, `datapackageR`
aims to aleviate these difficulties and provides a simple interface for the management
of raw data and deriving objects within the packaging infrastructure, emphasizing
cryptographically strong data integrity assurance (using
[digest](https://cran.r-project.org/web/packages/digest)) and mechanisms for separate
distribution of data and analyzing code.

## How to get `datapackageR` set up?
`datapackageR` is targeted to eventually be released on [CRAN](https://cran.r-project.org),
but until that happens and for the latest features and fixes, the following installation
procedure should be followed:

    install.packages(devtools)
	devtools::install_bitbucket("graumannlabtools/datapackageR")

## Example usage
### Comprehensive presentation of functionality
1. Load the required tools

        library(datapackageR)
	    library(magrittr)
	    requireNamespace("devtools")
	
2. Define a temporary working directory & packaging root

        pkg_root <- tempdir() %>%
		  file.path("packagetest")
	  
3. Generate a (dummy) data file

        data.frame(
		    x   = 1,
			y   = 1:10,
			fac = sample(LETTERS[1:3], 10, replace = TRUE)) %>%
          write.table(
            file      = file.path(dirname(pkg_root), "data_dummy.tsv"),
	        sep       = "\t",
			col.names = TRUE,
			row.names = FALSE)

4. Create the packaging skeleton, at the same time inserting the dummy data file from above:

        data_catalogue <- init(
          root               = pkg_root,
		  objects_to_include = file.path(dirname(pkg_root), "data_dummy.tsv"),
          root               = pkg_root,
          parsing_function   = "read.csv",
          parsing_options    = list(sep = "\t", stringsAsFactors = FALSE)))
		  
    1. Investigate the resulting `data_catalogue`
   
           str(data_catalogue)
		
    2. (Crudely) investigate the result in the file system
   
           list.files(pkg_root, recursive = TRUE)
	  
### Exemplary use case "seperately distributed data"
* Summary of set up
* Configuration
* Dependencies
* Database configuration
* How to run tests
* Deployment instructions

### Contribution guidelines ###

* Writing tests
* Code review
* Other guidelines

### Who do I talk to? ###

* Repo owner or admin
* Other community or team contact