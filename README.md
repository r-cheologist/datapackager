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
	- `inst` (directory holding other material *inst*alled into `R`s infrastructure
	  along with the package)
	    - `extdata` (directory holding compressed *ext*ernal raw *data*)
		    - `raw_data.csv.zip`
	    - `scripts` (directory holding work flow-documenting scripts)
		    - `00_Script_Documenting_Data_Import.R`
			- `01_Script_Documenting_Exploratory_Preliminary_Analysis.R`
			- ...

After building and loading, such a package makes readily available raw data
(`inst/extdata/raw_data.csv.zip`), its parsed, immediately `R`-accessible
counterpart (`data/raw_data.Rda`, respectively a `raw_data` object),
generalizable custom `R` functionality (`R/...`), as well as work flow
documentation (`inst/scripts/...`) in a neat, integrated and easily
distributed unit. The structure is easily extended and further customized to
e.g. include a manuscript when using [`rmarkdown`](https://rmarkdown.rstudio.com)
and/or [`bookdown`](https://bookdown.org) (e.g. in `inst/mansucript/manuscript.Rmd`).

Two major challenges when using `R`'s packaging infrastructure in this manner are

1. The complexity of establishing the project-specific infrastructure and

2. Obstacles in distributing the data at the basis of the documented project, which
   may, among other issues, be rooted in
   a. Size restrictions of shipped data in a targeted package repository
      ([CRAN](https://cran.r-project.org), for example, as of 23.10.2017 states "...
	  As a general rule, neither data nor documentation should exceed 5MB.").
   b. The need to enforce data onwnership/licensing in the context of freely providing
      peer review of analytical methodology and strategies employed. 


`datapackageR`
### What is this repository for? ###

* Quick summary
* Version
* [Learn Markdown](https://bitbucket.org/tutorials/markdowndemo)

### How do I get set up? ###

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