# README
## Introduction
Many (if not all) data analytical projects integrate raw input data from
upstream producers with tools and scripts aiding in their interpretation. Where
[`R`](https://r-project.org) is used as the platform for such analysis, the
canonical `R` packaging infrastructure (as e.g. described [here](https://cran.r-project.org/doc/manuals/r-release/R-exts.html)
in detail) is well suited for this integration and the representation of a
package tracking input data and its analysis parallely might (minimally) look
something like this in the file system:

- `analysis.of.x.2017` (top level directory named as the resulting package)
    - `DESCRIPTION` ([standardized](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#The-DESCRIPTION-file)
      file containing basic information about the package)
    - `NAMESPACE` (file describing what functions/objects the resulting package
      provides and requires from other packages; see [here](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Package-namespaces)
      for details)
    - `R` (Directory containing `R` functions)
        - `Generalizable_Function_A.R`
        - `Generalizable_Function_B.R`
        - ...
    - `data` (directory containing `R` objects saved using `save(...)` and
	    [lazy loaded](https://cran.r-project.org/doc/manuals/r-release/R-ints.html#Lazy-loading)
	    on attaching the resulting package)
        - `raw_data.Rda`
    - `inst` (directory holding other material **inst**alled into `R`s
      infrastructure along with the package)
        - `extdata` (directory holding compressed **ext**ernal raw **data**)
        - `raw_data.csv.zip`
        - `scripts` (directory holding work flow-documenting scripts)
            - `00_Script_Documenting_Data_Import.R`
            - `01_Script_Documenting_Exploratory_Preliminary_Analysis.R`
            - ...

After building and loading, such a package makes readily available from within
`R` a) raw data (`inst/extdata/raw_data.csv.zip`), b) its parsed, immediately
`R`-accessible counterpart (`data/raw_data.Rda`, respectively a `raw_data`
object), c) generalizable custom `R` functionality (`R/...`), as well as d) work
flow documentation (`inst/scripts/...`) in a neat, integrated and easily
distributed unit. The structure is easily extended and further customized to
e.g. include a manuscript when using [`rmarkdown`](https://cran.r-project.org/web/packages/rmarkdown)
and/or [`bookdown`](https://cran.r-project.org/web/packages/bookdown) (e.g. in
`inst/mansucript/manuscript.Rmd`).

When using `R`'s packaging infrastructure in this manner, two major challenges
are

1. The complexity of establishing the project-specific infrastructure and

2. Obstacles in distributing the data at the basis of the documented project,
   which may, among other issues, be rooted in
    - Size restrictions of shipped data in a targeted package repository
      ([CRAN](https://cran.r-project.org), for example, as of 23.10.2017 states
      "... As a general rule, neither data nor documentation should exceed
      5MB.").
    - The need to enforce data onwnership/licensing in the context of freely
      providing peer review of analytical methodology and strategies employed. 


Building heavily on the toolkit provided by the excellent [`devtools`](https://cran.r-project.org/web/packages/devtools)
package, `datapackageR` aims to aleviate these difficulties and provides a
simple interface for the management of raw data and deriving objects within the
packaging infrastructure, emphasizing cryptographically strong data integrity
assurance (using [digest](https://cran.r-project.org/web/packages/digest)) and
mechanisms for separate distribution of data and analyzing code.

## How to get `datapackageR` set up?
`datapackageR` is targeted to eventually be released on [CRAN](https://cran.r-project.org),
but until that happens and for the latest features and fixes, the following
installation procedure should be followed:

    install.packages('devtools')
    devtools::install_bitbucket('graumannlabtools/datapackageR')

## Example usage
### Comprehensive presentation of functionality
1. Load the required tools

        library(datapackageR)
        library(magrittr)
        library(readxl)
        requireNamespace('devtools')
	
2. Define a temporary working directory & packaging root

        pkg_root <- '~' %>%
          file.path('packagetest')
	  
3. Generate a (dummy) data file

        data.frame(
            x   = 1,
            y   = 1:10,
            fac = sample(LETTERS[1:3], 10, replace = TRUE)) %>%
          write.table(
            file      = file.path(dirname(pkg_root), 'data_dummy.tsv'),
            sep       = '\t',
            col.names = TRUE,
            row.names = FALSE)

4. Create the packaging skeleton, at the same time inserting the dummy data file from above:

        data_catalogue <- init(
          objects_to_include = file.path(dirname(pkg_root), 'data_dummy.tsv'),
          root               = pkg_root,
          parsing_function   = 'read.csv',
          parsing_options    = list(sep = '\t', stringsAsFactors = FALSE))

    - Investigate the resulting `data_catalogue`
   
            str(data_catalogue)

    - (Crudely) investigate the result in the file system
   
            list.files(pkg_root, recursive = TRUE)

5. Addition of other object classes:

    - Add a remote file that needs parsing (from Billing et al. (2016).
      *Comprehensive transcriptomic and proteomic characterization of human
      mesenchymal stem cells reveals source specific cellular markers.* Sci Rep
      *6*, 21507. Licensed under the [Creative Commons Attribution 4.0 International License](https://creativecommons.org/licenses/by/4.0/). **Note that the file is
      excluded from built packages (via `.rBuildignore`) and wouldn't be tracked
      by [`git`](https://git-scm.com/) (using `.gitignore`).**
   
            data_catalogue <- include_data(
              object_to_include    = 'http://www.nature.com/article-assets/npg/srep/2016/160209/srep21507/extref/srep21507-s4.xls',
              root                 = pkg_root,
              parsing_function     = 'read_excel',
              parsing_options      = list(skip = 1),
              package_dependencies = 'readxl',
              distributable        = FALSE)

    - Add a local object

            local_data_object <-list(A = LETTERS, B = letters)
            data_catalogue <- include_data(
              object_to_include = 'local_data_object',
              root              = pkg_root)

    - Add a remote *.Rda

            data_catalogue <- include_data(
              object_to_include = 'https://bitbucket.org/graumannlabtools/datapackager/downloads/remote_rda.Rda',
              root              = pkg_root)

    - Add a remote *.Rds

            data_catalogue <- include_data(
              object_to_include = 'https://bitbucket.org/graumannlabtools/datapackager/downloads/remote_rds.Rds',
              root              = pkg_root)
		  
6. Investigate the resulting `data_catalogue` & file system structure
   
        str(data_catalogue)
        list.files(pkg_root, recursive = TRUE)

7. Remove one of the tracked data sets

        data_catalogue <- remove_data(
          object_to_remove = 'data_dummy.tsv',
          root             = pkg_root)
		  
    - Investigate the resulting `data_catalogue`
   
            str(data_catalogue)
		
    - (Crudely) investigate the result in the file system
	
            list.files(pkg_root, recursive = TRUE)

10. (Remotely) install the result & use the internal functionality to test data integrity
        
        devtools::install(pkg_root)
        devtools::test(pkg_root)

11. Clean up the example package & -root

        pkg_root %>%
          basename() %>%
          remove.packages()
        unlink(pkg_root, recursive = TRUE)
        
	  
### Exemplary use case "seperately distributed data"
1. Create an empty packaging skeleton:

        pkg_root <- tempdir() %>%
          file.path('packagetest')
        data_catalogue <- init(
          root = pkg_root)

2. Attempt to add (access restricted) remote data

        data_catalogue <- include_data(
          object_to_include = 'https://bitbucket.org/graumannlabtools/datapackager-restricted-access/downloads/remote_rda.rda',
          root              = pkg_root)
        # --> Can't access URL: Client error: (401) Unauthorized

3. Authenticatedly add remote data

        data_catalogue <- include_data(
          object_to_include = 'https://bitbucket.org/graumannlabtools/datapackager-restricted-access/downloads/remote_rda.rda',
          root              = pkg_root,
          user              = 'datapackageR_user',
          password          = 'datapackageR_user',
          distributable     = FALSE)

        data_catalogue <- include_data(
          object_to_include = 'https://bitbucket.org/graumannlabtools/datapackager-restricted-access/downloads/remote_rds.Rds',
          root              = pkg_root,
          user              = 'datapackageR_user',
          password          = 'datapackageR_user',
          distributable     = FALSE)

    - Investigate the resulting `data_catalogue`
   
            str(data_catalogue)

    - (Crudely) investigate the result in the file system
   
            list.files(pkg_root, recursive = TRUE)

4. Simulate a package infrastructure shared **without** the remote &
   authentication-protected data
   
        list.files(
            pkg_root,
            recursive  = TRUE,
            pattern    = '^data[[:punct:]]remote',
            full.names = TRUE) %>%
          unlink()

    - (Crudely) investigate the result in the file system
   
            list.files(pkg_root, recursive = TRUE)

5. Use `datapackageR` for attempted retreival of the missing data

        retrieve_missing_remote_data(pkg_root)
        # --> Can't access URL: Client error: (401) Unauthorized

6. Repeat and properly authenticate (& implicitly check downloads against the
   stored hashes)
   
        retrieve_missing_remote_data(
          pkg_root,
          user     = 'datapackageR_user',
          password = 'datapackageR_user')
    
    - (Crudely) investigate the result in the file system
   
            list.files(pkg_root, recursive = TRUE)

7. Clean up the example package & -root

        pkg_root %>%
          basename() %>%
          remove.packages()
        unlink(pkg_root, recursive = TRUE)
