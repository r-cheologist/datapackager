#### -- Packrat Autoloader (version 0.4.8-1) -- ####
#source("packrat/init.R")
#### -- End Packrat Autoloader -- ####
# dev_lib <- "../devtools_local_library/"
# devtools::dev_mode(on = TRUE, path = dev_lib)
# .libPaths(dev_lib)
if (file.exists('switchr_manifest.rds'))
{
  switchr_manifest <- readRDS('switchr_manifest.rds')
  switchr::switchTo(basename(getwd()), seed = switchr_manifest)
} else {
  switchr::switchTo(basename(getwd()))
}

