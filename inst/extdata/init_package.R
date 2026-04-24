package <- "kwb.smartwater"

withr::with_dir("./", {
  kwb.pkgbuild::use_pkg_skeleton(package)
})

author <- list(
  name = "Hauke Sonnenberg", 
  orcid = "0000-0001-9134-2871"
)

description <- list(
  name = package, 
  title = "Functions related to the SmartWater project", 
  desc  = "This package contains functions related to the SmartWater project."
)

kwb.pkgbuild::use_pkg(
  author, 
  description, 
  version = "0.0.0.9000", 
  stage = "experimental"
)
