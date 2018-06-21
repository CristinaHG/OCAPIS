.onLoad <- function(libname, pkgname) {
 # op <- options()
 # op.devtools <- list(
 #   devtools.path = "~/R-dev",
 #   devtools.install.args = "",
 #   devtools.name = "Your name goes here",
 #   devtools.desc.author = "First Last <first.last@example.com> [aut, cre]",
 #   devtools.desc.license = "What license is it under?",
 #   devtools.desc.suggests = NULL,
 #   devtools.desc = list()
 # )
 # toset <- !(names(op.devtools) %in% names(op))
 # if(any(toset)) options(op.devtools[toset])

 # invisible()
  packageStartupMessage("This is OCAPIS V.0.1.0")
  .rscalaPackage(pkgname,heap.maximum="512M")
}

.onUnload <- function(libpath) {
  .rscalaPackageUnload()
}
