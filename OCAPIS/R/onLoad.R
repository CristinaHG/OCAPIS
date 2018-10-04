.onLoad <- function(libname, pkgname) {
#  callback <-'
#import breeze.linalg._'
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
  #packageStartupMessage("This is OCAPIS V.0.1.0")
  assign.callback <- function(s) {
    s + '
      import cristinahg.ocapis.svmop._
      import cristinahg.ocapis.kdlor._
      import cristinahg.ocapis.wknn._
      import cristinahg.ocapis.MonoFSelector._
      import cristinahg.ocapis.TSS._
    '
  }
  scalaPackage(pkgname,assign.callback=assign.callback)
}
.onAttach <-function(libname,pkgname) {
  packageStartupMessage("This is OCAPIS V.1.0.0")
}
.onUnload <- function(libpath) {
  scalaPackageUnload()
}
