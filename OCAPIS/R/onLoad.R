#' Loads scala implementations
#' Downloads and installs the jars providing the scala algorithms
#' The first time the package is loaded, it checks if the jar files
#' are available and if not, automatically download  them from \url{https://github.com/CristinaHG/OCAPIS/blob/master/OCAPIS/inst/java/scala-2.12/ocapis_source-assembly-0.1.jar?raw=true}
.onLoad <- function(libname, pkgname) {
  s <- scala(pkgname)
  ## path to the jar
  basepath <- file.path(system.file(package =pkgname))
  path<-file.path(basepath,"java","scala-2.12")

 # check if dir exists
   if (!dir.exists(path)) {
     dir.create(path, recursive = TRUE)
   }

  # check if dir is not empty
   if (length(dir(path)) == 0) {
    ocapisfile<-file.path(path,"ocapis.jar")
    dres <- tryCatch(download.file(
      url = 'https://github.com/CristinaHG/OCAPIS/raw/master/OCAPIS/inst/java/scala-2.12/ocapis_source-assembly-0.1.jar',
      destfile = ocapisfile, mode = 'wb'), error = function(e) e)


    scalaJARs(file.path(path,"ocapis.jar"),s)

  }
   scalaLazy(function(s) s + '
      import cristinahg.ocapis.svmop._
      import cristinahg.ocapis.kdlor._
      import cristinahg.ocapis.wknn._
      import cristinahg.ocapis.MonoFSelector._
      import cristinahg.ocapis.TSS._
    ')
    # scalaPackage(pkgname,assign.callback=assign.callback)
   assign("s",s,envir=parent.env(environment()))
}

.onAttach <-function(libname,pkgname) {
  path <- file.path(file.path(system.file(package =pkgname)),"java","scala-2.12")
  if (length(list.files(path)) == 0) {
    packageStartupMessage(
      'The automatic installation of Jar file containing the scala algorithms seems to have failed.\n',
      'Please check your Internet connection and you have writing permissions to ', path, '\n',
      'If still having issues, try install the full package from the GitHub repository:\n',
      'https://github.com/CristinaHG/OCAPIS/tree/master/OCAPIS\n',
      'where you can find a complete guide about HOW-TO install OCAPIS')
  }else
    packageStartupMessage("This is OCAPIS V.1.0.0")
}
.onUnload <- function(libpath) {
  close(s)
}
