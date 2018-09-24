load_libsvm<-function(){
  libdir<-file.path("/usr/lib","libsvm-weights-3.17","python")
  if(dir.exists(libdir)){
  svm<-import_from_path("svmutil",libdir)
  }else{
    libdir<-file.path("/usr/local/lib","libsvm-weights-3.17","python")
    if(dir.exists(libdir)){
      svm<-import_from_path("svmutil",libdir)
    }else {
      libdir<-file.path("C:\\Program Files (x86)","libsvm-weights-3.17","python")
      if(dir.exists(libdir)){
        svm<-import_from_path("svmutil",libdir)
      }
      else {
        libdir<-file.path("C:\\Program Files","libsvm-weights-3.17","python")
        if(dir.exists(libdir)){
          svm<-import_from_path("svmutil",libdir)
        }else{
          stop("No libsvm-weights-3.17 was found in library path")
        }
      }
    }
  }
  svm
  #mysvm<-import_from_path("svmutil",system.file("python","python",package = "OCAPIS"))

}
