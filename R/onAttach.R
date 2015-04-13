.onAttach <- function(libname, pkgname) {
  if (!requireNamespace("WGCNA", quietly = TRUE)) {
      packageStartupMessage("It is highly recommended that WGCNA be installed.")
      packageStartupMessage("To do this, enter the following commands into the console:")
      packageStartupMessage("source(\"http://bioconductor.org/biocLite.R\")")
      packageStartupMessage("biocLite(\"impute\")")
      packageStartupMessage("biocLite(\"preprocessCore\")") 
      packageStartupMessage("install.packages(\"WGCNA\")")
      packageStartupMessage(" ")
      packageStartupMessage("If this fails see http://labs.genetics.ucla.edu/horvath/CoexpressionNetwork/Rpackages/WGCNA/ \n for further information.")
    }
}


