#Calculating beta-diversity
install.packages("betapart")
  #issues with this
Sys.getenv("GMP_LIBS")
Sys.setenv("GMP_LIBS" = "/opt/homebrew/Cellar/gmp/6.2.1_1/lib")

Sys.getenv("GMP_INCLUDE")
Sys.setenv("GMP_INCLUDE" = "/opt/homebrew/Cellar/gmp/6.2.1_1/include")

Sys.getenv("GCC_LIBS")
Sys.setenv("GCC_LIBS" = "/opt/homebrew/Cellar/gcc/12.2.0/lib")

Sys.getenv("GCC_INCLUDE")
Sys.setenv("GCC_INCLUDE" = "/opt/homebrew/Cellar/gcc/12.2.0/include")


install.packages("gmp", type = "source",
                 configure.args = c("--with-gmp-include=/opt/homebrew/Cellar/gmp/6.2.1_1/include"
                                    , "--with-gmp-libs=/opt/homebrew/Cellar/gmp/6.2.1_1/lib" 
                                    #, "--with-gcc-include=GCC_INCLUDE", "--with-gcc-libs=GCC_LIBS"
                                    ))

install.packages("vegan")
library(betapart)
library(vegan)



