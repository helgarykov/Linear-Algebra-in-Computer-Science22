The student's version of Project C

The language settings should be very similar to PoP: 
this should run on Windows, Linux and Mac OS if Mono has been installed.
Mono comes with its own F# compiler. 

For Windows install of Mono, you need to add "C:\program Files\mono\bin"
to the PATH environment variable. For most Linux flavors, the installation 
will be systemwide and the path set by the installer. My guess is that this is 
the same for Mac OS, but I have no Mac.... Report ASAP in case of problems!


* To compile and execute immediately, go to ProjectC folder and

  C:\FolderPath> compileAndRun[.bat] (Windows)
  
  some_prompt_$ ./compileAndRun.sh  (Linux/MacOs)

* To just compile, go to ProjectC folder and

  C:\FolderPath> compile[.bat] (Windows)
  
  some_prompt_$ ./compile.sh  (Linux/MacOs)

    this can be useful when only coding/compiling to check potential error messages from the compiler.
  
+ François Lauze, Department of Computer Sciennce, University of Copenhagen, March 2021 - April 2022.

