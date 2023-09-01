The language settings should be very similar to PoP:
This should run on Windows, Linux and macOS if Mono has been installed.
Mono comes with its own F# compiler.

For Windows: After installing Mono, you need to add "C:\Program Files\Mono\bin"
to the SYSTEM PATH environment variable.

* To compile and execute immediately, navigate to ProjectA folder and
  C:\FolderPath> compileAndRun[.bat] (Windows)
  $ ./compileAndRun.sh  (Linux/MacOs)

* To just compile, navigate to ProjectA folder and
  C:\FolderPath> compile[.bat] (Windows)
  $ ./compile.sh  (Linux/MacOs)

[.bat] means that this file extension is normally omitted on Windows.


* This code was written by François Lauze (francois@diku.dk) and Rune Balder Iversen (bri@di.ku.dk)
* For questions and comments, write to francois@di.ku.dk
