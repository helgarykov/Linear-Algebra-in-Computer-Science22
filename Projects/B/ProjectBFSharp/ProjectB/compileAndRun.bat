@ECHO OFF
ECHO Cleaning...
call rm RunTest.exe 2> nul
ECHO Building tests... 
call fsharpc --nologo -o RunTest.exe ../Core/Vector.fs ../Core/Matrix.fs ../Core/VectorFactory.fs ../Core/MatrixFactory.fs GaussExtensions.fs TestProjectB.fs RunTest.fsx
ECHO done
ECHO.
ECHO.
ECHO Running tests...
ECHO.
call mono RunTest.exe
