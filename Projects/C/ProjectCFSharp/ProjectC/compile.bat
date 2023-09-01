@ECHO OFF
ECHO "Cleaning..."
call del RunTest.exe
2>nul
ECHO Building tests... 
call fsharpc --nologo -o RunTest.exe ../Core/Vector.fs ../Core/Matrix.fs ../Core/VectorFactory.fs ../Core/MatrixFactory.fs AdvancedExtensions.fs TestProjectC.fs RunTest.fsx
ECHO done


