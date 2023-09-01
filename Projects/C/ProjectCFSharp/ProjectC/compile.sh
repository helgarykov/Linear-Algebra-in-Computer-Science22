#!/bin/bash
echo "cleaning..."
rm -fr RunTest.exe
echo "Building tests... "
fsharpc --nologo -o RunTest.exe ../Core/Vector.fs ../Core/Matrix.fs ../Core/VectorFactory.fs ../Core/MatrixFactory.fs AdvancedExtensions.fs TestProjectC.fs RunTest.fsx
echo "done"
