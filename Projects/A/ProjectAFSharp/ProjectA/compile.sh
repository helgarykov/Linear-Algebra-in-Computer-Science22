#!/bin/bash
echo "Cleaning."
rm -f RunTest.exe
echo "Building tests... "
fsharpc --nologo -o RunTest.exe ../Core/Vector.fs ../Core/Matrix.fs ../Core/VectorFactory.fs ../Core/MatrixFactory.fs BasicExtensions.fs TestProjectA.fs RunTest.fsx
echo "done"
