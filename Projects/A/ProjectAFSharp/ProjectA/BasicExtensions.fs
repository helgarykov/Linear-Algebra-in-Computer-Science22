module ProjectA

open System
open LinAlgDat.Core

type BasicOps = class
  /// <summary>
  /// This function creates an augmented Matrix given a Matrix A, and a
  /// right-hand side Vector v.
  /// </summary>
  ///
  /// <remarks>
  /// See page 12 in "Linear Algebra for Engineers and Scientists"
  /// by K. Hardy.
  /// </remarks>
  ///
  /// <param name="A">An M-by-N Matrix.</param>
  /// <param name="v">An M-size Vector.</param>
  ///
  /// <returns>An M-by-(N+1) augmented Matrix [A | v].</returns>
  static member AugmentRight (A : Matrix) (v : Vector) : Matrix =
    let m_rows = A.M_Rows
    let n_cols = A.N_Cols

    let retval = Array2D.zeroCreate m_rows (n_cols + 1) //takes 2 parameters: length 1(length of the first dimention of the array); length 2(length of the second dimention of the array)

    for i in 0..m_rows-1 do
        for j in 0..n_cols-1 do
            retval.[i, j] <- A.[i, j]
        retval.[i, n_cols] <- v.[i]
    Matrix retval

  /// <summary>
  /// This function computes the Matrix-Vector product of a Matrix A,
  /// and a column Vector v.
  /// </summary>
  ///
  /// <remarks>
  /// See page 68 in "Linear Algebra for Engineers and Scientists"
  /// by K. Hardy.
  /// </remarks>
  ///
  /// <param name="A">An M-by-N Matrix.</param>
  /// <param name="v">An N-size Vector.</param>
  ///
  /// <returns>An M-size Vector b such that b = A * v.</returns>
  static member MatVecProduct (A : Matrix) (v : Vector) : Vector =
    if (A.N_Cols <> v.Size) then failwith "Number of columns in matrix is different from the size of v."
    let mutable retval = Vector(A.M_Rows)
    for i in 0..(A.M_Rows-1) do
      for j in 0..(A.N_Cols-1) do
        retval.[i] <- retval.[i] + (A.[i,j] * v.[j]) 
    //printfn "%A\n\n\n %A\n\n\n\n\n %A\n" (A.ToArray()) (v.ToArray()) (retval.ToArray())
    retval

  /// <summary>
  /// This function computes the Matrix product of two given matrices
  /// A and B.
  /// </summary>
  ///
  /// <remarks>
  /// See page 58 in "Linear Algebra for Engineers and Scientists"
  /// by K. Hardy.
  /// </remarls>
  ///
  /// <param name="A">An M-by-N Matrix.</param>
  /// <param name="B">An N-by-P Matrix.</param>
  ///
  /// <returns>The M-by-P Matrix A * B.</returns>
  static member MatrixProduct (A : Matrix) (B : Matrix) : Matrix =
    if (B.M_Rows <> A.N_Cols) then failwith "Number of columns in A is different from the number of rows in B or vice versa."    
    let mutable retval =  Matrix(A.M_Rows, 0)
    for j in 0..(B.N_Cols-1) do
      let jBVector = B.Column(j)
      let productVector = BasicOps.MatVecProduct A jBVector
      retval <- BasicOps.AugmentRight retval productVector
    printfn "%A\n\n\n %A\n\n\n %A\n\n\n\n\n\n" (A.ToArray()) (B.ToArray()) (retval.ToArray())    
    retval
    

  /// <summary>
  /// This function computes the transpose of a given Matrix.
  /// </summary>
  ///
  /// <remarks>
  /// See page 69 in "Linear Algebra for Engineers and Scientists"
  /// by K. Hardy.
  /// </remarks>
  ///
  /// <param name="A">An M-by-N Matrix.</param>
  ///
  /// <returns>The N-by-M Matrix B such that B = A^T.</returns>
  static member Transpose (A : Matrix) : Matrix =
    let mutable B =  Matrix(A.N_Cols, A.M_Rows)
    for i in 0..(A.M_Rows-1) do
      for j in 0..(A.N_Cols-1) do
        B.[j,i] <- A.[i,j]
    B
  /// <summary>
  /// This function computes the Euclidean Vector norm of a given
  /// Vector.
  /// </summary>
  ///
  /// <remarks>
  /// See page 197 in "Linear Algebra for Engineers and Scientists"
  /// by K. Hardy.
  /// </remarks>
  ///
  /// <param name="v">An N-dimensional Vector.</param>
  ///
  /// <returns>The Euclidean norm of the Vector.</returns>
  static member VectorNorm (v : Vector) : float =
    let mutable sum = 0.0    
    for i in 0..(v.Size-1) do
      sum <- sum + ((v.Item i) ** 2.0)
    sqrt sum

end
