namespace LinAlgDat.Core

type MatrixFactory = class
  /// <summary>
  /// Create an n-by-n identity matrix.
  /// </summary>
  /// 
  /// <param name="n">
  /// The dimensions of the identity matrix
  /// </param>
  /// 
  /// <returns>
  /// An identity matrix of dimension NxN
  /// </returns>
  /// 
  /// <remarks>
  /// See page 64 in "Linear Algebra for Engineers and
  /// Scientists" by K. Hardy.
  /// </remarks>
  static member Identity (n : int) : Matrix =
    let retval = Matrix(n, n)
    for i in 0..n-1 do
        retval.[i,i] <- 1.0
    retval

  /// <summary>
  /// Create an n-by-n Hilbert matrix.
  /// </summary>
  /// 
  /// <param name="n">
  /// The dimensions of the Hilbert matrix
  /// </param>
  /// 
  /// <returns>
  /// A Hilbert matrix of dimension NxN
  /// </returns>
  /// 
  /// <remarks>
  /// See https://en.wikipedia.org/wiki/Hilbert_matrix.
  /// </remarks>
  static member Hilbert (n : int) : Matrix =
    let retval = Matrix(n, n)
    for i in 0..n-1 do
      for j in 0..n-1 do
        retval.[i,j] <- 1.0 / ((float i) + (float j) + 1.0)
    retval 
end
