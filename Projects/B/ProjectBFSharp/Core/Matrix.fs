namespace LinAlgDat.Core

// 04/05/2020 Francois: 
// - Added a copy constructor which makes a
//   deep copy of the matrix
// - Modified the constructor from a 2D aray
//   to make a copy of the array, not just 
//   a shared reference
// 


/// <summary>
/// An M-by-N matrix.
/// </summary>
///
/// <remarks>
/// This implement is "row-major". This means that it is likely to be
/// faster to deal in rows rather than columns. For instance, if you must
/// iterate over all the elements, iterate over rows in the outer loop,
/// and columns in the inner loop.
/// </remarks>
type Matrix = class
  val private _xs : float[,]

  /// <summary>
  /// Initializes an m-by-n Matrix with all 0's.
  /// </summary>
  new (mRows : int, nCols : int) =
    { _xs = Array2D.zeroCreate mRows nCols }

  /// <summary>
  /// Initialises a Matrix from a Array2D
  /// </summary>
  /// <remarks>
  /// the new object has its own instance of _xs
  /// </remarks> 
  new (xs : float[,]) =
    { _xs = Array2D.copy xs }
  

  ///<summary>
  /// A copy constructor
  ///</summary 
  /// <remarks>
  /// Deep copy, no instances shared.
  /// </remarks>
  new (A : Matrix) = 
    {_xs = Array2D.copy A._xs}


  /// <summary>
  /// Get the matrix as a 2D array.
  /// </summary>
  /// 
  /// <returns>
  /// A 2D array representation of the Matrix.
  /// </returns>
  member this.ToArray() =
    this._xs

  member this.M_Rows =
    this._xs.GetLength(0)

  member this.N_Cols =
    this._xs.GetLength(1)

  member this.Size =
    (this.M_Rows, this.N_Cols)

  member this.Item
    with get (i : int, j : int) = this._xs.[i, j]
    and set (i : int, j : int) (value : float) = this._xs.[i, j] <- value

  /// <summary>
  /// Get a particular row vector.
  /// </summary>
  ///
  /// <param name="i">
  /// The index of the row to get.
  /// </param>
  ///
  /// <returns>
  /// A vector containing the data of the given row
  /// </returns>
  member this.Row (i : int) =
    let retval = new Vector(this.N_Cols)
    for j in 0..this.N_Cols-1 do
      retval.[j] <- this._xs.[i, j]
    retval

  /// <summary>
  /// Get a particular column vector.
  /// </summary>
  ///
  /// <param name="j">
  /// The index of the column to get.
  /// </param>
  ///
  /// <returns>
  /// A vector containing the data of the given column
  /// </returns>
  ///
  /// <remarks>
  /// The implementation is row-major, so this is expected to be a slow
  /// endeavour.
  /// </remarks>
  member this.Column (j : int) =
    let retval = new Vector(this.M_Rows)
    for i in 0..this.M_Rows-1 do
      retval.[i] <- this._xs.[i, j]
    retval

end
