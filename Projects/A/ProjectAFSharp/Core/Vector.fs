namespace LinAlgDat.Core

// 04/05/2020 Francois: 
// - Added a copy constructor which makes a
//   deep copy of the vector
// - Modified the constructor from a 1D aray
//   to make a copy of the array, not just 
//   a shared reference
// 


type Vector = class
  val private _xs : float[]

  /// <summary>
  /// Initializes an n-Vector with all 0's.
  /// </summary>
  new (n : int) = 
    { _xs = Array.zeroCreate n }
  /// <summary>
  /// Initializes an Vector from a float[]
  /// </summary>
  /// <remarks>
  /// Perform a copy of the array, not
  /// a shared reference.
  /// </remarks>
  new (xs : float[]) = 
    { _xs = xs }
  /// <summary>
  /// Copy constructor
  /// </summary>
  /// <remarks>
  /// Perform a copy of the array, not
  /// a shared reference.
  new (v : Vector) = 
    {_xs = Array.copy v._xs}

  /// <summary>
  /// Get the vector as an array.
  /// </summary>
  /// 
  /// <returns>
  /// An array representation of the Matrix.
  /// </returns>
  member this.ToArray() =
    this._xs
    
  member this.Size =
    this._xs.Length

  member this.Item
    with get (i : int) = this._xs.[i]
    and set (i : int) (value : float) = this._xs.[i] <- value 

  static member ( * ) (v : Vector, y : float) =
    let size = (v.Size)
    let retval = Vector(size)
    for i in 0..size-1 do
      retval.[i] <- v.[i] * y
    retval

  static member ( * ) (x : float, v : Vector) =
    let size = (v.Size)
    let retval = Vector(size)
    for i in 0..size-1 do
      retval.[i] <- v.[i] * x
    retval

  static member (+) (xs : Vector, ys : Vector) =
    let size = min (xs.Size) (ys.Size) 
    let retval = Vector(size)
    for i in 0..size-1 do
      retval.[i] <- xs.[i] + ys.[i]
    retval

  static member (-) (xs : Vector, ys : Vector) =
    let size = min (xs.Size) (ys.Size) 
    let retval = Vector(size)
    for i in 0..size-1 do
      retval.[i] <- xs.[i] - ys.[i]
    retval

  static member ( * ) (xs : Vector, ys : Vector) =
    let size = min (xs.Size) (ys.Size) 
    let mutable retval = 0.0
    for i in 0..size-1 do
      retval <- retval + (xs.[i] * ys.[i])
    retval
    
end
