module ProjectC

open System
open LinAlgDat.Core

type AdvancedOps = class

    /// <summary>
    ///     This function creates the square submatrix given a square matrix as
    ///     well as row and column indices to remove from it.
    /// </summary>
    /// <remarks>
    ///     See page 246-247 in "Linear Algebra for Engineers and Scientists"
    ///     by K. Hardy.
    /// </remarks>
    /// <param name="A">An N-by-N matrix.</param>
    /// <param name="i">The index of the row to remove.</param>
    /// <param name="j">The index of the column to remove.</param>
    /// <returns>The resulting (N - 1)-by-(N - 1) submatrix.</returns>
    static member SquareSubMatrix (A : Matrix) (i : int) (j : int) : Matrix =
       
      let mutable retval = new Matrix (A.M_Rows-1, A.N_Cols-1)
      let mutable row_counter = 0  
      let mutable column_counter = 0 

      for rows in 0..A.M_Rows-1 do  
        column_counter <- 0

        for columns in 0..A.N_Cols-1 do 
          if (j <> columns && i <> rows) then
            retval.[row_counter, column_counter] <- A.[rows, columns] 
            column_counter <- column_counter + 1

        if (i <> rows) then 
          row_counter <- row_counter + 1 
       
      retval
      

              
    /// <summary>
    ///     This function computes the determinant of a given square matrix.
    /// </summary>
    /// <remarks>
    ///     See page 247 in "Linear Algebra for Engineers and Scientists"
    ///     by K. Hardy.
    /// </remarks>
    /// <remarks>
    ///     Hint: Use SquareSubMatrix.
    /// </remarks>
    /// <param name="A">An N-by-N matrix.</param>
    /// <returns>The determinant of the matrix</returns>
    static member Determinant (A : Matrix) : float =
        let rec det (B : Matrix): float =
            let mutable result = 0.0 
            if B.N_Cols = 1 then 
                B.[0,0]
            elif B.M_Rows = 2 then  
                B.[0,0] * B.[1,1] - B.[1,0] * B.[0,1]
            else                    
                for i in 0..(B.N_Cols-1) do
                    result <- result + (-1.0**(float i)) * B.[0,i] * det (AdvancedOps.SquareSubMatrix B 0 i) 
                result
        det A


    /// <summary>
    ///     This function computes the Euclidean norm of a Vector. This has been implemented
    ///     in Project A and is provided here for convenience
    /// </summary>
    /// <param name="v">
    ///    A Vector
    /// </param>
    /// <returns>
    ///     Euclidean norm, i.e. (\sum v[i]^2)^0.5
    /// </returns>
    static member VectorNorm (v : Vector) =
        let mutable n2 = 0.0
        for i in 0..v.Size-1 do
            n2 <- n2 + v.[i] * v.[i]
        sqrt n2
    

    /// <summary>
    ///     This function copies Vector 'v' as a column of matrix 'A'
    ///     at column position j.
    /// </summary>
    /// <param name="A">
    ///     An M-by-N matrix.
    /// </param>
    /// <param name="v">
    ///     Vector objects that must be copied in A.
    /// </param>
    /// <param name="j">
    ///     column number.
    /// </param>
    /// <returns>
    ///     An M-by-N matrix after modification.
    /// </returns>
    /// <exception cref="ArgumentException"></exception>
    static member SetColumn (A : Matrix) (v : Vector) (j : int) =
        let mutable retval = new Matrix(A)
        
        for i in 0..A.M_Rows-1 do 
          retval.[i,j] <- v.[i]
   
        retval
    
    /// <summary>
    ///     This function computes the Gram-Schmidt process on a given matrix.
    /// </summary>
    /// <remarks>
    ///     See page 229 in "Linear Algebra for Engineers and Scientists"
    ///     by K. Hardy.
    /// </remarks>
    /// <param name="A">
    ///     An M-by-N matrix. All columns are implicitly assumed linear
    ///     independent.
    /// </param>
    /// <returns>
    ///     A tuple (Q,R) where Q is a M-by-N orthonormal matrix and R is an
    ///     N-by-N upper triangular matrix.
    /// </returns>
    static member GramSchmidt (A : Matrix) : Matrix * Matrix =
        
        // Helper-function to extract a column-vector from a given matrix
        let getVectorFromMatrix (B : Matrix) (col : int): Vector =  
          let mutable v = new Vector(B.M_Rows) 
          
          for i in 0..B.M_Rows-1 do  
            v.[i] <- B.[i,col]
          v

        // Matrix Q med ortonormale vektorer
        let mutable Q = new Matrix(A.M_Rows, A.N_Cols) 
        let mutable U = new Matrix(A.M_Rows, A.N_Cols)  
        
        for j in 0..U.N_Cols-1 do
          U <- new Matrix(AdvancedOps.SetColumn U (getVectorFromMatrix A j) j) 
          for i in 0..j do  
            let u_vector = getVectorFromMatrix U j 
            let a_vector = getVectorFromMatrix A j 
            let q_vector = getVectorFromMatrix Q i 

            U <- new Matrix(AdvancedOps.SetColumn U (u_vector - ((a_vector * q_vector) * q_vector)) j) 
          
          Q <- new Matrix(AdvancedOps.SetColumn Q (1.0 / AdvancedOps.VectorNorm (getVectorFromMatrix U j) * getVectorFromMatrix U j) j)

       // Matrix R med kofaktorer
        let mutable R = new Matrix(A.N_Cols, A.N_Cols) 
       
        for i in 0..R.N_Cols-1 do 
         for j in 0..R.N_Cols-1 do
           R.[i,j] <- getVectorFromMatrix A j * getVectorFromMatrix Q i  
       
        // Return matrices Q and R
        (Q,R)
    
end