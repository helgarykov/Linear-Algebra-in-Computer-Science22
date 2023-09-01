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
       
      let mutable retval = new Matrix (A.M_Rows-1, A.N_Cols-1) //returværdien: en matrice der er en søjle og en række mindre end den oprindelige
      let mutable row_counter = 0  // en counter, retval-matrices rækker (en mindre end A)
      let mutable column_counter = 0 // en counter som retval-matrices søjler (en mindre end A)

      for rows in 0..A.M_Rows-1 do  // looper igennem A-matricen rækker
        column_counter <- 0

        for columns in 0..A.N_Cols-1 do // looper igennem A-matricen søjler; A.N_Cols-1 her er ikke det samme som på linje 22. Her er det inde i et forloop, hvor der er 0-indexering i 2D-array.
          if (j <> columns && i <> rows) then
            retval.[row_counter, column_counter] <- A.[rows, columns] 
            column_counter <- column_counter + 1
            //printfn "%A" column_counter

        if (i <> rows) then 
          row_counter <- row_counter + 1 // we update every time we are on the row which is not to be deleted
       
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
        let rec det (B : Matrix): float =   // B-matrix her er vores A-matrix
            let mutable result = 0.0   // returværdien
            if B.N_Cols = 1 then // hvis matricen er 1x1, dvs. længden er 1, så returnerer vi den ene værdi B.[0,0]
                B.[0,0]
            elif B.M_Rows = 2 then // hvis matrix er 2x2, 
                B.[0,0] * B.[1,1] - B.[1,0] * B.[0,1]
            else                    // kommer ind i else hvis matrix er større end 2x2
                for i in 0..(B.N_Cols-1) do
                    result <- result + ((-1.0)**(float i)) * B.[0,i] * det (AdvancedOps.SquareSubMatrix (B) (0) (i)) // vi udregner fortegnet her (((-1.0)**(float i))), i-column, 0-række, dvs. for elementet B.[0,i].
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
        
        for i in 0..A.M_Rows-1 do  // looper igenne rækken og ikke søjlerne i A, fordi vektor v er en søjle
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
        
        let getVectorFromMatrix (B : Matrix) (col : int): Vector =  //trækker en vektor (given søjle:col) ud af en given matrix 
          let mutable v = new Vector(B.M_Rows) // en ny vektor med 3 rækker
          
          for i in 0..B.M_Rows-1 do  // looper igenne rækken og ikke søjlerne i A, fordi vektor v er en søjle (Rows-1 fordi den er 0-indexeret
            v.[i] <- B.[i,col]
          v

        // Matrix Q af ortonormale vektorer, som starter med at være tom
        let mutable Q = new Matrix(A.M_Rows, A.N_Cols) // en kopi af størrelsen hvor alle værdier er 0
        let mutable U = new Matrix(A.M_Rows, A.N_Cols) // arbejdsmatrix 
        
        for j in 0..U.N_Cols-1 do
          U <- new Matrix(AdvancedOps.SetColumn U (getVectorFromMatrix A j) j) //med rækker fra U og søjler fra A hvor værdierne er 0
          for i in 0..(j) do    // sætter grænsen til j så for-look kan kun køre det antal gang som det forrige for-loop kører 
            let uVector = getVectorFromMatrix U j // tager søjler i matrix U
            let aVector = getVectorFromMatrix A j // tager søjler i matrix A
            let qVector = getVectorFromMatrix Q i //tager rækker i matrix Q

            U <- new Matrix(AdvancedOps.SetColumn U (uVector - ((aVector * qVector) * qVector)) j) //U består nu af ortogonale vektorer, j-columns i U-matrix vi tager
          
          Q <- new Matrix(AdvancedOps.SetColumn Q (1.0 / AdvancedOps.VectorNorm (getVectorFromMatrix U j) * getVectorFromMatrix U j) j)

       // Matrix R med kofaktorer
        let mutable R = new Matrix(A.N_Cols, A.N_Cols) // new symmetrisk Square matrix A
       
        for i in 0..R.N_Cols-1 do 
         for j in 0..R.N_Cols-1 do
           R.[i,j] <- getVectorFromMatrix A j * getVectorFromMatrix Q i  // tager vektor j fra den oprindelige matrix A og ganger den med den første vector i Q
       

        (Q,R)
          

    
end