module ProjectB

open System
open LinAlgDat.Core

type GaussOps = class

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

        let retval = Array2D.zeroCreate m_rows (n_cols + 1)

        for i in 0..m_rows-1 do
            for j in 0..n_cols-1 do
                retval.[i,j] <- A.[i,j]
            retval.[i,n_cols] <- v.[i]
        Matrix retval

    /// <summary>
    /// This function computes the elementary row replacement operation on
    /// the given matrix.
    /// </summary>
    ///
    /// <remarks>
    /// Note that we add the row (as in the lectures) instead of subtracting
    /// the row (as in the textbook).
    /// </remarks>
    ///
    /// <param name="A">
    /// An M-by-N matrix to perform the elementary row operation on.
    /// </param>
    /// <param name="i">
    /// The index of the row to replace.
    /// </param>
    /// <param name="m">
    /// The multiple of row j to add to row i (scalar).
    /// </param>
    /// <param name="j">
    /// The index of the row whose mutiple is added to row i.
    /// </param>
    ///
    /// <returns>
    /// The resulting M-by-N matrix after having performed the elementary
    /// row operation.
    /// </returns>
    static member ElementaryRowReplacement (A : Matrix) (i : int) (m : float) (j : int) : Matrix =
        if((A.M_Rows < i) || (i < 0) || (A.M_Rows < j) || (j < 0)) then failwith "OutofBoundsExeption: ElementaryRowReplacement"
        let mutable retval = Matrix A
        for columns in 0..(retval.N_Cols-1) do
          //retval.[i, columns] <- retval.[i, columns] + (retval.[j, columns] * m))         
          retval.Item(i, columns) <- retval.Item(i,columns) + (retval.Item(j, columns) * m)
        retval
        
    /// <summary>
    /// This function computes the elementary row interchange operation on
    /// the given matrix.
    /// </summary>
    ///
    /// <param name="A">
    /// An M-by-N matrix to perform the elementary row operation on.
    /// </param>
    /// <param name="i">
    /// The index of the first row of the rows to interchange.
    /// </param>
    /// <param name="j">
    /// The index of the second row of the rows to interchange.
    /// </param>
    ///
    /// <returns>
    /// The resulting M-by-N matrix after having performed the elementary
    /// row operation.
    /// </returns>
    static member ElementaryRowInterchange (A : Matrix) (i : int) (j : int) : Matrix =
        if((A.M_Rows < i) || (i < 0) || (A.M_Rows < j) || (j < 0)) then failwith "OutofBoundsExeption: ElementaryRowInterchange"
        let mutable retval = A
        let rowI = retval.Row i 
        for columns in 0..(retval.N_Cols-1) do
          retval.Item(i,columns) <- retval.Item(j,columns)
          retval.Item(j,columns) <- rowI.Item(columns)
        retval 
            

    /// <summary>
    /// This function computes the elementary row scaling operation on the
    /// given matrix.
    /// </summary>
    ///
    /// <param name="A">
    /// An M-by-N matrix to perform the elementary row operation on.
    /// </param>
    /// <param name="i">The index of the row to scale.</param>
    /// <param name="c">The value to scale the row by.</param>
    ///
    /// <returns>
    /// The resulting M-by-N matrix after having performed the elementary
    /// row operation.
    /// </returns>
    static member ElementaryRowScaling (A : Matrix) (i : int) (c : float) : Matrix =
        if((A.M_Rows < i) || (i < 0)) then failwith "OutofBoundsExeption: ElementaryRowScaling"
        let mutable retval = A
        for columns in 0..(retval.N_Cols-1) do
          retval.Item (i, columns) <- retval.Item (i, columns) * c
        retval

    /// <summary>
    /// This function executes the forward reduction algorithm provided in
    /// the assignment text to achieve row Echelon form of a given
    /// augmented matrix.
    /// </summary>
    ///
    /// <param name="A">
    /// An M-by-N matrix, augmented (or not).
    /// </param>
    ///
    /// <returns>
    /// An M-by-N matrix that is the row Echelon form.
    /// </returns>
    static member ForwardReduction (M : Matrix) : Matrix =
      
      // One does simply not compare a float number with 0.0
      // A not-so-scientific way, but quite sufficient to this course,
      // is to have a threshold value, which is defined as below.
      let tolerance = 0.00000001

      let mutable retval = M // a copy of the original matrix M 
      let mutable pivotCounter = 0

      for columns in 0..(retval.N_Cols-1) do
        let mutable currentRow = 0
        let mutable nonZeroFound = false
        while( currentRow < retval.M_Rows) do 
          match nonZeroFound with 
          | false -> match retval.Item (currentRow, columns) with
                     | item when item < 0.00 + tolerance
                                 && item > 0.00 - tolerance 
                                 || pivotCounter > currentRow -> currentRow <- currentRow + 1
                     | item when currentRow > pivotCounter -> GaussOps.ElementaryRowInterchange retval currentRow pivotCounter
                                                              currentRow <- pivotCounter
                     | _ -> nonZeroFound <- true
          | true -> match retval.Item (currentRow, columns) with
                    | item when currentRow = pivotCounter -> 
                                                             currentRow <- currentRow + 1
                    | item when item < 0.0 + tolerance
                                && item > 0.0 - tolerance -> currentRow <- currentRow + 1
                    | item -> GaussOps.ElementaryRowReplacement retval currentRow ((item/retval.Item(pivotCounter, columns)) * -1.0) pivotCounter
                              currentRow <- currentRow + 1
        if nonZeroFound then pivotCounter <- pivotCounter + 1 
      retval

     
    /// <summary>
    /// This function executes the backward reduction algorithm provided in
    /// the assignment text given an augmented matrix in row Echelon form.
    /// </summary>
    ///
    /// <param name="A">
    /// An M-by-N augmented matrix in row Echelon form.
    /// </param>
    ///
    /// <returns>
    /// The resulting M-by-N matrix after executing the algorithm.
    /// </returns>
    static member BackwardReduction (A : Matrix) : Matrix =
      let tolerance = 0.00000001
      let mutable retval = A
      let mutable pivotRow = retval.M_Rows
      
      for columns in (retval.N_Cols-1) .. -1 .. 0 do
        let mutable currentRow = retval.M_Rows - 1
        let mutable nonZeroFound = false
        while (currentRow >= 0) do
          match nonZeroFound with
          | false -> match retval.Item(currentRow, columns) with
                     | item when item < 0.0 + tolerance
                                 && item > 0.0 - tolerance -> currentRow <- currentRow - 1
                     | item -> nonZeroFound <-true
                               pivotRow <- currentRow
                               GaussOps.ElementaryRowScaling retval pivotRow (1.0 / item)
                               currentRow <- currentRow - 1
                     | _ -> currentRow <- currentRow - 1
          | true -> match retval.Item(currentRow,columns) with
                          | item when item < 0.0 + tolerance 
                                      && item > 0.0 - tolerance -> currentRow <- currentRow - 1
                          | item -> GaussOps.ElementaryRowReplacement retval currentRow (item/retval.Item(pivotRow,columns) * -1.0) pivotRow
                                    currentRow <- currentRow - 1
      retval
        
    /// <summary>
    /// This function performs Gauss elimination of a linear system
    /// given in matrix form by a coefficient matrix and a right hand side
    /// vector. It is assumed that the corresponding linear system is
    /// consistent and has exactly one solution.
    /// </summary>
    ///
    /// <remarks>
    /// Hint: Combine ForwardReduction and BackwardReduction.
    /// </remarks>
    ///
    /// <param name="A">An M-by-N Matrix.</param>
    /// <param name="b">An M-size Vector.</param>
    ///
    /// <returns>The N-sized vector x such that A * x = b.</returns>
    static member GaussElimination (A : Matrix) (b : Vector) : Vector =
        let mutable matrix = GaussOps.AugmentRight A b
        matrix <- GaussOps.ForwardReduction matrix
        matrix <-GaussOps.BackwardReduction matrix

        matrix.Column (matrix.N_Cols-1)
end
