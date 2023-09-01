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
    /// The multiple of row j to add to row i.
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
        
        let mutable B =  Matrix(A)

        for c in 0..(A.N_Cols-1) do
            B.[i,c] <- A.[i,c] + (A.[j,c] * m) 
        B
        
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
         
        let mutable B = Matrix(A)

        for c in 0..(A.N_Cols-1) do
            B.[j,c] <- A.[i,c]
            B.[i,c] <- A.[j,c]
        B

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
        let mutable B = Matrix(A)

        for j in 0..(A.N_Cols-1) do
            B.[i,j] <- A.[i,j] * c
        B

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

        let tolerance = 0.00000001
        let mutable B = Matrix(M)
        let mutable subMatrix = Matrix(M)
        let mutable pivotColumn = 0
        let mutable pivotRow = 0
        
        let forwardReduction (A : Matrix) : Matrix =
            let mutable Mat = Matrix(A)
            let mutable foundPivot = false

            for j in 0..(Mat.N_Cols-1) do
                for i in 0..(Mat.M_Rows-1) do
                    if (abs Mat.[i,j]-tolerance > (abs Mat.[i,j] * tolerance) && foundPivot = false) then //-----------------------------------
                        pivotColumn <- j
                        pivotRow <- i
                        foundPivot <- true
                        Mat <- GaussOps.ElementaryRowInterchange Mat 0 pivotRow

            for i in 1..(Mat.M_Rows-1) do
                if(abs Mat.[i,pivotColumn]-tolerance > (abs Mat.[i,pivotColumn]* tolerance)) then //-----------------------------------
                    Mat <- GaussOps.ElementaryRowReplacement (Mat) (i) (-(Mat.[i,pivotColumn]/Mat.[0,pivotColumn])) (0)
            Mat

        for i in 0..(M.M_Rows-1) do
            let sub = Matrix(B.ToArray().[i..(B.M_Rows-1),*])
            let subArray = (sub.Row 0).ToArray()
            let allZero = Array.forall (fun elem -> abs elem-tolerance > (abs elem * tolerance)) //-----------------------------------

            if (sub.M_Rows > 0  && (allZero subArray && sub.M_Rows = 1) = false) then
                subMatrix <- Matrix(forwardReduction(sub))
                for si in 0..(subMatrix.M_Rows-1) do
                    for j in 0..(M.N_Cols-1) do
                        B.[i+si,j] <- (subMatrix.[si,j])

        Matrix(B)

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
        let mutable B = Matrix(A)
        let mutable pivotColumn = 0
        let mutable pivotRow = 0

        let mutable foundPivot = false
        for j = (B.N_Cols-1) downto 0 do
            for i = (B.M_Rows-1) downto 0 do
                if (abs B.[i,j]-tolerance > (abs B.[i,j]*tolerance) && foundPivot = false) then
                    pivotColumn <- j
                    pivotRow <- i
                    foundPivot <- true
                    B <- GaussOps.ElementaryRowScaling B i (1.0/B.[pivotRow,pivotColumn])
            for i = (pivotRow-1) downto 0 do
                if(abs B.[i,pivotColumn]-tolerance > (abs B.[i,pivotColumn]*tolerance)) then
                    B <- GaussOps.ElementaryRowReplacement (B) (i) (-(B.[i,pivotColumn]/B.[pivotRow,pivotColumn])) (pivotRow)
            foundPivot <- false
        B


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
        let mutable B = Matrix(A)
        B <- GaussOps.AugmentRight A b
        B <- GaussOps.BackwardReduction (GaussOps.ForwardReduction B)
        
        B.Column (B.N_Cols-1)

end
