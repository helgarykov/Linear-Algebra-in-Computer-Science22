// Francois: rewrote the logic of the tests, found the original
// a bit to convoluted.

module TestProjectB

open ProjectB
open LinAlgDat.Core

let Tolerance = 1e-3

let compareVectorDimensions (v : Vector) (w : Vector) =
    v.Size = w.Size

// compute L1-distance between arguments. If less than
// Tolerance, they are considered equal.
// v and w must have same size
let CompareVectors (v : Vector) (w : Vector) =
    if compareVectorDimensions v w then
        let vec = Array.zip (v.ToArray()) (w.ToArray())
        let sum = Array.fold (fun acc (v', w') -> acc + abs (v' - w')) 0.0 vec
        (sum / float v.Size) < Tolerance && not (System.Double.IsNaN(sum))
    else
        failwith "ERROR: Vector dimensions must be equal"; false

let compareMatrixDimensions (A : Matrix) (B : Matrix) =
    A.M_Rows = B.M_Rows && A.N_Cols = B.N_Cols

// compute L1-distance between arguments. If less than
// Tolerance, they are considered equal.
// A and B must have same dimensions
let CompareMatrices (A : Matrix) (B : Matrix) =
    if compareMatrixDimensions A B then
        let mat = Array.zip
                    (A.ToArray() |> Seq.cast<float> |> Seq.toArray)
                    (B.ToArray() |> Seq.cast<float> |> Seq.toArray)
        let sum = Array.fold (fun acc (a', b') -> acc + abs (a' - b')) 0.0 mat
        (sum / float (A.M_Rows * A.N_Cols)) < Tolerance && not (System.Double.IsNaN(sum))
    else
        failwith "ERROR: Matrix dimensions must be equal"; false

// display a message followed by [PASSED] or [FAILED]
let OutMessage (taskName : string) (subTaskName : string) (status : bool) : string =
    let s = (sprintf "%s %s" taskName subTaskName)
    let res = if status then "[PASSED]" else "[FAILED]"
    sprintf "%-50s %s" s res



// All the tests have the same structure.
// *) check that the implemented method runs
// *) if applicable, check that the result has expected size/dimensions
// *) check that the result has the expected value(s)
// Only if all the tests are successful, the method returns true

let TestRowReplacement (A : Matrix) (i : int) (f : float) (j : int) (expected : Matrix) =
    let taskName = "ElementaryRowReplacement(Matrix, int, float, int)"
    let mutable status = true
    let mutable result = ""
    // Would otherwise overwrite A
    let C = new Matrix(A)

    let provideContext (Av : Matrix) =
        let l1 = "\n" + (OutMessage taskName "Values" false) + "\n"
        let l2 = "********** Input Matrix **********"
        let l3 = sprintf "\n%A" (A.ToArray())
        let l4 = sprintf "\n\n********** Line to be replaced %i, adding %f multiple of line % i **********\n" i f j
        let l5 = "\n\n******** Actual results ********\n"
        let l6 = sprintf "\n%A\n" (Av.ToArray())
        let l7 = "\n\n********** Expected result **********"
        let l8 = sprintf "\n\n%A\n" (expected.ToArray())
        l1 + l2 + l3 + l4 + l5 + l6 + l7 + l8


    result <- result + "\n" + sprintf "Tests for the %s function" taskName
    result <- result + "\n" + sprintf "==============================================================================="

    try
        let Av = GaussOps.ElementaryRowReplacement C i f j
        if not (compareMatrixDimensions Av expected) then
            status <- false
            result <- result + "\n" + OutMessage taskName "Dims" status
            result <- result + provideContext Av
        else
            result <- result + "\n" + OutMessage taskName "Dims" status
            if not (CompareMatrices Av expected) then
                status <- false
                result <- result + "\n" + OutMessage taskName "Values" status
                result <- result + provideContext Av
            else
                result <- result + "\n" + OutMessage taskName "Values" status
    with
    | ex -> status <- false
            result <- result + "\n" + OutMessage taskName "Run" status + "\n" + ex.Message

    if status then
        result <- result + "\n" + OutMessage taskName "All" true

    result <- result + "\n" + sprintf "\nEnd of test for the %s function." taskName
    result <- result + "\n" + sprintf "-------------------------------------------------------------------------------\n"
    taskName,status,result


let TestRowInterchange (A : Matrix) (i : int) (j : int) (expected : Matrix) =
    let taskName = "ElementaryRowInterchange(Matrix, int, int)"
    let mutable status = true
    let mutable result = ""
    // Would otherwise overwrite A
    let C = new Matrix(A)

    let provideContext (Av : Matrix) =
        let l1 = "\n" + OutMessage taskName "Values" false
        let l2 = "\n" + sprintf "\n********** Input Matrix **********\n"
        let l3 = "\n" + sprintf "%A" (A.ToArray())
        let l4 = "\n" + sprintf "\n********** Lines to be interchanged: (%i, %i) **********\n" i j
        let l5 = "\n" + sprintf "\n********** Actual result **********\n"
        let l6 = "\n" + sprintf "%A" (Av.ToArray())
        let l7 = "\n" + sprintf "\n****** Expected result ******\n"
        let l8 = "\n" + sprintf "%A\n" (expected.ToArray())
        l1 + l2 + l3 + l4 + l5 + l6 + l7 + l8

    result <- result + "\n" + sprintf "Tests for the %s function" taskName
    result <- result + "\n" + sprintf "========================================================================"

    try
        let Av = GaussOps.ElementaryRowInterchange C i j
        if not (compareMatrixDimensions Av expected) then
            status <- false
            result <- result + "\n" + OutMessage taskName "Dims" false
            result <- result + (provideContext Av)
        else
            result <- result + "\n" + OutMessage taskName "Dims" true
            if not (CompareMatrices Av expected) then
                status <- false
                result <- result + "\n" + OutMessage taskName "Values" false
                result <- result + provideContext Av
            else
                result <- result + "\n" + OutMessage taskName "Values" true
    with
    | ex -> status <- false
            result <- result + "\n" + OutMessage taskName "Run" status + "\n" + ex.Message


    if status then
        result <- result + "\n" + OutMessage taskName "All" true

    result <- result + "\n" + sprintf "\nEnd of test for the %s function." taskName
    result <- result + "\n" + sprintf "------------------------------------------------------------------------\n"
    taskName,status,result



let TestRowScaling (A : Matrix) (i : int) (f : float) (expected : Matrix) =
    let taskName = "ElementaryRowScaling(Matrix, int, float)"
    let mutable status = true
    let mutable result = ""
    // Would otherwise overwrite A
    let C = new Matrix(A)

    let provideContext (Av : Matrix) =
        let l1 = "\n" + OutMessage taskName "Values" false
        let l2 = "\n" + sprintf "\n****** Input Matrix ******\n"
        let l3 = "\n" + sprintf "%A" (A.ToArray())
        let l4 = "\n" + sprintf "\n****** Line to be scaled %i, scale factor %f ******\n" i f
        let l5 = "\n" + sprintf "\n****** Actual result ******\n"
        let l6 = "\n" + sprintf "%A" (Av.ToArray())
        let l7 = "\n" + sprintf "\n****** Expected result ******\n"
        let l8 = "\n" + sprintf "%A\n" (expected.ToArray())
        l1 + l2 + l3 + l4 + l5 + l6 + l7 + l8

    result <- result + "\n" + sprintf "Tests for the %s function" taskName
    result <- result + "\n" + sprintf "======================================================================"

    try
        let Av = GaussOps.ElementaryRowScaling C i f
        if not (compareMatrixDimensions Av expected) then
            status <- false
            result <- result + "\n" + OutMessage taskName "Dims" false
            result <- result + (provideContext Av)
        else
            result <- result + "\n" + OutMessage taskName "Dims" true
            if not (CompareMatrices Av expected) then
                status <- false
                result <- result + "\n" + OutMessage taskName "Values" false
                result <- result + provideContext Av
            else
                result <- result + "\n" + OutMessage taskName "Values" true
    with
    | ex -> status <- false
            result <- result + "\n" + OutMessage taskName "Run" status + "\n" + ex.Message

    if status then
        result <- result + "\n" + OutMessage taskName "All" true

    result <- result + "\n" + sprintf "\nEnd of test for the %s function." taskName
    result <- result + "\n" + sprintf "----------------------------------------------------------------------\n"
    taskName,status,result

let TestForwardReduction (A : Matrix) (expected : Matrix) =
    let taskName = "ForwardReduction(Matrix)"
    let mutable status = true
    let mutable result = ""
    // Would otherwise overwrite A
    let C = new Matrix(A)

    result <- result + "\n" + sprintf "Tests for the %s function" taskName
    result <- result + "\n" + sprintf "-----------------------------------------------------------"

    let provideContext (Av : Matrix) =
        let l1 = "\n" + OutMessage taskName "Values" false
        let l2 = "\n" + sprintf "\n********** Input Matrix A **********\n"
        let l3 = "\n" + sprintf "%A" (A.ToArray())
        let l4 = "\n" + sprintf "\n********** Actual result **********\n"
        let l5 = "\n" + sprintf "%A" (Av.ToArray())
        let l6 = "\n" + sprintf "\n********** Expected result **********\n"
        let l7 = "\n" + sprintf "%A\n" (expected.ToArray())
        l1 + l2 + l3 + l4 + l5 + l6 + l7
    try
        let Av = GaussOps.ForwardReduction C
        if not (compareMatrixDimensions Av expected) then
            status <- false
            result <- result + "\n" + OutMessage taskName "Dims" false
            result <- result + (provideContext Av)
        else
            result <- result + "\n" + OutMessage taskName "Dims" true
            if not (CompareMatrices Av expected) then
                status <- false
                result <- result + "\n" + OutMessage taskName "Values" false
                result <- result + provideContext Av
            else
                result <- result + "\n" + OutMessage taskName "Values" true
    with
    | ex -> status <- false
            result <- result + "\n" + OutMessage taskName "Run" status + "\n" + ex.Message

    if status then
        result <- result + "\n" + OutMessage taskName "All" true

    result <- result + "\n" + sprintf "\nEnd of test for the %s function." taskName
    result <- result + "\n" + sprintf "-----------------------------------------------------------\n"
    taskName,status,result

let TestBackwardReduction (A : Matrix) (expected : Matrix) =
    let taskName = "BackwardReduction(Matrix)"
    let mutable status = true
    let mutable result = ""
    // Would otherwise overwrite A
    let C = new Matrix(A)

    result <- result + "\n" + sprintf "Tests for the %s function" taskName
    result <- result + "\n" + sprintf "-----------------------------------------------------------"

    let provideContext (Av : Matrix) =
        let l1 = "\n" + OutMessage taskName "Values" false
        let l2 = "\n" + sprintf "\n********** Input Matrix **********\n"
        let l3 = "\n" + sprintf "%A" (A.ToArray())
        let l4 = "\n" + sprintf "\n********** Actual result **********\n"
        let l5 = "\n" + sprintf "%A" (Av.ToArray())
        let l6 = "\n" + sprintf "\n********** Expected result **********\n"
        let l7 = "\n" + sprintf "%A\n" (expected.ToArray())
        l1 + l2 + l3 + l4 + l5 + l6 + l7

    try
        let Av = GaussOps.BackwardReduction C
        if not (compareMatrixDimensions Av expected) then
            status <- false
            result <- result + "\n" + OutMessage taskName "Dims" false
            result <- result + (provideContext Av)
        else
            result <- result + "\n" + OutMessage taskName "Dims" true
            if not (CompareMatrices Av expected) then
                status <- false
                result <- result + "\n" + OutMessage taskName "Values" false
                result <- result + provideContext Av
            else
                result <- result + "\n" + OutMessage taskName "Values" true
    with
    | ex -> status <- false
            result <- result + "\n" + OutMessage taskName "Run" status + "\n" + ex.Message

    if status then
        result <- result + "\n" + OutMessage taskName "All" true

    result <- result + "\n" + sprintf "\nEnd of test for the %s function." taskName
    result <- result + "\n" + sprintf "-----------------------------------------------------------\n"
    taskName,status,result

let TestGaussElimination (A : Matrix) (v : Vector) (expected : Vector) =
    let taskName = "GaussElimination(Matrix, Vector)"
    let mutable status = true
    let mutable result = ""
    // Would otherwise overwrite A
    let C = new Matrix(A)

    result <- result + "\n" + sprintf "Tests for the %s function" taskName
    result <- result + "\n" + sprintf "-----------------------------------------------------------"

    let provideContext (Av : Vector) =
        let l1 =  "\n" + OutMessage taskName "Values" false
        let l2 =  "\n" + sprintf "\n********** Input Matrix **********\n"
        let l3 =  "\n" + sprintf "%A" (A.ToArray())
        let l4 =  "\n" + sprintf "\n********** Input Vector **********\n"
        let l5 =  "\n" + sprintf "%A" (v.ToArray())
        let l6 =  "\n" + sprintf "\n********** Actual result **********\n"
        let l7 =  "\n" + sprintf "%A" (Av.ToArray())
        let l8 = "\n" + sprintf "\n********** Expected result **********\n"
        let l9 =  "\n" + sprintf "%A\n" (expected.ToArray())
        l1 + l2 + l3 + l4 + l5 + l6 + l7 + l8 + l9


    try
        let Av = GaussOps.GaussElimination C v
        if not (compareVectorDimensions Av expected) then
            status <- false
            result <- result + "\n" + OutMessage taskName "Dims" false
            result <- result + (provideContext Av)
        else
            result <- result + "\n" + OutMessage taskName "Dims" true
            if not (CompareVectors Av expected) then
                status <- false
                result <- result + "\n" + OutMessage taskName "Values" false
                result <- result + provideContext Av
            else
                result <- result + "\n" + OutMessage taskName "Values" true
    with
    | ex -> status <- false
            result <- result + "\n" + OutMessage taskName "Run" status + "\n" + ex.Message


    if status then
        result <- result + "\n" + OutMessage taskName "All" true

    result <- result + "\n" + sprintf "\nEnd of test for the %s function." taskName
    result <- result + "\n" + sprintf "-----------------------------------------------------------\n"
    taskName,status,result
