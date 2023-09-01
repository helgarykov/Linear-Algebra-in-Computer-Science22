(*  
    LinalgDat 2020
    Code for project C

    From Balder Runedal Ivarsen and François Lauze.
    François: As for Project B, rewriting of the test 
    structures to make them a bit clearer, the original 
    ones were too convoluted for my taste.
*)

module TestProjectC

open ProjectC
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

let TestSquareSubMatrix (A : Matrix) (i : int) (j : int) (expected : Matrix) =
    let taskName = "SquareSubMatrix(Matrix, int, int)"
    let mutable status = true
    let mutable result = ""
    // Would otherwise overwrite A
    let C = new Matrix(A)

    let provideContext (Av : Matrix) =
        let mutable l = [] : string list
        l <- l @ ["\n" + (sprintf "\n****** Input Matrix ******\n")]
        l <- l @ ["\n" + (sprintf "%A" (A.ToArray()))]
        l <- l @ ["\n" + (sprintf "\n****** line and columns to remove (%d, %d) ******\n" i j)]
        l <- l @ ["\n" + (sprintf "\n****** Actual result ******\n")]
        l <- l @ ["\n" + (sprintf "%A" (Av.ToArray()))]
        l <- l @ ["\n" + (sprintf "\n****** Expected result ******\n")]
        l <- l @ ["\n" + (sprintf "%A\n" (expected.ToArray()))]
        l |> String.concat("")
    
    result <- result + "\n" + sprintf "Tests for the %s function" taskName
    result <- result + "\n" + sprintf "==========================================================="
    
    try
        let Av = AdvancedOps.SquareSubMatrix C i j
        if not (compareMatrixDimensions Av expected) then
            status <- false
            result <- result + "\n" + OutMessage taskName "Dims" false
            result <- result + (provideContext Av)

        else
            result <- result + "\n" + OutMessage taskName "Dims" true
            if not (CompareMatrices Av expected) then
                status <- false
                result <- result + "\n" + OutMessage taskName "Values" false
                result <- result + (provideContext Av)
            else
                result <- result + "\n" + OutMessage taskName "Values" true
              
    with
    | ex -> status <- false
            result <- result + "\n" + OutMessage taskName "Run" status + "\n" + ex.Message
            
    if status then
        result <- result + "\n" + OutMessage taskName "All" true

    result <- result + "\n" + sprintf "\nEnd of test for the %s function." taskName
    result <- result + "\n" + sprintf "-----------------------------------------------------------\n"
    taskName, status, result


let TestDeterminant (A : Matrix) (expected : float) =
    let taskName = "Determinant(Matrix)"
    let mutable status = true
    let mutable result = ""
    // Would otherwise overwrite A
    let C = new Matrix(A)

    let provideContext (Av : float) = 
        let mutable l = [] : string list
        l <- l @ ["\n" + (sprintf "\n****** Input Matrix ******\n")]
        l <- l @ ["\n" + (sprintf "%A" (A.ToArray()))]
        l <- l @ ["\n" + (sprintf "\n****** Actual result ******\n")]
        l <- l @ ["\n" + (sprintf "%.05f" Av)]
        l <- l @ ["\n" + (sprintf "\n****** Expected result ******\n")]
        l <- l @ ["\n" + (sprintf "%.05f\n" expected)]
        l |> String.concat("")

    result <- result + "\n" + sprintf "Tests for the %s function" taskName
    result <- result + "\n" + sprintf "==========================================================="
    
    try
        let Av = AdvancedOps.Determinant C
        if abs (Av - expected) > Tolerance || (System.Double.IsNaN(Av)) then
            status <- false
            result <- result + "\n" + OutMessage taskName "Values" false
            result <- result + (provideContext Av)    
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




let TestSetColumn (A : Matrix) (v : Vector) (j : int) (expected : Matrix) =
    let taskName = "SetColumn(Matrix, Vector, int)"
    let mutable status = true
    let mutable result = ""
    // Would otherwise overwrite A
    let C = new Matrix(A)
    
    
    let provideContext (Av : Matrix) = 
        let mutable l = [] : string list
        l <- l @ ["\n" + (OutMessage taskName "Values" false)]
        l <- l @ ["\n" + (sprintf "\n****** Input Matrix A ******\n")]
        l <- l @ ["\n" + (sprintf "%A" (A.ToArray()))]
        l <- l @ ["\n" + (sprintf "\n****** Input Vector v ******\n")]
        l <- l @ ["\n" + (sprintf "%A" (v.ToArray()))]
        l <- l @ ["\n" + (sprintf "\n****** Index of column to be set: %i ******\n" j)]
        l <- l @ ["\n" + (sprintf "\n****** Actual result ******\n")]
        l <- l @ ["\n" + (sprintf "%A" (Av.ToArray()))]
        l <- l @ ["\n" + (sprintf "\n****** Expected result ******\n")]
        l <- l @ ["\n" + (sprintf "%A\n" (expected.ToArray()))]
        l |> String.concat("")

    result <- result + "\n" + sprintf "Tests for the %s function" taskName
    result <- result + "\n" + sprintf "-----------------------------------------------------------"
    
    try
        let Av = AdvancedOps.SetColumn C v j
        if not (compareMatrixDimensions Av expected) then
            status <- false
            result <- result + "\n" + OutMessage taskName "Dims" false
            result <- result + (provideContext Av)
        else
            result <- result + "\n" + OutMessage taskName "Dims" true
            if not (CompareMatrices Av expected) then
                status <- false
                result <- result + "\n" + OutMessage taskName "Values" false
                result <- result + (provideContext Av)
        
            else
                result <- result + "\n" + OutMessage taskName "Values" true
            
    with
    | ex -> status <- false
            result <- result + "\n" + OutMessage taskName "Run" false + "\n" + ex.Message
    
    if status then
        result <- result + "\n" + OutMessage taskName "All" true

    result <- result + "\n" + sprintf "\nEnd of test for the %s function." taskName
    result <- result + "\n" + sprintf "-----------------------------------------------------------\n"
    taskName,status,result





let TestGramSchmidt (A : Matrix)  (expected : Matrix * Matrix) =
    let taskName = "GramSchmidt(Matrix)"
    let mutable status = true
    let mutable result = ""
    // Would otherwise overwrite A
    let C = new Matrix(A)
    let eQ, eR = expected

    let provideContext (Av : Matrix * Matrix) = 
        let mutable l = [] : string list
        l <- l @ ["\n" + (OutMessage taskName "Values" false)]
        l <- l @ ["\n" + (sprintf "\n****** Input Matrix A ******\n")]
        l <- l @ ["\n" + (sprintf "%A" (A.ToArray()))]
        l <- l @ ["\n" + (sprintf "\n****** Actual result (fst) ******\n")]
        l <- l @ ["\n" + (sprintf "%A" ((fst Av).ToArray()))]
        l <- l @ ["\n" + (sprintf "\n****** Actual result (snd) ******\n")]
        l <- l @ ["\n" + (sprintf "%A" ((snd Av).ToArray()))]
        l <- l @ ["\n" + (sprintf "\n****** Expected result (fst) ******\n")]
        l <- l @ ["\n" + (sprintf "%A\n" ((fst expected).ToArray()))]
        l <- l @ ["\n" + (sprintf "\n****** Expected result (snd) ******\n")]
        l <- l @ ["\n" + (sprintf "%A\n" ((snd expected).ToArray()))]
        l |> String.concat("")


    result <- result + "\n" + sprintf "Tests for the %s function" taskName
    result <- result + "\n" + sprintf "-----------------------------------------------------------"
    
    try
        let aQ, aR = AdvancedOps.GramSchmidt C
        if not (compareMatrixDimensions eQ aQ) || not (compareMatrixDimensions eR aR) then
            status <- false
            result <- result + "\n" + OutMessage taskName "Dims" false
            result <- result + (provideContext (aQ, aR))
        else
            result <- result + "\n" + OutMessage taskName "Dims" true
            if not (CompareMatrices eQ aQ) || not (CompareMatrices eR aR) then
                status <- false
                result <- result + "\n" + OutMessage taskName "Values" false
                result <- result + (provideContext (aQ, aR))    
            else
                result <- result + "\n" + OutMessage taskName "Values" true
                
    with
    | ex -> status <- false
            result <- result + "\n" + OutMessage taskName "Run" false + "\n" + ex.Message
           
    if status then
        result <- result + "\n" + OutMessage taskName "All" true

    result <- result + "\n" + sprintf "\nEnd of test for the %s function." taskName
    result <- result + "\n" + sprintf "-----------------------------------------------------------\n"
    taskName,status,result

