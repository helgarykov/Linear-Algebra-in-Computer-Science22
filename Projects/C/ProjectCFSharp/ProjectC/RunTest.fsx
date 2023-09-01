// Generated test data for LinAlgDat Project C
// Seed: 656
// Date of creation: 26/05/2021

module testData
open ProjectC
open TestProjectC
open LinAlgDat.Core

let details = true
let summary = true

let mutable resultsList = []
// data for 'squareSubMatrix()'

let mutable ssmList = []

// random dimensions (m = 5)
let ssmA1 = array2D [[ -8.01;  -7.84;   3.06;  -4.78;  13.43]
                     [ 13.24;   1.88; -14.58; -13.03;   3.48]
                     [  4.63;   4.02;  10.27; -13.46;   8.64]
                     [  4.71;   8.59;  -2.59;  -7.17;  19.19]
                     [ 11.55;  14.36; -11.86;  19.66;  -1.83]]|> Matrix
let ssmi1 = 4
let ssmj1 = 0
let ssmAv1 = array2D [[ -7.84;   3.06;  -4.78;  13.43]
                      [  1.88; -14.58; -13.03;   3.48]
                      [  4.02;  10.27; -13.46;   8.64]
                      [  8.59;  -2.59;  -7.17;  19.19]]|> Matrix
let ssm1 = TestSquareSubMatrix ssmA1 ssmi1 ssmj1 ssmAv1
ssmList <- ssmList @ [ssm1]

// random dimensions (m = 7)
let ssmA2 = array2D [[ -4.12;  -5.30; -10.08; -16.39;  -2.61;   8.15; -12.30]
                     [ -2.90;  -2.24;   7.37;  -2.55; -15.46;  18.73;  -2.12]
                     [  0.36;  10.95;  -0.22;   0.89;   9.36; -10.65;  16.97]
                     [  4.94;  -4.88;  -5.09; -14.69;   1.82;   5.93;  -2.38]
                     [-15.98;   3.40;  17.05;  15.84;   7.23;  12.18;   9.54]
                     [ 12.26;  -6.45;   1.01; -13.38; -17.88;  18.83;  -8.46]
                     [-13.06; -18.78;  19.96; -11.71; -19.54;  -6.34;  -6.00]]|> Matrix
let ssmi2 = 3
let ssmj2 = 3
let ssmAv2 = array2D [[ -4.12;  -5.30; -10.08;  -2.61;   8.15; -12.30]
                      [ -2.90;  -2.24;   7.37; -15.46;  18.73;  -2.12]
                      [  0.36;  10.95;  -0.22;   9.36; -10.65;  16.97]
                      [-15.98;   3.40;  17.05;   7.23;  12.18;   9.54]
                      [ 12.26;  -6.45;   1.01; -17.88;  18.83;  -8.46]
                      [-13.06; -18.78;  19.96; -19.54;  -6.34;  -6.00]]|> Matrix
let ssm2 = TestSquareSubMatrix ssmA2 ssmi2 ssmj2 ssmAv2
ssmList <- ssmList @ [ssm2]

// random dimensions (m = 9)
let ssmA3 = array2D [[-11.77;  16.66;  12.72;  10.93;  19.57;  15.01;  18.30;  15.57; -17.78]
                     [ 10.07;  -6.93;  14.36;  10.32;   1.61;  -2.69; -13.45;   8.33;   8.09]
                     [ -2.61;  18.76;  -5.74;   7.23;  -9.49;   5.43;  -2.00;  -6.10;  -5.06]
                     [ 17.01;   1.36;  -7.74; -16.38;   6.83; -16.49;  19.77;   7.60;  10.56]
                     [ 19.45;  18.61;  12.68;  -1.47;   2.71;  -8.95;  19.90; -11.24;  18.46]
                     [ -7.43;  -9.11;  -1.65;  10.97;  13.91;   5.32;   5.91;  19.78;   1.12]
                     [  3.01;   0.99;   6.14; -12.72;  -7.08;   5.67;   0.06; -18.72;  -5.79]
                     [  9.95;   6.45;   6.23;  10.85;  10.54;  14.00;   6.74; -12.91;   9.72]
                     [ 15.40;  18.85;  -3.96; -16.79;   7.33; -18.25;   6.97; -14.58; -16.99]]|> Matrix
let ssmi3 = 8
let ssmj3 = 1
let ssmAv3 = array2D [[-11.77;  12.72;  10.93;  19.57;  15.01;  18.30;  15.57; -17.78]
                      [ 10.07;  14.36;  10.32;   1.61;  -2.69; -13.45;   8.33;   8.09]
                      [ -2.61;  -5.74;   7.23;  -9.49;   5.43;  -2.00;  -6.10;  -5.06]
                      [ 17.01;  -7.74; -16.38;   6.83; -16.49;  19.77;   7.60;  10.56]
                      [ 19.45;  12.68;  -1.47;   2.71;  -8.95;  19.90; -11.24;  18.46]
                      [ -7.43;  -1.65;  10.97;  13.91;   5.32;   5.91;  19.78;   1.12]
                      [  3.01;   6.14; -12.72;  -7.08;   5.67;   0.06; -18.72;  -5.79]
                      [  9.95;   6.23;  10.85;  10.54;  14.00;   6.74; -12.91;   9.72]]|> Matrix
let ssm3 = TestSquareSubMatrix ssmA3 ssmi3 ssmj3 ssmAv3
ssmList <- ssmList @ [ssm3]

resultsList <- resultsList @ [ssmList]

// data for 'determinant()'

let mutable detList = []

// random dimensions (m = 7)
let detA1 = array2D [[  6.93;  -2.29;   7.05;   2.39;  -3.24; -13.39; -15.58]
                     [ 12.17;   5.68; -12.99;  18.47;   1.42;  16.84; -10.06]
                     [  2.30;  -7.78;   1.72;   0.35;   4.35; -15.77;   6.81]
                     [ 15.60;  10.50; -19.08; -14.93;  -0.20;  13.65;   4.39]
                     [  0.31; -12.50;   2.66;   3.47;  15.24;   6.76; -16.98]
                     [-11.26;  19.18; -11.55;  -6.22;   7.38;   2.32;  -5.08]
                     [-15.97;   8.38; -19.62;  14.62; -13.64;  -2.61;  -0.56]]|> Matrix
let detAv1 = 3548584915.02763000
let det1 = TestDeterminant detA1 detAv1
detList <- detList @ [det1]

// random dimensions (m = 4)
let detA2 = array2D [[  3.69;   2.65;  -7.76; -18.19]
                     [-19.37;  11.32;   7.20;   6.12]
                     [ -2.68;  16.96;  -7.05;   0.03]
                     [ -8.15;  -1.82;   4.35;  -5.01]]|> Matrix
let detAv2 = 1431.49594494
let det2 = TestDeterminant detA2 detAv2
detList <- detList @ [det2]

// random dimensions (m = 7)
let detA3 = array2D [[ -9.92; -13.09;  -6.44;  -6.96;  -6.73; -13.44;  16.96]
                     [  1.90;  -8.11;  11.57; -17.89; -19.47;  -9.56;  -6.57]
                     [ -3.74; -15.06; -14.43;   3.81; -20.00;  -8.77;  13.43]
                     [  6.13;  -8.31;   1.53;  -6.95;  11.13;  -3.18;  -5.57]
                     [-17.72; -16.93;   5.85; -15.97;   5.39; -11.56;  16.28]
                     [ -9.53; -14.12; -18.31;  -9.58;  -3.91;  -4.72;  -0.10]
                     [  8.73;  19.75;   5.08;   5.58; -13.13;   3.52;  16.69]]|> Matrix
let detAv3 = 552064501.35235800
let det3 = TestDeterminant detA3 detAv3
detList <- detList @ [det3]

// Test data for determinant. For TAs
// dimension (m = 1)
let detA4 = array2D [[0.00]] |> Matrix
let detAv4 = 0.00
let det4 = TestDeterminant detA4 detAv4
detList <- detList @ [det4]

// dimension (m = 1)
let detA5 = array2D [[2.00]] |> Matrix
let detAv5 = 2.00
let det5 = TestDeterminant detA5 detAv5
detList <- detList @ [det5]

// dimension (m = 3)
let detA6 = array2D [[ 0.00; 10.00;  0.00]
                     [ 1.00;  0.00;  0.00]
                     [ 0.00;  0.00;  1.00]]|> Matrix
let detAv6 = -10.00
let det6 = TestDeterminant detA6 detAv6
detList <- detList @ [det6]

resultsList <- resultsList @ [detList]

// data for 'gramSchmidt()'

let mutable grsList = []

// Gram-Schmidt test: maximal rank matrices m > n
// random dimensions (m = 5, n = 3)
let grsA1 = array2D [[ -4.86; -18.00;  -2.86]
                     [-14.21;  19.06; -16.13]
                     [ -6.62;   3.90;  15.94]
                     [ -6.47;  17.39;  -8.06]
                     [ 16.45;   7.46;   4.26]]|> Matrix
let grsAv1Q = array2D [[-0.20148265; -0.62410987; -0.23338026]
                       [-0.58910874;  0.45074756; -0.31962560]
                       [-0.27444756;  0.05193184;  0.91377983]
                       [-0.26822896;  0.48175790; -0.09061787]
                       [ 0.68197317;  0.41536239; -0.01295945]]|> Matrix
let grsAv1R = array2D [[24.12118364; -8.24905208; 10.77100129]
                       [ 0.00000000; 31.50413369; -6.77133520]
                       [ 0.00000000;  0.00000000; 21.06385176]]|> Matrix
let grsAv1 = (grsAv1Q, grsAv1R)
let grs1 = TestGramSchmidt grsA1 grsAv1
grsList <- grsList @ [grs1]

// random dimensions (m = 8, n = 5)
let grsA2 = array2D [[-15.20;  10.53;   5.20; -17.83;   5.10]
                     [ 17.54; -16.81;  -6.76;  -2.52;  -2.08]
                     [-10.55;  -2.98;  -0.61;   6.91;   6.84]
                     [ -8.83;  14.39;  -9.50;  -0.78;  -6.19]
                     [ 16.47;   5.78;  11.98;  -1.72;   9.18]
                     [ 10.41;   9.91;   7.96; -12.14; -12.12]
                     [ -0.22; -11.41;  -3.08;  -1.45; -18.43]
                     [-19.65;  10.82; -19.78;  15.87; -17.11]]|> Matrix
let grsAv2Q = array2D [[-0.39328081;  0.17254491;  0.42897179; -0.63707317;  0.19902089]
                       [ 0.45382535; -0.36628475; -0.47146415; -0.41719703;  0.32235764]
                       [-0.27296793; -0.24942893;  0.23607764;  0.28758758;  0.08911899]
                       [-0.22846510;  0.39709233; -0.41276397; -0.24545621;  0.18749620]
                       [ 0.42614045;  0.42986218;  0.11061831;  0.32620973;  0.06659111]
                       [ 0.26934561;  0.49620787;  0.01003659; -0.27177041; -0.52347005]
                       [-0.00569222; -0.41239770;  0.00784115; -0.23120807; -0.69999291]
                       [-0.50841894;  0.12284552; -0.59598607;  0.20832649; -0.21197611]]|> Matrix
let grsAv2R = array2D [[ 38.64922897; -14.54805736;  14.54722164;  -7.90262078;   6.04886582]
                       [  0.00000000;  27.86827097;   7.69295716;  -8.40251556;   0.90845417]
                       [  0.00000000;   0.00000000;  22.36455349; -14.28900121;  18.28482333]
                       [  0.00000000;   0.00000000;   0.00000000;  20.96864234;   8.09033092]
                       [  0.00000000;   0.00000000;   0.00000000;   0.00000000;  23.27701903]]|> Matrix
let grsAv2 = (grsAv2Q, grsAv2R)
let grs2 = TestGramSchmidt grsA2 grsAv2
grsList <- grsList @ [grs2]

// random dimensions (m = 3, n = 2)
let grsA3 = array2D [[ -2.06;   1.24]
                     [ -2.37; -16.26]
                     [ 19.17;  -0.30]]|> Matrix
let grsAv3Q = array2D [[-0.10604627;  0.08654054]
                       [-0.12200469; -0.98981089]
                       [ 0.98684804; -0.11307138]]|> Matrix
let grsAv3R = array2D [[19.42548326;  1.55624442]
                       [ 0.00000000; 16.23555676]]|> Matrix
let grsAv3 = (grsAv3Q, grsAv3R)
let grs3 = TestGramSchmidt grsA3 grsAv3
grsList <- grsList @ [grs3]

resultsList <- resultsList @ [grsList]

let printSummaries llst =
    printfn "\nSummary\n=============================="
    let printSummary lst =
        let pass = List.filter (fun (x,y,z) -> y) lst
        match lst.Head with
        | x,y,z -> 
            let s = sprintf "Tests of %s passed/total:" x
            printfn "%-50s [%i/%i]" s (pass.Length) (lst.Length)
    List.map printSummary llst |> ignore
    printfn "------------------------------"

let printDetails llst =
    printfn "\nTest results\n=============================="
    let printDetail (lst : (string * bool * string) list) =
        List.map (fun (x,y,z) -> printf "%s" z) lst
    List.map printDetail llst |> ignore
    printfn ""

if details then printDetails resultsList
if summary then printSummaries resultsList
