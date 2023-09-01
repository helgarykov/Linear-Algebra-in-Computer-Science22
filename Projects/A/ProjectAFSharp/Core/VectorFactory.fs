namespace LinAlgDat.Core

type VectorFactory = class
  static member Ones(n : int) : Vector =
    let retval = Vector(n)
    for i in 0..n-1 do
      retval.[i] <- 1.0
    retval
end
