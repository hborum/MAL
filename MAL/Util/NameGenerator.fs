namespace itu.dk.MAL

module NameGenerator =

  let mutable i = 0
  let getName () =
    do i <- i+1
    sprintf "tempname_%d" i

  let wishName wish =
    do i <- i+1
    sprintf "%s_t_%d" wish i
