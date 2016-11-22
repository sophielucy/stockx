open Ocamlbuild_plugin;;

flag ["link"; "g++"] (S[A"-cc"; A"-cclib"; A"-rdynamic"]);;
dep ["link"; "ocaml"; "use_bindings"] ["bindings.o"];;
