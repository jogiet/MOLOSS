open Ocamlbuild_plugin

let () =
  dispatch begin function
    | After_hygiene ->
      if getenv "STATIC" ~default:"false" = "true" then
        tag_any ["ccopt(-static)"]
    | _ -> ()
  end
