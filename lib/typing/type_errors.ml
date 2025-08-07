open Error

let make_type_error msg pos = 
  type_error msg ~pos ()

let make_unbound_variable var pos = 
  unbound_variable var ~pos ()

let make_type_mismatch expected actual pos = 
  type_mismatch expected actual ~pos ()

let make_arity_mismatch expected actual pos = 
  arity_mismatch expected actual ~pos ()

let make_crypto_error msg pos = 
  crypto_error msg ~pos ()

let make_cyclic_dependency cycle pos = 
  cyclic_dependency cycle ~pos ()