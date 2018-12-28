let hola = Printf.printf("Hola, mi nombre es %s")

let nombres = ["Nanita", "Arturito", "Thorcito", "Oshito"]

nombres |>List.iter(hola);

let add1 = (+)(1);
let equals2 = x => x mod 2 == 0
Array. (
    init(100, (+)(1))
    |> map (add1)
    |> to_list
    |> List.filter (equals2)
)

let bind = (f, valor) => 
    switch (valor) {
    | Some(x) => f(x)
    | None => None
    };
 

let c1 = Complex.{re: 4., im:2.}
let c2 = Complex.{re: 1., im:2.}

let ( *\/ ) = Complex.mul;
c1 *\/ c2


module type X_int = {let x: int;};
module Three: X_int = {
  let x = 3;
};
module Four = {
  let x = 4;
};
let three = (module Three: X_int);
let numbers = [
  three,
  (module Four),
  (module
   {
     let x = 5;
   }),
];

module New_three = (val three: X_int);

let to_int = (module M: X_int) => M.x;

let plus = (m1, m2): (module X_int) =>
  (module
   {
     let x = to_int(m1) + to_int(m2);
   });
