type parseResult ('a) = 
  | Success (array('a), string)
  | Failure (string, string);

type parser ('a) = Parser(string => parseResult('a))

let run = (parser, cadena) => 
    switch(parser) {
    | Parser(fn) => fn(cadena)
    }

/*let parseChar: string=> parser(string) = caracter =>
   {cadena => String.(
    switch (length(cadena)) {
    | 0 => Failure("Final de la cadena", cadena)
    | _ => let car1 = sub(cadena, 0,1)
            let resto = sub(cadena, 1, length(cadena) - 1)
        if (car1 == caracter) 
            Success([|caracter|], resto)
        else 
        Failure({j|Esperaba $caracter y obtuve $car1|j}, cadena);
    })};


let parseNotChar: string => parser(string) =   (caracter: string) =>
  (cadena: string) => String.(
    switch (length(cadena)) {
    | 0 => Failure("Final de la cadena", cadena)
    | _ => let car1 = sub(cadena, 0,1)
            let resto = sub(cadena, 1, length(cadena) - 1)
        if (car1 != caracter) 
            Success([|car1|], resto)
        else 
        Failure({j|No Esperaba $caracter |j}, cadena);
    });

let parserAny: unit => parser(string) = () =>
    (cadena: string ) => String.(
        switch (length(cadena)) {
        | 0 => Failure("Final de la cadena", cadena)
        | _ => let car1 = sub(cadena, 0,1)
                let resto = sub(cadena, 1, length(cadena) - 1)
                Success([|car1|], resto)
        })*/

let parserOr = (p1, p2) =>{
    let innerFn = cadena => String.(
    switch (length(cadena)) {
    | 0 => Failure("Final de la cadena", cadena)
    | _ => switch(run (p1, cadena)) {
            | Success(_, _) as s1 => s1
            | Failure (_, _) => run(p2, cadena)
            }
    })
    Parser(innerFn)
}

let parserAnd = (p1, p2) => {
    let innerFn = (cadena) => String.(
        switch (length(cadena)) {
        | 0 => Failure("Final de la cadena", cadena)
        | _ =>
            switch (run (p1, cadena)) {
            | Success(valor1, resto1) =>
                switch (run (p2, resto1)) {
                | Success (valor2, resto2) => Success(Array.append(valor1, valor2) , resto2)
                | Failure (_, _) as e1 => e1
                }
            | Failure(_, _) as e1 => e1
            }
        })
    Parser(innerFn)
}

/*let parserChoice = (lista) => Array.(
        fold_left(parserOr, get(lista, 0), sub(lista, 1, length(lista) - 1)))

let parserAll = (lista) => Array.(
        fold_left(parserAnd, get(lista, 0), sub(lista, 1, length(lista) - 1))
)

let parserAnyOf = cadena => parserChoice(Array.map(parseChar, Js.String.split("", cadena)))

let parserSequence = cadena => parserAll(Array.map(parseChar,  Js.String.split("", cadena)))*/
