type parseResult ('a) = 
| Exito ('a, string)
| Fallo (string, string);

type parser ('a) = Parser(string => (parseResult('a) => unit) => unit)

let compose = (f, g) =>
    x => g(f(x))

let ( -| ) = compose

let run = (parser, cadena, k) => 
switch(parser) {
| Parser(fn) => fn(cadena, k)
}

let parseChar = caracter =>{
    let innerFn = (cadena, k) => {
        String.(
        switch (length(cadena)) {
        | 0 => k(Fallo("Final de la cadena", cadena))
        | _ => let car1 = sub(cadena, 0,1)
                let resto = sub(cadena, 1, length(cadena) - 1)
            if (car1 == caracter) 
                k(Exito(caracter, resto))
            else 
            k(Fallo({j|Esperaba $caracter y obtuve $car1|j}, cadena))
        })
    }
    Parser(innerFn)
}

let parseNotChar = caracter =>{
let innerFn = (cadena, k) => {
    String.(
    switch (length(cadena)) {
    | 0 => k(Fallo("Final de la cadena", cadena))
    | _ => let car1 = sub(cadena, 0,1)
            let resto = sub(cadena, 1, length(cadena) - 1)
        if (car1 != caracter) 
            k(Exito(car1, resto))
        else 
        k(Fallo({j|No Esperaba $caracter |j}, cadena))
})}
Parser(innerFn)
}

let parserAny: unit => parser(string) = () =>{
let innerFn = (cadena, k) => {
    String.(
        switch (length(cadena)) {
        | 0 => k(Fallo("Final de la cadena", cadena))
        | _ => let car1 = sub(cadena, 0,1)
                let resto = sub(cadena, 1, length(cadena) - 1)
                k(Exito(car1, resto))
    })}
Parser(innerFn)
}

let parserOr = (p1, p2) =>{
    let innerFn = (cadena, k) => 
        String.(
        switch (length(cadena)) {
        | 0 => k(Fallo("Final de la cadena", cadena))
        | _ => run (p1, cadena,
                    fun 
                    | Exito(_) as s1 => k(s1)
                    | Fallo(_) => run (p2, cadena, k))
        })
    Parser(innerFn)
}

let ( <|> ) = parserOr

let parserAnd = (p1, p2) => {
    let innerFn = (cadena, k) => 
        String.(
        switch (length(cadena)) {
        | 0 => k(Fallo("Final de la cadena", cadena))
        | _ => run (p1, cadena,
                    fun
                    | Exito(valor1, resto1) =>
                        run (p2, resto1,
                            fun 
                            | Exito (valor2, resto2) => k(Exito((valor1, valor2) , resto2))
                            | Fallo (_, _) as e1 => k(e1))
                    | Fallo(_, _) as e1 => k(e1))
            
        })
Parser(innerFn)
}

let ( >-> ) = parserAnd;

let parserMap = (fn, p) => {
    let innerFn = (cadena, k) => 
        String.(
            switch (length(cadena)) {
            | 0 => k(Fallo("Final de la cadena", cadena))
            | _ =>
                    run(p, cadena, 
                        fun
                        | Exito(valor, resto) => k(Exito(fn(valor), resto))
                        | Fallo(_,_) as e1 => k(e1))
            })
    Parser(innerFn)
};

let ( <@> ) = parserMap

let parserReturn = valor => {
    let innerFn = (cadena, k) => 
        k(Exito(valor, cadena))
    Parser(innerFn)
}

let parserApply = (fP, xP) => 
        (((f,x)) => f(x)) <@> (fP >-> xP)

let ( <*> ) = parserApply

let parserChoice = (lista) => List.(
    fold_left(parserOr, hd(lista), tl(lista)));

let parserAll = lista => {
    let concatResults = (p1, p2) =>
        p1 >-> p2
        |> parserMap(((l1, l2)) => l1@l2);
    let lista2 = lista |> List.map (parserMap(p1=>[p1]))
    List.tl(lista2)
    |> List.fold_left (concatResults, List.hd(lista2) )
}

let parserAnyOf = cadena =>
Js.String.split("", cadena)
|> Array.to_list
|> List.map(parseChar)
|> parserChoice

let parseString = cadena => 
Js.String.split("", cadena)
|> Array.to_list
|> List.map(parseChar)
|> parserAll

let lift2 = (f, xP, yP) => 
    parserReturn(f) <*> xP <*> yP

let rec many = p => {
    let innerFn = (cadena, k) => 
        run(p, cadena,
            fun
            | Fallo (_) => k(Exito([], cadena))
            | Exito (valor1,resto1)  => 
                run(many(p),resto1,
                    fun
                    | Exito (valor2,resto2) => k(Exito(List.append([valor1], valor2), resto2))
                    | Fallo(_) => k(Exito ([valor1], resto1))))
    Parser(innerFn)
}

let many1 = p => {
    let innerFn = (cadena, k) => 
        run(p, cadena,
            fun
            | Fallo (_) as f => k(f)
            | Exito (valor1,resto1)  => 
                run(many(p),resto1,
                    fun
                    | Exito (valor2,resto2) => k(Exito(List.append([valor1], valor2), resto2))
                    | Fallo(_) => k(Exito([valor1], resto1))))
    Parser(innerFn)
}

let optional = (p) => {
    let innerFn = (cadena, k) => 
        run (p, cadena,
                fun
                | Exito(v1,r1) => k(Exito(Some(v1), r1))
                | Fallo(_) => k(Exito(None, cadena)))
    Parser(innerFn)
}

let skip = p => {
    let innerFn = (cadena, k) => 
        run(p, cadena,
            fun
            | Exito(_v1, r1) => k(Exito((), r1))
            | Fallo(_) as f => k(f))
    Parser(innerFn)
}

let keepLeft = (p1,p2) =>
    p1 >-> p2
    |> parserMap (((a,_b)) => a)

let keepRight = (p1,p2) => 
    p1 >-> p2
    |> parserMap (((_a,b)) => b)

let ( -<< ) = keepLeft

let ( ->> ) = keepRight;

let digit = parserAnyOf("0123456789");

let digits = many1(digit)

let intP = {Array.of_list -| Js.Array.joinWith("") -| int_of_string } <@> digits

let whitespace = parserAnyOf("\ \t\n\r")

let whitespaces = many(whitespace)