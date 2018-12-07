module Fraccionario {
    open ParserC;

    type fraccionario = {
        numerador:int,
        denominador: int
    };

    type operador = 
    | Plus
    | Minus
    | Times
    | DividedBy;

    let rec mcd = (a, b) =>
        switch (a mod b) {
        | 0 => b
        | r => mcd(b, r)
        };

    let mcm = (a, b)  =>
        a * b / mcd(a, b)

    let crearFraccion = (numerador, denominador) => {
        numerador: numerador / mcd(numerador, denominador),
        denominador: denominador / mcd(numerador, denominador)
    }

    let ( +/ ) = (f1, f2) => {
        let denominador = mcm(f1.denominador, f2.denominador);
        let num1 = f1.numerador * denominador / f1.denominador;
        let num2 = f2.numerador * denominador / f2.denominador;
        let numerador = num1 + num2;
        crearFraccion (numerador, denominador)
    }

    let ( -/ ) = (f1, f2) => {
        let denominador = mcm(f1.denominador, f2.denominador);
        let num1 = f1.numerador * denominador / f1.denominador;
        let num2 = f2.numerador * denominador / f2.denominador;
        let numerador = num1 - num2;
        crearFraccion (numerador, denominador)
    }

    let ( *\/ ) = (f1, f2) => {
        let numerador = f1.numerador * f2.numerador;
        let denominador = f1.denominador * f2.denominador;
        crearFraccion(numerador, denominador)
    }

    let ( /\/ ) = (f1, f2) => {
        let numerador = f1.numerador * f2.denominador;
        let denominador = f1.denominador * f2.numerador;
        crearFraccion(numerador, denominador)
    }

    let string_of_frac = (f) =>
        string_of_int(f.numerador) ++ "/" ++ string_of_int(f.denominador)

    let operador = 
            {fun
            | "+" => Plus
            | "-" => Minus
            | "*" => Times
            | _ => DividedBy}
            <@>
            whitespaces ->> parserAnyOf("/*-+") -<< whitespaces

    let parseFrac = 
        {((n,d)) => crearFraccion(n,d)}
        <@>
        {(intP -<< parseChar("/")) >-> intP}

    let operaFrac =     
        {(((f1, op), f2)) => 
            switch (op) {
            | Plus => f1 +/ f2
            | Minus => f1 -/ f2
            | Times => f1 *\/ f2
            | DividedBy => f1 /\/ f2
            };
        }
        <@>
        (parseFrac >-> operador >-> parseFrac);

    run(string_of_frac <@> operaFrac, "8/5+2/4",
        {
            fun
            | Exito(v,_) => v
            | Fallo(e,_) => e
        } -| Js.log)
}