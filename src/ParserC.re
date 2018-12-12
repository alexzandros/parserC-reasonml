type parseResult ('a) = 
    | Exito ('a, string)
    | Fallo (string, string)

type continuation('a) = parseResult('a) => unit

type  parser ('a) = Parser(string => continuation('a) => unit)

type trampoline = {
    tabla: Hashtbl.t((parser('a), string), (list(parseResult('a)), list(continuation('a)))),
    mutable pila: list((parser('a), string, continuation('a)))
} constraint 'a = [ `O  | `I ]

let compose = (f, g) =>
    x => g(f(x))

let ( -| ) = compose

let run = (parser, cadena, k):unit => 
switch(parser) {
| Parser(fn) => fn(cadena, k)
}

let execParser = (parser, cadena) => {
    let resultados = ref ([]);
    run (parser, cadena,
        fun
        | Exito(v,"") => {resultados := [v, ...resultados^]}
        | _ => () )
    resultados^ |> Array.of_list
}

let memo = fn => {
    let tabla = Hashtbl.create(10);
    arg =>
        switch (Hashtbl.find(tabla, arg)) {
        | exception Not_found => {
            let valor = fn(arg);
            Hashtbl.add(tabla, arg, valor);
            valor
        }
        | valor => valor
        };
    };

let memo2 = fn => {
    let tabla = Hashtbl.create(10);
    (arg1, arg2) =>
        switch (Hashtbl.find(tabla, (arg1,arg2))) {
        | exception Not_found => {
            let valor = fn(arg1, arg2);
            Hashtbl.add(tabla, (arg1, arg2), valor);
            valor
        }
        | valor => valor
        };
    }


let memoCPS = fn => {
    open Hashtbl;
    let tablaContinuaciones = create(10);
    let tablaResultados = create(10);
    Parser((arg, k) => {
        let listadoContinuaciones =  switch(find(tablaContinuaciones, arg)) {
        | exception Not_found => []
        | lista => lista
        };
        if (List.length(listadoContinuaciones) == 0) {
            add(tablaContinuaciones, arg,[k]);
            run(fn, arg,
                result => {
                    switch (find(tablaResultados, arg)) {
                    | exception Not_found => {
                        add(tablaResultados, arg, result);
                        switch (find(tablaContinuaciones, arg)) {
                        | exception Not_found => ()
                        | lista => List.iter(cont=> cont(result), lista)
                        }
                    }
                    | _ => ()
                    };
                })
        } else {
                replace(tablaContinuaciones, arg, [k, ...listadoContinuaciones])
                switch (find(tablaResultados, arg)) {
                | exception Not_found => ()
                | valor => k(valor)
                };
        }
    })
}

let pushContinuation = (tramp, parser, cadena, cont) => {
    switch (Hashtbl.find(tramp.tabla, (parser, cadena))) {
    | (resultados, continuaciones) => {
        Hashtbl.replace(tramp.tabla, (parser, cadena), (resultados, [cont, ...continuaciones]));
        List.iter(cont, resultados)
    }
    | exception Not_found => {
        Hashtbl.replace(tramp.tabla, (parser, cadena), ([], [cont]))
        tramp.pila = [(parser, cadena,
            result =>{
                switch (Hashtbl.find(tramp.tabla, (parser, cadena))){
                | (resultados, continuaciones) => {
                    if (!List.mem(result, resultados)) {
                        Hashtbl.replace(tramp.tabla,
                                (parser, cadena),
                                ([result, ...resultados], continuaciones));
                        List.iter(c => c(result), continuaciones)
                    }
                    }
                }
            }), ...tramp.pila]
    }
    };
}

/* let string_of_list = Array.of_list -| Js.Array.joinWith("") */

let parseString = memo(patron => {
    let innerFn = (cadena,k) => {
        Js.log("funcion parseString: " ++ patron ++ " " ++ cadena)
        let longPatron = String.length(patron);
        let longCadena = String.length(cadena);
        if (longPatron > longCadena) {
            k(Fallo("Final de la cadena", cadena))
        } else {
            let subcadena = String.sub(cadena,0, longPatron);
            if (subcadena == patron) {
                k(Exito(subcadena, String.sub(cadena, longPatron, longCadena-longPatron)))
            } else {
                k(Fallo("No esperado", cadena))
            }
        }
    };
    memoCPS(Parser(innerFn))
})

let parseChar = memo (caracter =>{
    let innerFn = (cadena:string, k:continuation('a)) => {
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
    };
    memoCPS(Parser(innerFn))
})

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
    })};
    Parser(innerFn)
}

let parserAny = () =>{
    let innerFn = (cadena, k) => {
        String.(
            switch (length(cadena)) {
            | 0 => k(Fallo("Final de la cadena", cadena))
            | _ => let car1 = sub(cadena, 0,1)
                    let resto = sub(cadena, 1, length(cadena) - 1)
                    k(Exito(car1, resto))
        })};
    Parser( innerFn)
}

let parserOr = memo2((p1:parser(string), p2) =>{
    let innerFn = (cadena, k) => 
        String.(
        switch (length(cadena)) {
        | 0 => k(Fallo("Final de la cadena", cadena))
        | _ => {
                run(p1, cadena, k);
                run(p2, cadena, k);
            }
        });
    memoCPS(Parser(innerFn))
})

let ( <|> ) = parserOr


let parserAnd = memo2((p1:parser(string), p2:parser(string)) => {
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
            
        });
    memoCPS(Parser(innerFn))
})

let ( >-> ) = parserAnd;

let parserMap = memo((fn:string=>string, p:parser(string)) => {
    let innerFn = (cadena, k) => 
        String.(
            switch (length(cadena)) {
            | 0 => k(Fallo("Final de la cadena", cadena))
            | _ =>
                    run(p, cadena,
                        fun
                        | Exito(valor, resto) => k(Exito(fn(valor), resto))
                        | Fallo(_,_) as e1 => k(e1))
            });
    memoCPS(Parser(innerFn))
});

let ( <@> ) = parserMap

/* let parserReturn = memo(valor => {
    let innerFn = (cadena, k) => 
        k(Exito(valor, cadena));
        memoCPS(Parser(innerFn))
})
 */
/* let parserApply = (fP, xP) => 
        (((f,x)) => f(x)) <@> (fP >-> xP)

let ( <*> ) = parserApply
 */
/* let parserChoice = (lista) => List.(
    fold_left(parserOr, hd(lista), tl(lista))); */

/* let parserAll = lista => {
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
string_of_list <@>
(Js.String.split("", cadena)
    |> Array.to_list
    |> List.map(parseChar)
    |> parserAll)

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
                    | Exito (valor2,resto2) => k(Exito([valor1] @ valor2, resto2))
                    | Fallo(_) => k(Exito ([valor1], resto1))));
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
                    | Exito (valor2,resto2) => k(Exito([valor1] @ valor2, resto2))
                    | Fallo(_) => k(Exito([valor1], resto1))));
    Parser(innerFn)
}

let optional = (p) => {
    let innerFn = (cadena, k) => 
        run (p, cadena,
                fun
                | Exito(v1,r1) => k(Exito(Some(v1), r1))
                | Fallo(_) => k(Exito(None, cadena)));
    Parser(innerFn)
}

let skip = p => {
    let innerFn = (cadena, k) => 
        run(p, cadena,
            fun
            | Exito(_v1, r1) => k(Exito((), r1))
            | Fallo(_) as f => k(f));
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

let intP = {string_of_list -| int_of_string } <@> digits

let whitespace = parserAnyOf("\ \t\n\r")

let whitespaces = many(whitespace); */

let miko = parserOr(parseString("carlos"), parseString("carlos rojas"))

execParser(parseString("carlos rojas"), "carlos rojas") |> Js.log
run(miko, "carlos rojas c", Js.log)
run(miko, "carlos rojas c", Js.log)
run(miko, "carlos rojas c", Js.log)
