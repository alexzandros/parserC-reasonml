type parseResult ('a) = 
    | Exito ('a, string)
    | Fallo (string, string)

type continuation('a) = parseResult('a) => unit

type  parserFn ('a) = Parser(string => continuation('a) => unit)

type parser('a) = {
    fn: parserFn('a),
    nombre: string
}

let compose = (f, g) =>
    x => g(f(x))

let ( -| ) = compose

let run = (parser, cadena, k) => 
switch(parser.fn) {
| Parser(fn) => fn(cadena, k)
}

let execParser = (parser, cadena) => {
    let resultados = ref ([]);
    run (parser, cadena,
        fun
        | Exito(v,_) => {resultados := [v, ...resultados^]}
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

let memoCPS = fn => {
    open Hashtbl;
    let isResultSaved = (tabla,cadena:string, result:parseResult('a)) => 
        find_all(tabla, cadena) |> List.mem(result)
    
    let tablaContinuaciones = create(10);
    let tablaResultados = create(10);
    let innerFn = Parser((arg, k) => {
        let listadoContinuaciones =  find_all(tablaContinuaciones, arg);
        if (List.length(listadoContinuaciones) == 0) {
            add(tablaContinuaciones, arg, k);
            run(fn, arg,
                result => {
                    switch (isResultSaved(tablaResultados, arg, result)) {
                    | false => {
                        add(tablaResultados, arg, result);
                        let lista =  find_all(tablaContinuaciones, arg);
                        lista |> List.iter(cont=> cont(result)) 
                        }
                    | _ => ()
                    };
                })
        } else {
                add(tablaContinuaciones, arg, k)
                find_all(tablaResultados,arg) |> List.iter(valor=>k(valor))
        }
    })
    let innerParser = {fn:innerFn,nombre:fn.nombre}
    innerParser
}

let parseString = memo(patron => {
    let innerFn = (cadena,k:continuation('a)) => {
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
    memoCPS({fn:Parser(innerFn), nombre:"parseString-" ++ patron})
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
    memoCPS({fn:Parser(innerFn),nombre:"parseChar-"++caracter})
})

let parserOr = memo((p1:parser(string), p2) =>{
    let innerFn = (cadena, k) => 
        String.(
        switch (length(cadena)) {
        | 0 => k(Fallo("Final de la cadena", cadena))
        | _ => {
                run(p1, cadena, k);
                run(p2, cadena, k);
                ();
            }
        });
    memoCPS({fn:Parser(innerFn),nombre:"parserOr-"++p1.nombre++"-"++p2.nombre})
})

let ( <|> ) = parserOr


let parserAnd = memo((p1:parser(string), p2:parser(string)) => {
    let innerFn = (cadena, k) => 
        String.(
        switch (length(cadena)) {
        | 0 => k(Fallo("Final de la cadena", cadena))
        | _ => run (p1, cadena,
                    fun
                    | Exito(valor1, resto1) =>
                        run (p2, resto1,
                            fun 
                            | Exito (valor2, resto2) => k(Exito(valor1 ++ valor2 , resto2))
                            | Fallo (_, _) as e1 => k(e1))
                    | Fallo(_, _) as e1 => k(e1))
            
        });
    memoCPS({fn:Parser(innerFn),nombre:"parserAnd-"++p1.nombre++"-"++p2.nombre})
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
    memoCPS({fn:Parser(innerFn),nombre:"parserMap-"++p.nombre})
});

let ( <@> ) = parserMap

let parserReturn = memo((valor:string) => {
    let innerFn = (cadena, k) => 
        k(Exito(valor, cadena));
        memoCPS({fn:Parser(innerFn),nombre:"parserReturn-"++valor})
})

let miko = parseString("carlos") <|> parseString("carlos rojas")

/* execParser(miko, "carlos rojas contreras") |> Js.log */

/* let s1 = parseString("a")
let rec s2 = arg => run(
    parserAnd({fn:Parser(s),nombre:"parserS"},s1),
    arg)
and s = arg => run(
    parserOr(s1, {fn:Parser(s2),nombre:"parserS2"}),
    arg
)
 */    

 let rec s = arg => run(
     parserOr(
         parseString("a"),
         parserAnd(
             {fn:Parser(s),nombre:"parserS"},
             parseString("a"))),
         arg)
run({fn:Parser(s),nombre:"parserS"}, "aaaaaaaaa", Js.log)