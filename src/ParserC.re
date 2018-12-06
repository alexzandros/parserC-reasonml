module ParserC = {
    type parseResult ('a) = 
    | Success ('a, string)
    | Failure (string, string);

    type parser ('a) = Parser(string => parseResult('a))

    let run = (parser, cadena) => 
    switch(parser) {
    | Parser(fn) => fn(cadena)
    }

    let parseChar = caracter =>{
    let innerFn = cadena => String.(
    switch (length(cadena)) {
    | 0 => Failure("Final de la cadena", cadena)
    | _ => let car1 = sub(cadena, 0,1)
            let resto = sub(cadena, 1, length(cadena) - 1)
        if (car1 == caracter) 
            Success(caracter, resto)
        else 
        Failure({j|Esperaba $caracter y obtuve $car1|j}, cadena);
    })
    Parser(innerFn)
    }

    let parseNotChar = caracter =>{
    let innerFn = cadena => String.(
    switch (length(cadena)) {
    | 0 => Failure("Final de la cadena", cadena)
    | _ => let car1 = sub(cadena, 0,1)
            let resto = sub(cadena, 1, length(cadena) - 1)
        if (car1 != caracter) 
            Success(car1, resto)
        else 
        Failure({j|No Esperaba $caracter |j}, cadena);
    })
    Parser(innerFn)
    }

    let parserAny: unit => parser(string) = () =>{
    let innerFn = cadena => String.(
        switch (length(cadena)) {
        | 0 => Failure("Final de la cadena", cadena)
        | _ => let car1 = sub(cadena, 0,1)
                let resto = sub(cadena, 1, length(cadena) - 1)
                Success(car1, resto)
        })
    Parser(innerFn)
    }

    let parserOr = (p1, p2) =>{
    let innerFn = cadena => String.(
    switch (length(cadena)) {
    | 0 => Failure("Final de la cadena", cadena)
    | _ => switch(run (p1, cadena)) {
            | Success(_) as s1 => s1
            | Failure (_) => run(p2, cadena)
            }
    })
    Parser(innerFn)
    }

    let ( <|> ) = parserOr

    let parserAnd = (p1, p2) => {
    let innerFn = (cadena) => String.(
        switch (length(cadena)) {
        | 0 => Failure("Final de la cadena", cadena)
        | _ =>
            switch (run (p1, cadena)) {
            | Success(valor1, resto1) =>
                switch (run (p2, resto1)) {
                | Success (valor2, resto2) => Success((valor1, valor2) , resto2)
                | Failure (_, _) as e1 => e1
                }
            | Failure(_, _) as e1 => e1
            }
        })
    Parser(innerFn)
    };

    let ( >-> ) = parserAnd;

    let parserMap = (fn, p) => {
    let innerFn = cadena => String.(
        switch (length(cadena)) {
        | 0 => Failure("Final de la cadena", cadena)
        | _ =>
            switch (run(p, cadena)) {
            | Success(valor, resto) => Success(fn(valor), resto)
            | Failure(_,_) as e1 =>e1
            };
        })
    Parser(innerFn)
    };

    let ( <@> ) = parserMap

    let parserReturn = valor => {
        let innerFn = cadena => 
            Success(valor, cadena)
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
        let innerFn = cadena => {
            switch (run(p, cadena)) {
            | Failure (_) => Success([], cadena)
            | Success (valor1,resto1)  => 
                switch(run(many(p),resto1)) {
                | Success (valor2,resto2) => Success(List.append([valor1], valor2), resto2)
                | Failure(_) => Success ([valor1], resto1)
                }
            }
        }
        Parser(innerFn)
    }

    let many1 = p => {
        let innerFn = cadena => {
            switch (run(p, cadena)) {
            | Failure (_) as f => f
            | Success (valor1,resto1)  => 
                switch(run(many(p),resto1)) {
                | Success (valor2,resto2) => Success(List.append([valor1], valor2), resto2)
                | Failure(_) => Success ([valor1], resto1)
                }
            }
        }
        Parser(innerFn)
    }

    let optional = (p) => {
        let innerFn = cadena => 
            switch (run (p, cadena)) {
                    | Success(v1,r1) => Success(Some(v1), r1)
                    | Failure(_) => Success(None, cadena)
                    }
        Parser(innerFn)
    }

    let skip = p => {
        let innerFn = cadena => 
            switch (run(p, cadena)) {
            | Success(_v1, r1) => Success((), r1)
            | Failure(_) as f => f
            };
        Parser(innerFn)
    }

    let keepLeft = (p1,p2) => {
        p1 >-> p2
        |> parserMap (((a,_b)) => a)
    }
    
    let keepRight = (p1,p2) => {
        p1 >-> p2
        |> parserMap (((_a,b)) => b)
    }

    let ( -<< ) = keepLeft

    let ( ->> ) = keepRight;

    let digit = parserAnyOf("0123456789");

    digit ->> digit;

    let digits = many1(digit)

    let intP = {arreglo => arreglo |> Array.of_list |> Js.Array.joinWith("") |> int_of_string } <@> digits
};

let miko = ParserC.(run(parserAnd(intP,  optional(parseChar(";"))),"23;"));

