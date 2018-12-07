// Generated by BUCKLESCRIPT VERSION 4.0.8, PLEASE EDIT WITH CARE
'use strict';

var Caml_int32 = require("bs-platform/lib/js/caml_int32.js");
var ParserC$ParserLibrary = require("./ParserC.bs.js");

function mcd(_a, _b) {
  while(true) {
    var b = _b;
    var a = _a;
    var r = Caml_int32.mod_(a, b);
    if (r !== 0) {
      _b = r;
      _a = b;
      continue ;
    } else {
      return b;
    }
  };
}

function mcm(a, b) {
  return Caml_int32.div(Caml_int32.imul(a, b), mcd(a, b));
}

function crearFraccion(numerador, denominador) {
  return /* record */[
          /* numerador */Caml_int32.div(numerador, mcd(numerador, denominador)),
          /* denominador */Caml_int32.div(denominador, mcd(numerador, denominador))
        ];
}

function $plus$slash(f1, f2) {
  var denominador = mcm(f1[/* denominador */1], f2[/* denominador */1]);
  var num1 = Caml_int32.div(Caml_int32.imul(f1[/* numerador */0], denominador), f1[/* denominador */1]);
  var num2 = Caml_int32.div(Caml_int32.imul(f2[/* numerador */0], denominador), f2[/* denominador */1]);
  var numerador = num1 + num2 | 0;
  return crearFraccion(numerador, denominador);
}

function $neg$slash(f1, f2) {
  var denominador = mcm(f1[/* denominador */1], f2[/* denominador */1]);
  var num1 = Caml_int32.div(Caml_int32.imul(f1[/* numerador */0], denominador), f1[/* denominador */1]);
  var num2 = Caml_int32.div(Caml_int32.imul(f2[/* numerador */0], denominador), f2[/* denominador */1]);
  var numerador = num1 - num2 | 0;
  return crearFraccion(numerador, denominador);
}

function $star$slash(f1, f2) {
  var numerador = Caml_int32.imul(f1[/* numerador */0], f2[/* numerador */0]);
  var denominador = Caml_int32.imul(f1[/* denominador */1], f2[/* denominador */1]);
  return crearFraccion(numerador, denominador);
}

function $slash$slash(f1, f2) {
  var numerador = Caml_int32.imul(f1[/* numerador */0], f2[/* denominador */1]);
  var denominador = Caml_int32.imul(f1[/* denominador */1], f2[/* numerador */0]);
  return crearFraccion(numerador, denominador);
}

function string_of_frac(f) {
  return String(f[/* numerador */0]) + ("/" + String(f[/* denominador */1]));
}

var operador = ParserC$ParserLibrary.$less$at$great((function (param) {
        switch (param) {
          case "*" : 
              return /* Times */2;
          case "+" : 
              return /* Plus */0;
          case "-" : 
              return /* Minus */1;
          default:
            return /* DividedBy */3;
        }
      }), ParserC$ParserLibrary.$neg$less$less(ParserC$ParserLibrary.$neg$great$great(ParserC$ParserLibrary.whitespaces, ParserC$ParserLibrary.parserAnyOf("/*-+")), ParserC$ParserLibrary.whitespaces));

var parseFrac = ParserC$ParserLibrary.$less$at$great((function (param) {
        return crearFraccion(param[0], param[1]);
      }), ParserC$ParserLibrary.$great$neg$great(ParserC$ParserLibrary.$neg$less$less(ParserC$ParserLibrary.intP, ParserC$ParserLibrary.parseChar("/")), ParserC$ParserLibrary.intP));

var operaFrac = ParserC$ParserLibrary.$less$at$great((function (param) {
        var f2 = param[1];
        var match = param[0];
        var f1 = match[0];
        switch (match[1]) {
          case 0 : 
              return $plus$slash(f1, f2);
          case 1 : 
              return $neg$slash(f1, f2);
          case 2 : 
              return $star$slash(f1, f2);
          case 3 : 
              return $slash$slash(f1, f2);
          
        }
      }), ParserC$ParserLibrary.$great$neg$great(ParserC$ParserLibrary.$great$neg$great(parseFrac, operador), parseFrac));

ParserC$ParserLibrary.run(ParserC$ParserLibrary.$less$at$great(string_of_frac, operaFrac), "8/5+2/4", (function (param) {
        return ParserC$ParserLibrary.$neg$pipe((function (param) {
                      return param[0];
                    }), (function (prim) {
                      console.log(prim);
                      return /* () */0;
                    }), param);
      }));

var Fraccionario = /* module */[
  /* mcd */mcd,
  /* mcm */mcm,
  /* crearFraccion */crearFraccion,
  /* +/ */$plus$slash,
  /* -/ */$neg$slash,
  /* */ */$star$slash,
  /* // */$slash$slash,
  /* string_of_frac */string_of_frac,
  /* operador */operador,
  /* parseFrac */parseFrac,
  /* operaFrac */operaFrac
];

exports.Fraccionario = Fraccionario;
/* operador Not a pure module */