// Generated by BUCKLESCRIPT VERSION 4.0.8, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var $$String = require("bs-platform/lib/js/string.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Caml_format = require("bs-platform/lib/js/caml_format.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");

function run(parser, cadena) {
  return Curry._1(parser[0], cadena);
}

function parseChar(caracter) {
  var innerFn = function (cadena) {
    var match = cadena.length;
    if (match !== 0) {
      var car1 = $$String.sub(cadena, 0, 1);
      var resto = $$String.sub(cadena, 1, cadena.length - 1 | 0);
      if (car1 === caracter) {
        return /* Success */Block.__(0, [
                  caracter,
                  resto
                ]);
      } else {
        return /* Failure */Block.__(1, [
                  "Esperaba " + (String(caracter) + (" y obtuve " + (String(car1) + ""))),
                  cadena
                ]);
      }
    } else {
      return /* Failure */Block.__(1, [
                "Final de la cadena",
                cadena
              ]);
    }
  };
  return /* Parser */[innerFn];
}

function parseNotChar(caracter) {
  var innerFn = function (cadena) {
    var match = cadena.length;
    if (match !== 0) {
      var car1 = $$String.sub(cadena, 0, 1);
      var resto = $$String.sub(cadena, 1, cadena.length - 1 | 0);
      if (car1 !== caracter) {
        return /* Success */Block.__(0, [
                  car1,
                  resto
                ]);
      } else {
        return /* Failure */Block.__(1, [
                  "No Esperaba " + (String(caracter) + " "),
                  cadena
                ]);
      }
    } else {
      return /* Failure */Block.__(1, [
                "Final de la cadena",
                cadena
              ]);
    }
  };
  return /* Parser */[innerFn];
}

function parserAny(param) {
  var innerFn = function (cadena) {
    var match = cadena.length;
    if (match !== 0) {
      var car1 = $$String.sub(cadena, 0, 1);
      var resto = $$String.sub(cadena, 1, cadena.length - 1 | 0);
      return /* Success */Block.__(0, [
                car1,
                resto
              ]);
    } else {
      return /* Failure */Block.__(1, [
                "Final de la cadena",
                cadena
              ]);
    }
  };
  return /* Parser */[innerFn];
}

function parserOr(p1, p2) {
  var innerFn = function (cadena) {
    var match = cadena.length;
    if (match !== 0) {
      var s1 = run(p1, cadena);
      if (s1.tag) {
        return run(p2, cadena);
      } else {
        return s1;
      }
    } else {
      return /* Failure */Block.__(1, [
                "Final de la cadena",
                cadena
              ]);
    }
  };
  return /* Parser */[innerFn];
}

function parserAnd(p1, p2) {
  var innerFn = function (cadena) {
    var match = cadena.length;
    if (match !== 0) {
      var e1 = run(p1, cadena);
      if (e1.tag) {
        return e1;
      } else {
        var e1$1 = run(p2, e1[1]);
        if (e1$1.tag) {
          return e1$1;
        } else {
          return /* Success */Block.__(0, [
                    /* tuple */[
                      e1[0],
                      e1$1[0]
                    ],
                    e1$1[1]
                  ]);
        }
      }
    } else {
      return /* Failure */Block.__(1, [
                "Final de la cadena",
                cadena
              ]);
    }
  };
  return /* Parser */[innerFn];
}

function parserMap(fn, p) {
  var innerFn = function (cadena) {
    var match = cadena.length;
    if (match !== 0) {
      var e1 = run(p, cadena);
      if (e1.tag) {
        return e1;
      } else {
        return /* Success */Block.__(0, [
                  Curry._1(fn, e1[0]),
                  e1[1]
                ]);
      }
    } else {
      return /* Failure */Block.__(1, [
                "Final de la cadena",
                cadena
              ]);
    }
  };
  return /* Parser */[innerFn];
}

function parserReturn(valor) {
  var innerFn = function (cadena) {
    return /* Success */Block.__(0, [
              valor,
              cadena
            ]);
  };
  return /* Parser */[innerFn];
}

function parserApply(fP, xP) {
  return parserMap((function (param) {
                return Curry._1(param[0], param[1]);
              }), parserAnd(fP, xP));
}

function parserChoice(lista) {
  return List.fold_left(parserOr, List.hd(lista), List.tl(lista));
}

function parserAll(lista) {
  var concatResults = function (p1, p2) {
    return parserMap((function (param) {
                  return Pervasives.$at(param[0], param[1]);
                }), parserAnd(p1, p2));
  };
  var lista2 = List.map((function (param) {
          return parserMap((function (p1) {
                        return /* :: */[
                                p1,
                                /* [] */0
                              ];
                      }), param);
        }), lista);
  return List.fold_left(concatResults, List.hd(lista2), List.tl(lista2));
}

function parserAnyOf(cadena) {
  return parserChoice(List.map(parseChar, $$Array.to_list(cadena.split(""))));
}

function parseString(cadena) {
  return parserAll(List.map(parseChar, $$Array.to_list(cadena.split(""))));
}

function lift2(f, xP, yP) {
  return parserApply(parserApply(parserReturn(f), xP), yP);
}

function many(p) {
  var innerFn = function (cadena) {
    var match = run(p, cadena);
    if (match.tag) {
      return /* Success */Block.__(0, [
                /* [] */0,
                cadena
              ]);
    } else {
      var resto1 = match[1];
      var valor1 = match[0];
      var match$1 = run(many(p), resto1);
      if (match$1.tag) {
        return /* Success */Block.__(0, [
                  /* :: */[
                    valor1,
                    /* [] */0
                  ],
                  resto1
                ]);
      } else {
        return /* Success */Block.__(0, [
                  List.append(/* :: */[
                        valor1,
                        /* [] */0
                      ], match$1[0]),
                  match$1[1]
                ]);
      }
    }
  };
  return /* Parser */[innerFn];
}

function many1(p) {
  var innerFn = function (cadena) {
    var f = run(p, cadena);
    if (f.tag) {
      return f;
    } else {
      var resto1 = f[1];
      var valor1 = f[0];
      var match = run(many(p), resto1);
      if (match.tag) {
        return /* Success */Block.__(0, [
                  /* :: */[
                    valor1,
                    /* [] */0
                  ],
                  resto1
                ]);
      } else {
        return /* Success */Block.__(0, [
                  List.append(/* :: */[
                        valor1,
                        /* [] */0
                      ], match[0]),
                  match[1]
                ]);
      }
    }
  };
  return /* Parser */[innerFn];
}

function optional(p) {
  var innerFn = function (cadena) {
    var match = run(p, cadena);
    if (match.tag) {
      return /* Success */Block.__(0, [
                undefined,
                cadena
              ]);
    } else {
      return /* Success */Block.__(0, [
                Caml_option.some(match[0]),
                match[1]
              ]);
    }
  };
  return /* Parser */[innerFn];
}

function skip(p) {
  var innerFn = function (cadena) {
    var f = run(p, cadena);
    if (f.tag) {
      return f;
    } else {
      return /* Success */Block.__(0, [
                /* () */0,
                f[1]
              ]);
    }
  };
  return /* Parser */[innerFn];
}

function keepLeft(p1, p2) {
  return parserMap((function (param) {
                return param[0];
              }), parserAnd(p1, p2));
}

function keepRight(p1, p2) {
  return parserMap((function (param) {
                return param[1];
              }), parserAnd(p1, p2));
}

var digit = parserAnyOf("0123456789");

keepRight(digit, digit);

var digits = many1(digit);

var intP = parserMap((function (arreglo) {
        return Caml_format.caml_int_of_string($$Array.of_list(arreglo).join(""));
      }), digits);

var ParserC = /* module */[
  /* run */run,
  /* parseChar */parseChar,
  /* parseNotChar */parseNotChar,
  /* parserAny */parserAny,
  /* parserOr */parserOr,
  /* <|> */parserOr,
  /* parserAnd */parserAnd,
  /* >-> */parserAnd,
  /* parserMap */parserMap,
  /* <@> */parserMap,
  /* parserReturn */parserReturn,
  /* parserApply */parserApply,
  /* <*> */parserApply,
  /* parserChoice */parserChoice,
  /* parserAll */parserAll,
  /* parserAnyOf */parserAnyOf,
  /* parseString */parseString,
  /* lift2 */lift2,
  /* many */many,
  /* many1 */many1,
  /* optional */optional,
  /* skip */skip,
  /* keepLeft */keepLeft,
  /* keepRight */keepRight,
  /* -<< */keepLeft,
  /* ->> */keepRight,
  /* digit */digit,
  /* digits */digits,
  /* intP */intP
];

var miko = run(parserAnd(intP, optional(parseChar(";"))), "23;");

exports.ParserC = ParserC;
exports.miko = miko;
/* digit Not a pure module */