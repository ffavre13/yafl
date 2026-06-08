# People on the group
- Florian Favre
- Julien Roduit
- Guilherme Marques
- Carolina Oliveira Ribeiro

# Link to our project tag



# Tasks repartitions

| Parsing Tasks | Person(s)| File name | lines |
| - | - |  - | - |
**Conditionals and Bindings** (required)          | Florian Favre | src/main/scala/parser/Parser.scala | 134, 135, 198-212, 215-229 |
**Type abstractions** (required)                  | Florian Favre | src/main/scala/parser/Parser.scala | 136, 232-248 |
**Prefix terms** (required)                       | Florian Favre | src/main/scala/parser/Parser.scala | 84-87, 91-97 |
**Universal types** (required)                    | Florian Favre | src/main/scala/parser/Parser.scala | 324, 334-350 |
**Arrow types** (required)                        | Florian Favre | src/main/scala/parser/Parser.scala | 304-309, 312-318 |
**Parenthesized types** (required)                | Florian Favre | src/main/scala/parser/Parser.scala | 325, 353-361 |
**Type applications** (required)                  | Florian Favre | src/main/scala/parser/Parser.scala | 100-125 |
**Recursive abstractions** (required)             | Florian Favre | src/main/scala/parser/Parser.scala | 137, 251-265 |
**Multiple parameters and arguments** (optional)  | Florian Favre | src/main/scala/parser/Parser.scala | 285-301, 334-350, 232-248, 100-125 |


| Parsing Tasks | Person(s)| File name | lines |
| - | - |  - | - |
**Normalization** (optional)                          | Julien Roduit | src/main/scala/optimizer/Optimizer.scala | 14, 154-200 |
**Dead code elimination** (optional)                  | Not done |  |  |
**Constant propagation** (optional)                   | Not done | | |
**Inlining** (optional)                               | Carolina Oliveira Ribeiro, Guilherme Marques | src/main/scala/optimizer/Optimizer.scala <br> src/test/scala/Optimizer.scala | 33-38, 44-109 <br> 25-33|
**Common subexpression elimination** (optional, hard) | Not done |  |  |
**Loop unrolling** (optional, brutal)                 | Not done |  |  |


| Parsing Tasks | Person(s)| File name | lines |
| - | - |  - | - |
**Built-in arithmetic and comparison** (optional) | Carolina Oliveira Ribeiro | src/main/scala/emitter/Emitter.scala <br> src/main/scala/optimizer/Optimizer.scala <br> src/main/scala/syntax/InfixOperator.scala <br> src/test/scala/Optimizer.scala | 4-5,90-99 <br>  5, 119-154 <br> 7-8, 18-27 <br> 15-23|   
**Bindings** (optional)                           | Florian Favre | src/main/scala/emitter/Emitter.scala | 17, 24, 54-59, 73-75, 107-121 |
**Monomorphiation** (optional, hard)              | Not done |  |  |
**Closures** (optional, brutal)                   | Not done |  |  |