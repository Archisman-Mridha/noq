#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(dead_code)]
#![allow(unused_parens)]
#![allow(unused_variables)]

use core::fmt;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
enum Expression {

    symbol(String),
    operation(String, Vec<Expression>)
}

impl fmt::Display for Expression {

    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::symbol(name) => write!(formatter, "{ }", name),

            Expression::operation(name, arguments) => {
                write!(formatter, "{ }(", name);

                for (index, argument) in arguments.iter( ).enumerate( ) {
                    if(index > 0) { write!(formatter, ", "); }

                    write!(formatter, "{ }", argument);
                }

                return write!(formatter, ")");
            }
        }
    }
}

#[derive(Debug)]
struct AxiomEquation {

    left: Expression,
    right: Expression
}

impl fmt::Display for AxiomEquation {

    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        return write!(formatter, "{ } = { }", self.left, self.right);
    }
}

impl AxiomEquation {

    fn useAxiomEquationForExpression(&self, expression: Expression) -> Expression {
        todo!( );
    }
}

fn matchPattern(referenceExpression: &Expression, providedExpression: &Expression) -> Option<HashMap<String, Expression>> {

    fn recursiveHelper(referenceExpression: &Expression, providedExpression: &Expression, bindings: &mut HashMap<String, Expression>) -> bool {
        use Expression::*;

        match (referenceExpression, providedExpression) {

            (symbol(name), _) => {
                if let Some(existingSymbolValue)= bindings.get(name) {
                    return existingSymbolValue == providedExpression;
                }

                else { bindings.insert(name.clone( ), providedExpression.clone( )); }

                return true;
            },
    
            (operation(referenceExpressionName, referenceExpressionArguments), operation(providedExpressionName, providedExpressionArguments)) => {
                if(referenceExpressionName == providedExpressionName && referenceExpressionArguments.len( ) == providedExpressionArguments.len( )) {

                    for index in 0..referenceExpressionArguments.len( ) {
                        if(! recursiveHelper(&referenceExpressionArguments[index], &providedExpressionArguments[index], bindings)) {
                            return false;
                        }
                    }

                    return true;
                }

                else { return false; }
            },

            _ => { return false; }
        }
    }

    let mut bindings: HashMap<String, Expression>= HashMap::new( );

    if(recursiveHelper(referenceExpression, providedExpression, &mut bindings)) { return Some(bindings); }

    else { return None; }
}

fn main( ) {
    use Expression::*;

    // TODO: implement pattern matching

    let swapOperation= AxiomEquation {

        left: operation(
            String::from("swap"), vec![ operation(
                String::from("pair"), vec![ symbol(String::from("a")), symbol(String::from("b")) ]
            )
        ]),
        right: operation(String::from("pair"), vec![ symbol(String::from("b")), symbol(String::from("a")) ])
    };

    let referenceExpression= swapOperation.left;

    println!("{ }", referenceExpression);

    let providedExpression= operation(String::from("swap"), vec![
        operation(String::from("pair"), vec![
            operation(String::from("f"), vec![ symbol(String::from("g")) ]),
            operation(String::from("f"), vec![ symbol(String::from("h")) ])
        ])
    ]);

    println!("{ }", providedExpression);

    if let Some(bindings)= matchPattern(&referenceExpression, &providedExpression) {
        println!("pattern match successfull ! showing bindings : ");

        for (key, value) in bindings.iter( ) {
            println!("{ } : { }", key, value);
        }
    }

    else { println!("pattern match unsuccessfull"); }
}