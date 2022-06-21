#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(dead_code)]
#![allow(unused_parens)]
#![allow(unused_variables)]
#![allow(unused_must_use)]

use core::fmt;
use std::collections::HashMap;
use Expression::*;

#[derive(Debug, Clone, PartialEq)]
enum Expression {

    symbol(String),
    operation(String, Vec<Expression>)
}

impl fmt::Display for Expression {

    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match self {
            symbol(name) => write!(formatter, "{ }", name),

            operation(name, arguments) => {
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

fn matchPattern(referenceExpression: &Expression, providedExpression: &Expression) -> Option<HashMap<String, Expression>> {

    fn recursiveHelper(referenceExpression: &Expression, providedExpression: &Expression, bindings: &mut HashMap<String, Expression>) -> bool {
        match (referenceExpression, providedExpression) {

            (symbol(name), _) => {
                if let Some(existingSymbolValue)= bindings.get(name) {
                    return existingSymbolValue == providedExpression;
                }

                else {
                    bindings.insert(name.clone( ), providedExpression.clone( ));

                    return true;
                }
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

            _ => false
        }
    }

    let mut bindings: HashMap<String, Expression>= HashMap::new( );

    if(recursiveHelper(referenceExpression, providedExpression, &mut bindings)) { return Some(bindings); }

    else { return None; }
}

impl AxiomEquation {

    fn useAxiom(&self, providedExpression: &Expression) -> Expression {
        if let Some(bindings)= matchPattern(&self.left, providedExpression) {

            fn performReplacements(bindings: &HashMap<String, Expression>, axiomRHS: &Expression) -> Expression {
                match axiomRHS {

                    symbol(name) => {
                        if let Some(replacementSymbol)= bindings.get(name) {
                            return replacementSymbol.clone( );
                        }

                        else { return axiomRHS.clone( ); }
                    },

                    operation(referenceOperationName, referenceArguments) => {
                        let mut argumentsWithReplacements= Vec::new( );

                        for argument in referenceArguments {
                            argumentsWithReplacements.push(performReplacements(bindings, argument));
                        }

                        return operation(referenceOperationName.clone( ), argumentsWithReplacements);
                    }
                }
            }

            return performReplacements(&bindings, &self.right);
        }

        else {
            match providedExpression {
                symbol(_) => providedExpression.clone( ),

                operation(operationName, operationArguments) => {
                    let mut evaluatedArguments= Vec::new( );

                    for argument in operationArguments {
                        evaluatedArguments.push(self.useAxiom(argument));
                    }

                    return operation(operationName.clone( ), evaluatedArguments);
                }
            }
        }
    }
}

#[derive(Debug)]
enum TokenCategories {

    symbol,
    openParanthesis,
    closeParanthesis,
    comma,
    equals
}

#[derive(Debug)]
struct Token {

    category: TokenCategories,
    asString: String
}

struct Lexer<CharactersIterator: Iterator<Item= char>> {
    charactersIterator: CharactersIterator
}

impl<CharactersIterator: Iterator<Item = char>> Iterator for Lexer<CharactersIterator> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        todo!( );
    }
}

impl<CharactersIterator: Iterator<Item = char>> Lexer<CharactersIterator> {
    fn constructor(charactersIterator: CharactersIterator) -> Self {
        return Lexer { charactersIterator: charactersIterator };
    }
}

fn main( ) {
    // TODO: creating the replacement engine

    for token in Lexer::constructor("swap(pair(a, b))".chars( )) {
        println!("{:?}", token);
    }
}