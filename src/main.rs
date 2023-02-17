#![allow(unused_variables)]
#![allow(non_snake_case)]
#![allow(dead_code)]

use std::{fmt, collections::HashMap, iter::Peekable};
use Expression::*;

// the clone trait allows us to perform deep copies
#[derive(Debug, Clone, PartialEq)] /*
    * generates an implementation of the Debug trait for an enum or 
    This generated implementation contains methods which help in printing values of the struct or enum */
enum Expression {

    Symbol(String),

    // TODO: learn about `Vec` in detail
    FunctionInvocation(String, Vec<Expression>)
}

// fmt::Display is a trait. traits are similar to interfaces
impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {

            Symbol(name) => write!(f, "{}", name),

            FunctionInvocation(name, arguments) => {

                // `?` is used to propagate error upstream. So, if any error is encountered by write!( ) it will be returned immediately
                write!(f, "{}(", name)?;

                for(i, argument) in arguments.iter( ).enumerate( ) {
                    if i > 0 {
                        write!(f, ", ")?; }

                    write!(f, "{}", argument)?;
                }

                write!(f, ")")
            }
        }
    }
}

#[derive(Debug)]
struct Rule {
    head: Expression,
    body: Expression
}

impl fmt::Display for Rule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        
        write!(f, "{}= {}", self.head, self.body)
    }
}

fn applyBindings(bindings: &Bindings, expression: &Expression) -> Expression {
    match expression {

        Symbol(name) => {
            if let Some(value)= bindings.get(name) {
                return value.clone( );
            }

            return expression.clone( );
        },

        FunctionInvocation(name, arguments) => {

            let mut argumentsAfterApplyingBindings= Vec::new( );

            for argument in arguments {
                argumentsAfterApplyingBindings.push(applyBindings(bindings, argument));
            }

            return FunctionInvocation(name.clone( ), argumentsAfterApplyingBindings);
        }
    }
}

impl Rule {
    fn apply(&self, expression: &Expression) -> Expression {

        if let Some(bindings)= performPatternMatching(&expression, &self.head) {
            applyBindings(&bindings, &self.body)

        } else {
            match expression {
                Symbol(_) => expression.clone( ),

                FunctionInvocation(name, arguments) => {
                    let mut transformedArguments= Vec::new( );

                    for argument in arguments {
                        transformedArguments.push(self.apply(argument));
                    }

                    return FunctionInvocation(name.clone( ), transformedArguments);
                }
            }
        }
    }
}

type Bindings= HashMap<String, Expression>;

fn performPatternMatching(pattern: &Expression, reference: &Expression) -> Option<Bindings> {
    let mut bindings: Bindings= HashMap::new( );

    fn recursiveHelperForPatternMatching(pattern: &Expression, reference: &Expression, bindings: &mut Bindings) -> bool {
        match(reference, pattern) {
    
            (Symbol(name), _) => {
                if let Some(value)= bindings.get(name) {
                    return value == pattern;
                } else {
                    bindings.insert(name.clone( ), pattern.clone( ));

                    return true;
                }
            },
    
            (FunctionInvocation(nameInReference, argumentsInReference), FunctionInvocation(nameInPattern, argumentsInPattern)) => {

                if nameInPattern == nameInReference && argumentsInPattern.len( ) == argumentsInReference.len( ) {

                    for i in 0..argumentsInPattern.len( ) {
                        if !recursiveHelperForPatternMatching(&argumentsInPattern[i], &argumentsInReference[i], bindings) {
                            return false;}
                    }

                    return true;

                } else {
                    return false;}
            },
    
            _ => false
        }
    }

    if recursiveHelperForPatternMatching(pattern, reference, &mut bindings) {
        return Some(bindings) } else {

    return None }
}

macro_rules! functionArguments {

    // function( )
    ( ) => { vec![ ] };

    //function(a)
    ($name:ident) => { vec![expression!($name)] };

    // function(a, b, c)
    ($firstArgument: ident, $($remainingArguments:tt)*) => {
        {
            let mut parsedArguments= vec![expression!($firstArgument)];
            parsedArguments.append(&mut functionArguments!($($remainingArguments)*));

            parsedArguments
        }
    };

    // f(g(a))
    ($name: ident($($arguments: tt)*)) => {
        vec![expression!($name($($arguments)*))]
    };

    // f(g(a), h(b), c)
    ($name: ident($($arguments: tt)*), $($remainingArguments: tt)*) => {
        {
            let mut parsedArguments= vec![expression!($name($($arguments)*))];
            parsedArguments.append(&mut functionArguments!($($remainingArguments)*));

            parsedArguments
        }
    }
}

macro_rules! expression {

    ($name: ident) => {
        Symbol(stringify!($name).to_string( ))
    };

    ($name:ident(
        $($arguments:tt)* // unparsed function arguments
    )) => {
        FunctionInvocation(stringify!($name).to_string( ), functionArguments!($($arguments)*))
    };
}

#[derive(Debug)]
enum TokenTypes {

    Symbol(String),
    OpenParanthesis,
    CloseParanthesis,
    Comma,
    Equals
}

#[derive(Debug)]
struct Token {
    _type: TokenTypes,
    asString: String
}

//* sourcecode is an iterator where the item type is 'char'
//* and lexer is an iterator over the parsed tokens
struct Lexer<Sourcecode: Iterator<Item= char>> {
    sourcecode: Peekable<Sourcecode>
}

impl<Sourcecode: Iterator<Item= char>> Lexer<Sourcecode> {

    fn constructLexerForSourcecode(sourcecode: Sourcecode) -> Self {
        return Self { sourcecode: sourcecode.peekable( ) };
    }
}

impl<Sourcecode: Iterator<Item= char>> Iterator for Lexer<Sourcecode> {
    type Item= Token;

    fn next(&mut self) -> Option<Self::Item> {
        // ignoring whitespaces
        while let Some(_)= self.sourcecode.next_if(|character| character.is_whitespace( )) { }

        if let Some(character)= self.sourcecode.next( ) {

            let mut asString= String::new( );
            asString.push(character);

            match character {

                '(' => Some(Token {
                    _type: TokenTypes::OpenParanthesis,
                    asString
                }),

                ')' => Some(Token {
                    _type: TokenTypes::CloseParanthesis,
                    asString
                }),

                ',' => Some(Token {
                    _type: TokenTypes::Comma,
                    asString
                }),

                '=' => Some(Token {
                    _type: TokenTypes::Equals,
                    asString
                }),

                _ => {
                    if !character.is_alphanumeric( ) {
                        todo!("ERROR: unexpexted token `{}`", character)
                    }

                    while let Some(character)= self.sourcecode.next_if(|character| character.is_alphanumeric( )) {
                        asString.push(character)
                    }

                    return Some(Token {
                        _type: TokenTypes::Symbol(asString.clone( )),
                        asString
                    });
                }
            }

        } else { return None; }
    }
}

fn main( ) {

    // // the rule - swap(pair(a, b))= pair(b, a)
    // let swapRule= Rule {
    //     head: expression!(swap(pair(a, b))),
    //     body: expression!(pair(b, a))
    // };

    // // the expression - foo(swap(pair(f(a), g(b))), swap(pair(q(c), z(d))))
    // let expression= expression!{
    //     foo(swap(pair(f(a), g(b))), swap(pair(q(c), z(d))))
    // };

    // println!("rule - {}", swapRule);

    // println!("original expression - {}", expression);
    // println!("transformed expression - {}", swapRule.apply(&expression));

    let tokens: Vec<Token>= Lexer::constructLexerForSourcecode(
        "swap(pair(a, b))".chars( )
    ).collect( );

    for token in tokens {
        println!("{:?}", token);
    }
}