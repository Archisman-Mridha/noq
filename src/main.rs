#![allow(unused_variables)]
#![allow(non_snake_case)]
#![allow(dead_code)]

mod lexer;

use std::{fmt::{self}, collections::HashMap, iter::Peekable, io::{stdin, stdout, Write, self}, fs};
use Expression::*;
use lexer::*;

// Clone trait allows us to perform deep copies
#[derive(Debug, Clone, PartialEq)] /*
    * generates an implementation of the Debug trait for an enum or 
    This generated implementation contains methods which help in printing values of the struct or enum */
enum Expression {

    Symbol(String),

    FunctionInvocation(String, Vec<Expression>)
}

impl Expression {
    fn parse(lexer: &mut Peekable<impl Iterator<Item= Token>>) -> Result<Self, Error> {
        if let Some(token)= lexer.next( ) {
            match token._type {

                TokenTypes::Symbol => {
                    if let Some(_)= lexer.next_if(|token| token._type == TokenTypes::OpenParanthesis) {
                        let mut parsedArguments= Vec::new( );

                        // in case of no arguments
                        if let Some(_)= lexer.next_if(|token| token._type == TokenTypes::CloseParanthesis) {
                            return Ok(Expression::FunctionInvocation(token.asString, parsedArguments));}

                        parsedArguments.push(Expression::parse(lexer)?);
                        while let Some(_)= lexer.next_if(|token| token._type == TokenTypes::Comma) {
                            parsedArguments.push(Expression::parse(lexer)?);
                        }

                        if lexer.next_if(|token| token._type == TokenTypes::CloseParanthesis).is_none( ) {
                            return Err(Error::UnexpectedToken(TokenTypes::CloseParanthesis, token));}

                        return Ok(Expression::FunctionInvocation(token.asString, parsedArguments));
    
                    } else {
                        return Ok(Expression::Symbol(token.asString));
                    }
                }

                _ => return Err(Error::UnexpectedToken(TokenTypes::Symbol, token))
            }

        } else {
            todo!("ERROR: no tokens left in lexer to parse")
        }
    }
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

impl Rule {
    fn parse(lexer: &mut Peekable<impl Iterator<Item= Token>>) -> Result<Rule, Error> {
        let head= Expression::parse(lexer)?;
        expectTokenType(lexer, TokenTypes::Equals)?;
        let body= Expression::parse(lexer)?;

        return Ok(Rule { head, body });
    }

    fn apply(&self, expression: &Expression) -> Expression {

        if let Some(bindings)= performPatternMatching(&expression, &self.head) {
            applyBindings(&bindings, &self.body)}
        else {
            match expression {
                Symbol(_) => expression.clone( ),

                FunctionInvocation(name, arguments) => {
                    let mut transformedArguments= Vec::new( );

                    for argument in arguments {
                        transformedArguments.push(self.apply(argument));}

                    return FunctionInvocation(name.clone( ), transformedArguments);
                }
            }
        }
    }
}

fn performPatternMatching(pattern: &Expression, reference: &Expression) -> Option<Bindings> {
    let mut bindings: Bindings= HashMap::new( );

    fn recursionFn(pattern: &Expression, reference: &Expression, bindings: &mut Bindings) -> bool {
        match(reference, pattern) {
    
            (Symbol(name), _) => {
                if let Some(value)= bindings.get(name) {
                    return value == pattern;}
                else {
                    bindings.insert(name.clone( ), pattern.clone( ));

                    return true;
                }
            },
    
            (FunctionInvocation(nameInReference, argumentsInReference), FunctionInvocation(nameInPattern, argumentsInPattern)) => {

                if nameInPattern == nameInReference && argumentsInPattern.len( ) == argumentsInReference.len( ) {

                    for i in 0..argumentsInPattern.len( ) {
                        if !recursionFn(&argumentsInPattern[i], &argumentsInReference[i], bindings) {
                            return false;}
                    }

                    return true;

                } else { return false; }
            },
    
            _ => false
        }
    }

    if recursionFn(pattern, reference, &mut bindings) {
        return Some(bindings)}
    else { return None }
}

fn expectTokenType(lexer: &mut Peekable<impl Iterator<Item= Token>>, expectedTokenType: TokenTypes) -> Result<Token, Error> {
    let token= lexer.next( ).expect("no tokens left to lex");

    if token._type != expectedTokenType {
        return Err(Error::UnexpectedToken(expectedTokenType, token));
    }

    return Ok(token);
}

type Bindings= HashMap<String, Expression>;

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
                argumentsAfterApplyingBindings.push(applyBindings(bindings, argument));}

            return FunctionInvocation(name.clone( ), argumentsAfterApplyingBindings);
        }
    }
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

    // function
    ($name:ident($($arguments:tt)*)) => {
        FunctionInvocation(stringify!($name).to_string( ), functionArguments!($($arguments)*))
    };
}

fn parseRulesFromFile(filePath: &str) -> Result<HashMap<String, Rule>, Error> {
    let mut rules= HashMap::new( );

    let sourcecode= fs::read_to_string(filePath)
        .map_err(|error| Error::IO(error))?;

    let mut lexer= Lexer::constructLexerForSourcecode(sourcecode.chars( ));
    lexer.setFilePath(filePath);
    let mut lexer= lexer.peekable( );

    while let Some(token)= lexer.peek( ) {
        if token._type == TokenTypes::EOF { break; }

        expectTokenType(&mut lexer, TokenTypes::Rule)?;
        let ruleName= expectTokenType(&mut lexer, TokenTypes::Symbol)?.asString;
        let rule= Rule::parse(&mut lexer)?;

        rules.insert(ruleName, rule);
    }

    return Ok(rules);
}

fn main( ) {

    let defaultRulesFilePath= "rules.default.noq";
    let defaultRules: HashMap<String, Rule>= match parseRulesFromFile(defaultRulesFilePath) {

        Ok(defaultRules) => {
            println!("INFO: successfully loaded default rules from {}", defaultRulesFilePath);
            defaultRules
        }

        Err(Error::IO(error)) => {
            eprintln!("ERROR: could not read file {}, {:?}", defaultRulesFilePath, error);
            Default::default( )
        }

        Err(Error::UnexpectedToken(expectedTokenType, actualToken)) => {
            eprintln!("ERROR: expected {:?} but got {:?} '{}'", expectedTokenType, actualToken._type, actualToken.asString);
            Default::default( )
        }
    };

    println!("INFO: available rules : ");
    for (defaultRuleName, defaultRule) in defaultRules {
        println!("INFO: `{}` : {}", defaultRuleName, defaultRule);}

    let swapRule= Rule {
        head: expression!(swap(pair(a, b))),
        body: expression!(pair(b, a))
    };

    let mut input= String::new( );

    loop {
        input.clear( );
        print!("> ");
        stdout( ).flush( ).unwrap( );

        stdin( ).read_line(&mut input).unwrap( );

        let lexer= Lexer::constructLexerForSourcecode(input.chars( ));
        let parsedExpression= Expression::parse(&mut lexer.peekable( ));
        match parsedExpression {

            Ok(parsedExpression) => {
                println!("  original expression - {}", parsedExpression);

                let transformedExpression= swapRule.apply(&parsedExpression);
                println!("  transformed expression - {}", transformedExpression);
            },

            Err(Error::UnexpectedToken(expectedTokenType, actualToken)) => {
                eprintln!("{:>width$}^", "", width= 2 + actualToken.location.columnNumber);
                eprintln!("ERROR: expected {:?} but got {:?} '{}'", expectedTokenType, actualToken._type, actualToken.asString);
            },

            _ => todo!("ERROR: unhandled error occured during parsing expression")
        }
    }
}

#[derive(Debug)]
enum Error {
    UnexpectedToken(TokenTypes, Token),
    IO(io::Error)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn testSwapRule( ) {

        // the rule - swap(pair(a, b))= pair(b, a)
        let swapRule= Rule {
            head: expression!(swap(pair(a, b))),
            body: expression!(pair(b, a))
        };

        let sourcecode= "foo(swap(pair(f(a), g(b))), swap(pair(p(r), q(s))))";

        let lexer= Lexer::constructLexerForSourcecode(sourcecode.chars( ));
        let parsedExpression= Expression::parse(&mut lexer.peekable( )).unwrap( );

        let expectedTransformedExpression= expression!(foo(pair(g(b), f(a)), pair(q(s), p(r))));
        let transformedExpression= swapRule.apply(&parsedExpression);

        assert_eq!(expectedTransformedExpression, transformedExpression);
    }
}