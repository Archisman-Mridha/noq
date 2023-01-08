#![allow(unused_variables)]
#![allow(non_snake_case)]
#![allow(dead_code)]

use std::{fmt, collections::HashMap};
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
    
            (FunctionInvocation(nameInPattern, argumentsInPattern), FunctionInvocation(nameInReference, argumentsInReference)) => {

                if nameInPattern == nameInReference && argumentsInPattern.len( ) == argumentsInReference.len( ) {

                    for i in 0..argumentsInPattern.len( ) {
                        if !recursiveHelperForPatternMatching(&argumentsInPattern[i], &argumentsInReference[i], bindings) {
                            return false; }
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

fn main( ) {

    // the rule - swap(pair(a, b))= pair(b, a)
    let swapRule= Rule {
        head: FunctionInvocation("swap".to_string( ),
                                        vec![FunctionInvocation("pair".to_string( ),
                                                                    vec![Symbol("a".to_string( )), Symbol("b".to_string( ))])]),
        body: FunctionInvocation("pair".to_string( ),
                                        vec![Symbol("b".to_string( )), Symbol("a".to_string( ))])
    };

    let expression= FunctionInvocation("foo".to_string( ),
                                            vec![
                                                FunctionInvocation("swap".to_string( ),
                                                                        vec![FunctionInvocation("pair".to_string( ),
                                                                                                    vec![
                                                                                                        FunctionInvocation("f".to_string( ), vec![Symbol("a".to_string( ))]),
                                                                                                        FunctionInvocation("g".to_string( ), vec![Symbol("b".to_string( ))])])]),
                                                FunctionInvocation("swap".to_string( ),
                                                                        vec![FunctionInvocation("pair".to_string( ),
                                                                                                    vec![
                                                                                                        FunctionInvocation("q".to_string( ), vec![Symbol("c".to_string( ))]),
                                                                                                        FunctionInvocation("z".to_string( ), vec![Symbol("d".to_string( ))])])])
                                            ]);

    println!("rule - {}", swapRule);

    println!("original expression - {}", expression);
    println!("transformed expression - {}", swapRule.apply(&expression));
}