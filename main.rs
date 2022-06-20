#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(dead_code)]
#![allow(unused_parens)]
#![allow(unused_variables)]

#[derive(Debug)]
enum Expression {

    symbol(String),
    operation(String, Vec<Expression>)
}

impl Expression {

    fn prettyPrint(&self) {
        match self {
            Expression::symbol(name) => print!("{ }", name),

            Expression::operation(name, arguments) => {
                print!("{ }(", name);

                for (index, argument) in arguments.iter( ).enumerate( ) {
                    if(index > 0) { print!(", "); }

                    argument.prettyPrint( );
                }

                print!(")");
            }
        }
    }
}

#[derive(Debug)]
struct AxiomEquation {

    left: Expression,
    right: Expression
}

impl AxiomEquation {

    fn prettyPrint(&self) {
        self.left.prettyPrint( );

        print!(" = ");

        self.right.prettyPrint( );
    }
}

impl AxiomEquation {

    fn useAxiomEquationForExpression(&self, expression: Expression) -> Expression {
        todo!( );
    }
}

fn main( ) {
    use Expression::*;

    // TODO: swap(pair(a, b))= pair(b, a)

    let swapOperation= AxiomEquation {

        left: operation(String::from("swap"), vec![ operation(String::from("pair"), vec![ symbol(String::from("a")), symbol(String::from("b")) ]) ]),
        right: operation(String::from("pair"), vec![ symbol(String::from("b")), symbol(String::from("a")) ])
    };

    swapOperation.prettyPrint( );
}