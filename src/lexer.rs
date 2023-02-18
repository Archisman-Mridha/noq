use std::{iter::Peekable, fmt};

#[derive(Debug, PartialEq)]
pub enum TokenTypes {

    Symbol(String),

    // keywords
    Rule,
    Shape,
    Apply,
    Done,

    // special characters
    OpenParanthesis,
    CloseParanthesis,
    Comma,
    Equals,

    // terminators
    Invalid,
    EOF
}

fn getKeywordForString(string: &str) -> Option<TokenTypes> {
    match string {

        "rule" => Some(TokenTypes::Rule),
        "shape" => Some(TokenTypes::Shape),
        "apply" => Some(TokenTypes::Apply),
        "done" => Some(TokenTypes::Done),

        _ => None
    }
}

#[derive(Debug, PartialEq)]
pub struct TokenLocation {
    pub filePath: Option<String>,

    pub rowNumber: usize,
    pub columnNumber: usize
}

impl fmt::Display for TokenLocation {

    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.rowNumber, self.columnNumber)
    }
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub _type: TokenTypes,
    pub asString: String,
    pub location: TokenLocation
}

//* sourcecode is an iterator where the item type is 'char'
//* and lexer is an iterator over the parsed tokens
pub struct Lexer<Sourcecode: Iterator<Item= char>> {
    sourcecode: Peekable<Sourcecode>,

    filePath: Option<String>,
    currentRowNumber: usize,
    currentColumnNumber: usize,

    stopLexing: bool
}

impl<Sourcecode: Iterator<Item= char>> Lexer<Sourcecode> {

    pub fn constructLexerForSourcecode(sourcecode: Sourcecode) -> Self {
        return Self {
            sourcecode: sourcecode.peekable( ),

            filePath: None,
            currentRowNumber: 0,
            currentColumnNumber: 0,

            stopLexing: false
        };
    }

    fn getCurrentLocation(&self) -> TokenLocation {
        return TokenLocation {
            filePath: self.filePath.clone( ),
            
            rowNumber: self.currentRowNumber,
            columnNumber: self.currentColumnNumber
        };
    }

    pub fn setFilePathForLexer(&mut self, filePath: &str) {
        self.filePath= Some(filePath.to_string( ));
    }
}

impl<Sourcecode: Iterator<Item= char>> Iterator for Lexer<Sourcecode> {
    type Item= Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.stopLexing { return None; }

        // ignoring whitespaces
        while let Some(character)= self.sourcecode.next_if(|character| character.is_whitespace( )) {
            self.currentColumnNumber += 1;

            if character == '\n' {
                self.currentRowNumber += 1;
            }
        }

        let currentLocation= self.getCurrentLocation( );

        match self.sourcecode.next( ) {

            Some(character) => {
                let mut asString= String::new( );
                asString.push(character);

                match character {

                    '(' => Some(Token {
                        _type: TokenTypes::OpenParanthesis,
                        asString,
                        location: currentLocation }),

                    ')' => Some(Token {
                        _type: TokenTypes::CloseParanthesis,
                        asString,
                        location: currentLocation }),

                    ',' => Some(Token {
                        _type: TokenTypes::Comma,
                        asString,
                        location: currentLocation }),

                    '=' => Some(Token {
                        _type: TokenTypes::Equals,
                        asString,
                        location: currentLocation }),

                    _ => {
                        if !character.is_alphanumeric( ) {
                            self.stopLexing= true;

                            return Some(Token {
                                _type: TokenTypes::Invalid,
                                asString,
                                location: currentLocation })
                        } else {

                            while let Some(character)= self.sourcecode.next_if(|character| character.is_alphanumeric( )) {
                                self.currentColumnNumber += 1;
                                asString.push(character)
                            }

                            if let Some(tokenType)= getKeywordForString(&asString) {
                                return Some(Token {
                                    _type: tokenType,
                                    asString,
                                    location: currentLocation });
                            } else {
                                return Some(Token {
                                    _type: TokenTypes::Symbol(asString.clone( )),
                                    asString,
                                    location: currentLocation });
                            }
                        }
                    }
                }
            }

            None => {
                self.stopLexing= true;

                return Some(Token {
                    _type: TokenTypes::EOF,
                    asString: "".to_string( ),
                    location: currentLocation });
            }
        }
    }
}