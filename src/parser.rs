use std::{collections::HashMap, error};

use crate::{error_handler::ParseError, tokens::{self, Token}};

#[derive(Debug, Clone, PartialEq)]

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    output: Vec<String>,
    symbol_table: HashMap<String, Token>,
    
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            pos: 0,
            output: Vec::new(),
            symbol_table: HashMap::new(),
        }
    }

    pub fn current_token(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    pub fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos + 1)
    }

    fn eat(&mut self, token_type: Token) -> Result<(), ParseError> {
        if let Some(tok) = self.current_token() {
            if *tok == token_type {
                self.pos += 1;
                Ok(())
            }
            else {
                Err(ParseError::UnexpectedToken {
                    expected: token_type,
                    found: Some(tok.clone()),
                })
            }
        }
        else {
            Err(ParseError::UnexpectedToken {
                expected: token_type,
                found: None,
            })
        }
    }

    fn handle_token(&mut self, token: Token, name: &str) -> Result<(), ParseError> {
        self.output.push(name.to_string());
        self.eat(token)
    }
    
    pub fn parse_let(&mut self) -> Result<(), ParseError> {
        self.eat(Token::Let)?;

        let mut var_name: Option<String> = None;
        let mut assigned_value: Option<Token> = None;

        // After `let` → expect type (number/string/bool)
        if let Some(next) = self.current_token() {
            match next {
                Token::TypeNumber | Token::TypeString | Token::TypeBool => {
                    let ty = format!("{:?}", next);
                    self.output.push(format!("type {}", ty));
                    self.pos += 1; // consume type
                }
                _ => {
                    return Err(ParseError::UnexpectedToken {
                        expected: Token::TypeNumber, // just a placeholder
                        found: Some(next.clone()),
                    })
                }
            }
        } else {
            return Err(ParseError::UnexpectedToken {
                expected: Token::TypeNumber,
                found: None,
            });
        }

        if let Some(next) = self.current_token() {
            match next {
                Token::Identifier(name) => {
                    var_name = Some(name.clone());
                    self.output.push(format!("var {}", name));
                    self.pos += 1;
                }
                _ => {
                    return Err(ParseError::UnexpectedToken {
                        expected: Token::Identifier("x".to_string()),
                        found: Some(next.clone()),
                    })
                }
            }
        } else {
            return Err(ParseError::UnexpectedToken {
                expected: Token::Identifier("x".to_string()),
                found: None,
            })
        }

        // After identifier → expect '='
        self.eat(Token::Assign)?;

        if let Some(next) = self.current_token()  {
            match next {
                Token::Number(n) => {
                    assigned_value = Some(next.clone());
                    self.output.push(format!("value {}", n));
                    self.pos += 1;
                }

                Token::String(s) => {
                    assigned_value = Some(next.clone());
                    self.output.push(format!("value {}", s));
                    self.pos += 1;
                }

                Token::Boolean(b) => {
                    assigned_value = Some(next.clone());
                    self.output.push(format!("value {}", b));
                    self.pos += 1;
                }

                _ => {
                    return Err(ParseError::UnexpectedToken {
                        expected: Token::Number(0.0),
                        found: Some(next.clone()),
                    })
                }
            }
        }
        if let (Some(name), Some(value)) = (var_name, assigned_value) {
            self.symbol_table.insert(name, value);
            println!("{:?}", self.symbol_table);
        }
        Ok(())
    }

    fn parse_print(&mut self) -> Result<(), ParseError> {
        self.eat(Token::Print)?;
        if let Some(next) = self.current_token() {
            match next {
                Token::String(s) => {
                    println!("{:?}", s);
                    self.output.push(format!("print value: {}", s));
                    self.pos += 1;
                }

                Token::Number(n) => {
                    println!("{:?}", n);
                    self.output.push(format!("print value: {}", n));
                    self.pos += 1;
                }

                Token::Boolean(b) => {
                    println!("{:?}", b);
                    self.output.push(format!("print value: {}", b));
                    self.pos += 1;
                }

                Token::Identifier(name) => {
                    if let Some(value_token) = self.symbol_table.get(name)  {
                        println!("{:?}", value_token);
                        self.output.push(format!("print var: {} = {:?}", name, value_token));
                        self.pos += 1;
                    }
                    else {
                        return Err(ParseError::UndeclaredVariable{name: name.to_string()})
                    }
                }

                _ => {
                    return Err(ParseError::UnexpectedToken {
                        expected: Token::Number(0.0),
                        found: Some(next.clone()),
                    })
                }
            }
            
        }
        else {
            // End of tokens after 'print'
            return Err(ParseError::UnexpectedToken {
                expected: Token::String("".to_string()),
                found: None,
            })
        }
        Ok(())
    }

    pub fn sense(&mut self) -> Result<(), ParseError> {
        if let Some(tok) = self.current_token() {
                match tok {
                    Token::Let => self.parse_let(),
                    Token::Print => self.parse_print(),
                    _ => {
                        let name = format!("{:?}", tok);
                        self.output.push("unknown".to_string());
                        Err(ParseError::UnkownType { type_name: name })
                    }
                }
        } else {
            Ok(())
        }
    }
    
}