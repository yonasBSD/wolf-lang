use std::{collections::HashMap};

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

        let mut declared_type: Option<Token> = None;

        // After `let` → expect type (number/string/bool)
        if let Some(next) = self.current_token() {
            match next {
                Token::TypeNumber | Token::TypeString | Token::TypeBool => {
                    declared_type = Some(next.clone());
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

        if let Some(decl_ty) = declared_type.as_ref() {
            // --- NEW LOGIC: Check for mathematical expressions ---
            if *decl_ty == Token::TypeNumber {
                match self.parse_expr() {
                    Ok(result) => {
                        // If parse_expr succeeds, the value is the calculated result.
                        assigned_value = Some(Token::Number(result));
                        self.output.push(format!("value (expression result) {}", result));
                        // NOTE: parse_expr correctly advances self.pos, so no manual pos += 1 here.
                    }
                    // If the expression parser fails (e.g., unexpected token), propagate the error
                    Err(e) => return Err(e),
                }
            } 
            // --- OLD LOGIC: Handle String and Boolean literals ---
            else if let Some(next) = self.current_token() {
                match next {
                    // We removed Token::Number(n) check here since parse_expr handles it now.
                    
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
                            expected: Token::String("literal or expression".to_string()),
                            found: Some(next.clone()),
                        })
                    }
                }
            } else {
                return Err(ParseError::UnexpectedToken {
                    expected: Token::String("literal or expression".to_string()),
                    found: None,
                })
            }
        }
    

        if let (Some(decl_ty), Some(val_tok)) = (declared_type.as_ref(), assigned_value.as_ref()) {
            let matches = match (decl_ty, val_tok) {
                (Token::TypeBool, Token::Boolean(_)) => true,
                (Token::TypeNumber, Token::Number(_)) => true,
                (Token::TypeString, Token::String(_)) => true,

                _ => false
            };

            if !matches {
                return Err(ParseError::TypeMismatch {
                    expected: decl_ty.clone(),
                    found: val_tok.clone(),
                });
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

    fn parse_expr(&mut self) -> Result<f64, ParseError> {
        let mut result = self.parse_term()?; 

        while let Some(tok) = self.current_token() {
            match tok {
                Token::Plus => {
                    self.eat(Token::Plus)?;
                    result += self.parse_term()?;
                }
                Token::Minus => {
                    self.eat(Token::Minus)?;
                    result -= self.parse_term()?;
                }
                _ => break,
            }
        }
        Ok(result)
    }

    fn parse_term(&mut self) -> Result<f64, ParseError> {
        let mut result = self.parse_factor()?; 

        while let Some(tok) = self.current_token() {
            match tok {
                Token::Multiply => {
                    self.eat(Token::Multiply)?;
                    result *= self.parse_factor()?;
                }
                Token::Divide => {
                    self.eat(Token::Divide)?;
                    result /= self.parse_factor()?;
                }
                _ => break
            }
        }
        Ok(result)
    }

    fn parse_factor(&mut self) -> Result<f64, ParseError> {
        let tok = match self.current_token().cloned() {
            Some(t) => t,
            None => {
                return Err(ParseError::UnexpectedToken {
                    expected: Token::Number(0.0), // Expected something, found None
                    found: None,
                });
            }
        };

        match tok {
            Token::Number(n) => {
                self.eat(Token::Number(n))?; 
                Ok(n)
            }

            Token::Plus => {
                self.eat(Token::Plus)?;
                return self.parse_factor()
            }
            Token::Minus => {
                self.eat(Token::Minus)?;
                let value = self.parse_factor()?;
                return Ok(-value)
            }

            Token::LParen => {
                self.eat(Token::LParen)?;
                let result = self.parse_expr()?;
                self.eat(Token::RParen)?;
                return Ok(result)
            }

            Token::Identifier(name) => {
                self.eat(Token::Identifier(name.clone()))?;
                if let Some(value_token) = self.symbol_table.get(&name) {
                    if let Token::Number(n) =  value_token{
                        return Ok(*n)
                    } else {
                        return Err(ParseError::TypeMismatch {
                            expected: Token::TypeNumber,
                            found: value_token.clone(),
                        })
                    }
                } else {
                    return Err(ParseError::UndeclaredVariable { name })
                }

            }

            _ => {
                Err(ParseError::UnexpectedToken {
                    expected: Token::Number(0.0),
                    found: Some(tok),
                })
            }
        }
       
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