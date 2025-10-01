use std::fs;
use clap::{Arg, Parser as otherParser};
use crate::{lexer::lexer, parser::Parser};

mod tokens;
mod lexer;
mod parser;
mod error_handler;

#[derive(otherParser, Debug)]
#[command(author, version, about)]
struct Args {
    #[arg(short, long)]
    file: Option<String>,
}

fn main() {
    let args = Args::parse();
    if let Some(file_path) = args.file {
        run_file(&file_path);
    } else {
        run();
    }   
    
}

fn run_file(path: &str) {
    let content = match fs::read_to_string(path) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("Dosya okunamadı: {}", e);
            return;
        }
    };

    let tokens = match lexer(&content) {
        Ok(t) => t,
        Err(err) => {
            eprintln!("Lexer error: {}", err);
            return;
        }
    };

    let mut parser = Parser::new(tokens);
    while let Some(_) = parser.current_token() {
        if let Err(err) = parser.sense() {
            eprintln!("Parser error: {:?}", err);
            break;
        }
    }
}

fn run() {
    let content = "let string message = \"hello world\" let int num = 1 + 1 print message print num";
    let tokens = match lexer(&content) {
        Ok(tokens) => {
            tokens
        }
        Err(err) => {
            eprintln!("Lexer error: {}", err);
            return;
        }
    };
    println!("{:?}", tokens);

    let mut parser = Parser::new(tokens);

    while parser.current_token().is_some() {
        match parser.sense() {
            Ok(_) => (),
            Err(err) => {
                eprintln!("Parser error: {:?}", err);
                break;
            }
        }
    }
}