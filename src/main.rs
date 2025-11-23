use std::{fs, io::{self, BufRead, Read}};
use clap::{Arg, Parser as otherParser};
use crate::{lexer::lexer, parser::Parser};

mod tokens;
mod lexer;
mod parser;
mod error_handler;
mod stdlib;


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
    println!(r#"                                        
               ##    #                  
              #### ###                  
             ##  ##  #                  
             ##   ###                   
            ###  ########               
           #################            
           ###################          
        #################  ###%         
         ##########################     
        #############################   
      ##%###########################    
        ###########################     
      ######################            
     #####################              
      ###################               
        ################                
          #############                 
             #########                  
               ######                   
                 ###                    
                                        
"#);
    println!("   W O L F L A N G  v0.1");
    println!("---------------------------");
    let mut lines: Vec<String> = Vec::new();
    let mut content;
    loop {
        let stdin = io::stdin();
        let mut buffer: String = String::new();
        for line in stdin.lock().lines() {
            let line = line.expect("read error");

            if line.trim() == "run()" {
                // for döngüsünü bitir
                break;
            }

            lines.push(line);
        }

        content = lines.join("");
        break;
    }
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