use crate::tokens::Token;

pub enum Expr {
    Literal(i64),
    Binary {            
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },
    Grouping(Box<Expr>),
}

pub enum Stmt {
    Let {
        name: String,
        data_type: Token,
        value: Expr
    },
    Print(Expr),
    Block(Vec<Stmt>),
}