#![allow(dead_code, non_camel_case_types, unused_variables)]

/* == Includes == */
use std::{
    // Lexer includes;
    iter::Peekable,

    /* Parser includes (This would be used for the parser, specifically fetching the lbp & rbp)
     * collections::HashMap; 
    */

    // AST includes:
    borrow::Borrow,

    // REPL includes:
    io,
    io::Write,
};

/* == Lexer == */

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Token {
    Int(i32),
    Bop(char),
    Paren(char),

    End
}

impl Token {
    #[inline]
    fn is_bop(&self) -> bool {
        matches!(self, Token::Bop(_))
    }

    #[inline]
    fn get_binary_prec(&self) -> (u8, u8)  {
        match self {
            Token::Bop('+') | Token::Bop('-') => (1, 2),
            Token::Bop('*') | Token::Bop('/') => (3, 4),
            Token::Bop('^') => (6, 5),
            _ => panic!("Unexpected token {:?}. Expected: [+, -, *, /]", self),
        }
    }

    #[inline]
    fn get_unary_prec(&self) -> u8 {
        match self {
            Token::Bop('-') => 5,
            _ => panic!("Unexpected token {:?}. Expected: [+, -]", self),
        }
    }
}

type Token_stream = Vec<Token>;

fn is_balanced_expr(src: &str) -> bool {
    let mut stack: Vec<char> = Vec::new();

    for c in src.chars() {
        match c {
            '(' => { stack.push(c) },
            ')' => {
                if stack.pop().is_none() { return false; }
            }
            _ => {}
        }
    }

    stack.is_empty()
}

fn lex_number<T>(mut int: i32, src_iter: &mut Peekable<T>) -> i32
where T: Iterator<Item = char> {
    while let Some(next_int) = src_iter.next_if(|next_char| next_char.is_numeric()) {
        int = int * 10 + next_int.to_digit(10).unwrap() as i32;
    }

    int
}

struct Lexer {
    token_stream: Token_stream,
    token_stream_cursor: usize,
}

impl Lexer {
    fn new(src: &str) -> Self {
        Self {
            token_stream_cursor: 0,
            token_stream: Self::lex_src(src),
        }
    }

    fn lex_src<T>(src: T) -> Token_stream
    where T: AsRef<str> {
        let src = src.as_ref();

        assert_ne!(src.len(), 0, "Invalid src size!");
        assert_eq!(is_balanced_expr(src), true, "Imbalanced parentheses");

        let mut token_stream: Token_stream = Vec::new();
        let mut src_iter = src.chars()
                              .filter(|curr_char| !curr_char.is_whitespace())
                              .peekable();

        while let Some(&next_char) = src_iter.peek() {
            src_iter.next();

            match next_char {
                // Lexing all the concurrent integers in 'src'
                '0'..='9' => token_stream.push(Token::Int(
                        lex_number(
                            next_char.to_digit(10).unwrap() as i32,
                           &mut src_iter
                        )
                )),

                // Lexing binary operations:
                '+' | '-' | '*' | '/' | '^' => token_stream.push(Token::Bop(next_char)),

                // Lexing parentheses:
                '(' | ')' => token_stream.push(Token::Paren(next_char)),

                // Invalid token:
                _ => panic!("{} is an invalid token!", next_char)
            }
        }

        // End of token stream marker:
        token_stream.push(Token::End);

        token_stream
    }

    fn next_token(&mut self) -> Token {
       let tmp_token = self.peek_token();
       self.token_stream_cursor += 1;

       tmp_token
    }

    fn peek_token(&self) -> Token {
        self.token_stream[self.token_stream_cursor]
    }
}

/* == Parser == */

#[derive(Debug, Clone, PartialEq)]
enum Ast {
    Int(i32),
    Bop(Box<Self>, Token, Box<Self>),
    Uop(Token, Box<Self>)
}

fn parse<T>(expr: T) -> Ast 
where T: AsRef<str> {
    parse_expr(&mut Lexer::new(expr.as_ref()), 0)
}

fn parse_expr(lexer: &mut Lexer, min_prec: u8) -> Ast {
    let token = lexer.next_token();

    let mut lhs = match token {
        Token::Int(int) => Ast::Int(int),
        Token::Bop('+') | Token::Bop('-') => {
            let rbp = token.get_unary_prec();
            let rhs = parse_expr(lexer, rbp);

            Ast::Uop(token, Box::new(rhs))
        }

        Token::Paren('(') => {
            let lhs = parse_expr(lexer, 0);
            lexer.next_token();

            lhs
        }

        invalid_token => panic!("Unexpected token: {:?}. Expected: [+, -, 0-9]", invalid_token)
    };

    loop {
        let curr_token = lexer.peek_token();

        if matches!(curr_token, Token::End) || 
           matches!(curr_token, Token::Paren(')')){
            break; 
        }

        let (lbp, rbp) = curr_token.get_binary_prec();

        if lbp < min_prec {
            break;
        }

        lexer.next_token();

        let rhs = parse_expr(lexer, rbp);
        lhs = Ast::Bop(Box::new(lhs), curr_token, Box::new(rhs));
    }

    lhs
}

/* == Ast evaluation and code generation == */


fn evaluate<T>(ast: T) -> i32 
where T: Borrow<Ast> {
    let ast_ref = ast.borrow();

    match ast_ref {
        Ast::Int(int) => *int,
        Ast::Bop(left, token, right) => {
            let left_node  = evaluate(&**left);
            let right_node = evaluate(&**right);

            match token {
                Token::Bop('+') => left_node + right_node,
                Token::Bop('-') => left_node - right_node,
                Token::Bop('*') => left_node * right_node,
                Token::Bop('/') => left_node / right_node,
                Token::Bop('^') => left_node.pow(right_node as u32),
                _ => panic!("Invalid operation!")
            }
        }

        Ast::Uop(token, trailing_node) => {
            let unary_value = evaluate(&**trailing_node);

            match token {
                Token::Bop('+') => unary_value,
                Token::Bop('-') => -unary_value,
                _ => panic!("Invalid operation!")
            }
        }
    }
}

fn extract_int(ast_node: &Ast) -> Result<i32, &'static str> {
    println!("Node: {:#?}", ast_node);

    if let Ast::Int(int) = ast_node {
        return Ok(*int)
    }

    Err("'ast_node' isn't Ast::Int(_)!")
}

fn walk_ast(ast: &Ast, output: &mut String) {
    match ast {
        Ast::Int(int) => output.push_str(&format!("\tmov rax, {}\n", int)),
        Ast::Bop(left, token, right) => {
            match token {
                Token::Bop('+') => {
                    walk_ast(left, output);

                    output.push_str("\tpush rax\n");

                    walk_ast(right, output);

                    output.push_str("\tpop rbx\n\
                                     \tadd rax, rbx\n");
                }

                Token::Bop('-') => {
                    walk_ast(right, output);

                    output.push_str("\tpush rax\n");

                    walk_ast(left, output);

                    output.push_str("\tpop rbx\n\
                                     \tsub rax, rbx\n");
                }

                Token::Bop('*') => {
                    walk_ast(left, output);

                    output.push_str("\tpush rax\n");
                    
                    walk_ast(right, output);

                    output.push_str("\tpop rbx\n\
                                     \timul rbx\n");
                }

                Token::Bop('/') => {
                    walk_ast(right, output);

                    output.push_str("\tpush rax\n");
                    
                    walk_ast(left, output);

                    output.push_str("\txor rdx, rdx\n\
                                     \tpop rbx\n\
                                     \tidiv rbx\n");
                }

                Token::Bop('^') => { 
                    walk_ast(left, output);
                    output.push_str("\tpush rax\n");
                }
                _ => panic!("Invalid operation!")
            }
        }

        Ast::Uop(token, trailing_node) => {
            match token {
                Token::Bop('+') => { /* Do nothing, it's positive to begin with */ }
                Token::Bop('-') => {
                    let extracted_int = extract_int(trailing_node).unwrap();

                    output.push_str(&format!("\tmov rax, -{}\n", extracted_int));
                }

                _ => panic!("Invalid operation!")
            }
        }
    }
}

fn generate_x86(ast: &Ast) -> String {
    let mut generated_code = "global main\n\
                              section .text\n\
                              main:\n".to_string();

    walk_ast(&ast, &mut generated_code);

    generated_code.push_str("\tret");

    generated_code
}

/* == REPL (Read-eval-print-loop) */
fn repl() {
    loop { 
        print!("Enter an expression: ");
        io::stdout().flush().unwrap();

        let mut stdin_buffer: String = String::new();
        io::stdin().read_line(&mut stdin_buffer).unwrap();
        stdin_buffer.pop(); 

        let ast = parse(stdin_buffer);

        println!("== Tree form (AST)==\n\
                 {:#?}\n\n\
                 == Evaluation ==\n\
                 {}\n\n\
                 == Code generation ==\n\
                 {}", ast, evaluate(&ast), generate_x86(&ast));
    }
}

#[cfg(test)]
mod unit_tests {
    use crate::{ Token, Lexer, parse, evaluate };

    #[test]
    fn lex_test() {
        // Whitespace ignorance test 
        assert_eq!(Lexer::lex_src(" "), vec![Token::End]);

        // Single digit test 
        assert_eq!(Lexer::lex_src("1 + 1"), vec![Token::Int(1), Token::Bop('+'), Token::Int(1), Token::End]);

        // Multi-digit test
        assert_eq!(Lexer::lex_src("10 * 20"), vec![Token::Int(10), Token::Bop('*'), Token::Int(20), Token::End]);

        // Multiple operations test 
        assert_eq!(Lexer::lex_src("(10 + 5 * 2 / 2) + 5"), vec![Token::Paren('('),
                                                                Token::Int(10),
                                                                Token::Bop('+'),
                                                                Token::Int(5),
                                                                Token::Bop('*'),
                                                                Token::Int(2),
                                                                Token::Bop('/'),
                                                                Token::Int(2),
                                                                Token::Paren(')'),
                                                                Token::Bop('+'),
                                                                Token::Int(5),
                                                                Token::End]);
    }

    #[test]
    fn evaluate_test() {
        // Single addition:
        assert_eq!(evaluate(parse("15 + 5")), 20);

        // Multiple additions and subtractions
        assert_eq!(evaluate(parse("1 + 1 + 1 - 1 - 1")), 1);

        // Multiple multiplication and division:
        assert_eq!(evaluate(parse("5 * 10 / 2")), 25);

        // Negative numbers:
        assert_eq!(evaluate(parse("-5 + 1")), -4);

        // Parentheses:
        assert_eq!(evaluate(parse("(10 + 5) * 2")), 30);
    }
}

fn main() {
    repl();
}

