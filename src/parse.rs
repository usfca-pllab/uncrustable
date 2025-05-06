//! The parser

use logos::Logos;
use std::collections::{HashMap, HashSet};

use crate::syntax::*;
//---------------------------------------------------------------------
// Logos-based Lexer
//---------------------------------------------------------------------

#[derive(Default, Debug, PartialEq, Clone)]
enum LexError {
    InvalidNumber,
    #[default]
    UnexpectedChar,
}

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\n\r\f]+")]
#[logos(skip r"//[^\n]*")]
#[logos(skip r"/\*([^*]|\*[^/])*\*/")]
#[logos(error = LexError)]
enum Token {
    // Keywords
    #[token("alphabet:")]
    Alphabet,
    #[token("fn")]
    Fn,
    #[token("let")]
    Let,
    #[token("on")]
    On,
    #[token("input")]
    Input,
    #[token("accept")]
    Accept,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("int")]
    Int,
    #[token("bool")]
    Bool,
    #[token("sym")]
    Sym,
    #[token("match")]
    Match,
    #[token("as")]
    As,
    #[token("mod")]
    Mod,
    #[token("wraparound")]
    Wraparound,
    #[token("saturate")]
    Saturate,
    #[token("fail")]
    Fail,

    // Operators and punctuation
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("<<")]
    Shl,
    #[token(">>")]
    Shr,
    #[token("<=")]
    Lte,
    #[token(">=")]
    Gte,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("!=")]
    Ne,
    #[token("==")]
    Eq,
    #[token("&&")]
    And,
    #[token("||")]
    Or,
    #[token("!")]
    Not,
    #[token("->")]
    Arrow,
    #[token("..")]
    DotDot,

    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token(";")]
    Semicolon,
    #[token("=")]
    Gets,

    // Literals
    #[regex(r"-?[0-9]+", |lex| lex.slice().parse().or(Err(LexError::InvalidNumber)))]
    Number(i64),
    #[regex(r"[a-zA-Z][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Identifier(String),
    #[regex(r"'([^'\\]|\\.)'", parse_char)]
    CharLiteral(char),
}

fn parse_char(lex: &mut logos::Lexer<Token>) -> Option<char> {
    let slice = lex.slice();
    // Assume a simple char literal of the form 'x'
    slice.chars().nth(1)
}

//---------------------------------------------------------------------
// Recursive Descent Parser
//---------------------------------------------------------------------

struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    fn new(input: &str) -> Self {
        let tokens: Vec<Token> = Token::lexer(input)
            .spanned()
            .map(|(tok, span)| match tok {
                Ok(t) => t,
                Err(e) => panic!("Lexical error at {span:?}: {e:?}"),
            })
            .collect();
        Parser { tokens, pos: 0 }
    }

    fn current(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn bump(&mut self) {
        self.pos += 1;
    }

    // Check for a token variant (ignoring any attached data)
    fn expect(&mut self, expected: Token) -> Result<(), String> {
        if let Some(tok) = self.current() {
            if std::mem::discriminant(tok) == std::mem::discriminant(&expected) {
                self.bump();
                Ok(())
            } else {
                Err(format!("Expected {:?}, found {:?}", expected, tok))
            }
        } else {
            Err(format!("Expected {:?}, found EOF", expected))
        }
    }

    // For tokens with data
    fn expect_identifier(&mut self) -> Result<String, String> {
        if let Some(Token::Identifier(s)) = self.current() {
            let s = s.clone();
            self.bump();
            Ok(s)
        } else {
            Err(format!("Expected identifier, found {:?}", self.current()))
        }
    }

    fn expect_number(&mut self) -> Result<i64, String> {
        if let Some(Token::Number(n)) = self.current() {
            let n = *n;
            self.bump();
            Ok(n)
        } else {
            Err(format!("Expected number, found {:?}", self.current()))
        }
    }

    fn expect_char_literal(&mut self) -> Result<char, String> {
        if let Some(Token::CharLiteral(c)) = self.current() {
            let c = *c;
            self.bump();
            Ok(c)
        } else {
            Err(format!("Expected char literal, found {:?}", self.current()))
        }
    }

    //-----------------------------------------------------------------
    // Parsing functions for grammar nonterminals
    //-----------------------------------------------------------------

    // program ::= `alphabet:` `{` LIST(symbol) `}` fun_decl* local_decl* block action accept
    fn parse_program(&mut self) -> Result<Program, String> {
        self.expect(Token::Alphabet)?;
        self.expect(Token::LBrace)?;
        let alphabet = self.parse_symbol_list()?;
        self.expect(Token::RBrace)?;

        let mut helpers = HashMap::new();
        while let Some(tok) = self.current() {
            if let Token::Fn = tok {
                let (id, func) = self.parse_fun_decl()?;
                helpers.insert(id, func);
            } else {
                break;
            }
        }

        let mut locals = HashMap::new();
        while let Some(tok) = self.current() {
            if let Token::Let = tok {
                self.parse_local_decl(&mut locals)?;
            } else {
                break;
            }
        }

        let mut start = vec![];
        while let Some(tok) = self.current() {
            if let Token::On = tok {
                break;
            }
            start.push(self.parse_stmt()?);
        }
        let action = self.parse_action()?;
        let accept = self.parse_accept()?;

        if let Some(tok) = self.current() {
            return Err(format!("Unexpected token after program end: {:?}", tok));
        }

        Ok(Program {
            alphabet,
            helpers,
            locals,
            start,
            action,
            accept,
        })
    }

    // LIST(symbol) = symbol (',' symbol)* ','?
    fn parse_symbol_list(&mut self) -> Result<HashSet<Symbol>, String> {
        let mut syms = HashSet::new();
        loop {
            if let Some(Token::CharLiteral(c)) = self.current() {
                syms.insert(Symbol(*c));
                self.bump();
            } else {
                return Err("Expected symbol in alphabet list".to_string());
            }
            if let Some(Token::Comma) = self.current() {
                self.bump();
                // Allow a trailing comma.
                if let Some(Token::RBrace) = self.current() {
                    break;
                }
            } else {
                break;
            }
        }
        Ok(syms)
    }

    // fun_decl ::= `fn` id `(` LIST(decl)? `)` `->` type `=` expr
    // Returns a pair (Id, Function)
    fn parse_fun_decl(&mut self) -> Result<(Id, Function), String> {
        self.expect(Token::Fn)?;
        let name = self.expect_identifier()?;
        let func_id = id(&name);
        self.expect(Token::LParen)?;
        let params = if let Some(Token::Identifier(_)) = self.current() {
            self.parse_decl_list_with_types().unwrap_or_else(|_| vec![])
        } else {
            vec![]
        };
        self.expect(Token::RParen)?;
        self.expect(Token::Arrow)?;
        let ret_typ = self.parse_type()?;
        self.expect(Token::Gets)?;
        let body = self.parse_expr()?;
        Ok((
            func_id,
            Function {
                params,
                ret_typ,
                body,
            },
        ))
    }

    // local_decl ::= `let` LIST(decl) `;`
    // Update the provided locals map.
    fn parse_local_decl(&mut self, locals: &mut HashMap<Id, Type>) -> Result<(), String> {
        self.expect(Token::Let)?;
        let decls = self.parse_decl_list_with_types()?;
        self.expect(Token::Semicolon)?;
        for (id, typ) in decls {
            locals.insert(id, typ);
        }
        Ok(())
    }

    // decl ::= id `:` type
    fn parse_decl(&mut self) -> Result<(Id, Type), String> {
        let name = self.expect_identifier()?;
        let var_id = id(&name);
        self.expect(Token::Colon)?;
        let typ = self.parse_type()?;
        Ok((var_id, typ))
    }

    // decl_list ::= decl (',' decl)* ','?
    // For function parameters and local declarations.
    fn parse_decl_list_with_types(&mut self) -> Result<Vec<(Id, Type)>, String> {
        let mut decls = Vec::new();
        decls.push(self.parse_decl()?);
        while let Some(Token::Comma) = self.current() {
            self.bump();
            if let Some(tok) = self.current() {
                if let Token::RParen | Token::Semicolon = tok {
                    break;
                }
            }
            decls.push(self.parse_decl()?);
        }
        Ok(decls)
    }

    // type ::= `bool` | `int` `[` nonneg `]` | `int` `[` num `..` num `]` | `sym`
    fn parse_type(&mut self) -> Result<Type, String> {
        if let Some(tok) = self.current() {
            match tok {
                Token::Bool => {
                    self.bump();
                    Ok(Type::BoolT)
                }
                Token::Int => {
                    self.bump();
                    self.expect(Token::LBracket)?;
                    let start = self.expect_number()?;
                    if let Some(Token::DotDot) = self.current() {
                        self.bump();
                        let end = self.expect_number()?;
                        self.expect(Token::RBracket)?;
                        Ok(Type::NumT(start..end))
                    } else {
                        self.expect(Token::RBracket)?;
                        Ok(Type::NumT(0..start))
                    }
                }
                Token::Sym => {
                    self.bump();
                    Ok(Type::SymT)
                }
                _ => Err(format!("Unexpected token in type: {:?}", tok)),
            }
        } else {
            Err("Unexpected end of input while parsing type".to_string())
        }
    }

    // block ::= `{` stmt+ `}`
    fn parse_block(&mut self) -> Result<Block, String> {
        self.expect(Token::LBrace)?;
        let mut stmts = Vec::new();
        while let Some(tok) = self.current() {
            if let Token::RBrace = tok {
                break;
            }
            stmts.push(self.parse_stmt()?);
        }
        self.expect(Token::RBrace)?;
        Ok(stmts)
    }

    // action ::= `on` `input` id? block
    fn parse_action(&mut self) -> Result<(Option<Id>, Block), String> {
        self.expect(Token::On)?;
        self.expect(Token::Input)?;
        let id_opt = if let Some(Token::Identifier(s)) = self.current() {
            let s = s.clone();
            self.bump();
            Some(id(&s))
        } else {
            None
        };
        let block = self.parse_block()?;
        Ok((id_opt, block))
    }

    // accept ::= `accept` `if` expr
    fn parse_accept(&mut self) -> Result<Expr, String> {
        self.expect(Token::Accept)?;
        self.expect(Token::If)?;
        self.parse_expr()
    }

    // stmt ::= id `=` expr `;` | `if` expr block (`else` block)?
    fn parse_stmt(&mut self) -> Result<Stmt, String> {
        if let Some(tok) = self.current() {
            match tok {
                Token::If => {
                    self.bump();
                    let cond = self.parse_expr()?;
                    let true_branch = self.parse_block()?;
                    let false_branch = if let Some(Token::Else) = self.current() {
                        self.bump();
                        self.parse_block()?
                    } else {
                        vec![]
                    };
                    Ok(Stmt::If {
                        cond,
                        true_branch,
                        false_branch,
                    })
                }
                _ => {
                    let name = self.expect_identifier()?;
                    let var = id(&name);
                    self.expect(Token::Gets)?;
                    let expr = self.parse_expr()?;
                    self.expect(Token::Semicolon)?;
                    Ok(Stmt::Assign(var, expr))
                }
            }
        } else {
            Err("Unexpected end of input in statement".to_string())
        }
    }

    // Expression parsing.
    // For simplicity, we implement a recursive descent parser with:
    //   expr ::= binary_expr
    //   binary_expr ::= cast_expr ( binop cast_expr )*
    //   cast_expr ::= unary_expr ( "as" type overflow? )*
    //   unary_expr ::= unop unary_expr | primary
    //   primary ::= id | char | num | true | false | match ... | function call | ( expr )
    fn parse_expr(&mut self) -> Result<Expr, String> {
        self.parse_binary_expr()
    }

    fn parse_binary_expr(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_cast_expr()?;
        while let Some(op) = self.parse_binop()? {
            let right = self.parse_cast_expr()?;
            left = Expr::BinOp {
                lhs: Box::new(left),
                op,
                rhs: Box::new(right),
            };
        }
        Ok(left)
    }

    fn parse_cast_expr(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_unary_expr()?;
        while let Some(Token::As) = self.current() {
            self.bump(); // consume "as"
            let typ = self.parse_type()?;
            // Optional overflow specification.
            let overflow = if let Some(tok) = self.current() {
                match tok {
                    Token::Wraparound => {
                        self.bump();
                        Overflow::Wraparound
                    }
                    Token::Saturate => {
                        self.bump();
                        Overflow::Saturate
                    }
                    Token::Fail => {
                        self.bump();
                        Overflow::Fail
                    }
                    _ => Overflow::Wraparound,
                }
            } else {
                Overflow::Wraparound
            };
            expr = Expr::Cast {
                inner: Box::new(expr),
                typ,
                overflow,
            };
        }
        Ok(expr)
    }

    fn parse_unary_expr(&mut self) -> Result<Expr, String> {
        if let Some(tok) = self.current() {
            match tok {
                Token::Not | Token::Minus => {
                    let op = match tok {
                        Token::Not => UOp::Not,
                        Token::Minus => UOp::Negate,
                        _ => unreachable!(),
                    };
                    self.bump();
                    let expr = self.parse_unary_expr()?;
                    return Ok(Expr::UOp {
                        op,
                        inner: Box::new(expr),
                    });
                }
                _ => {}
            }
        }
        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Result<Expr, String> {
        if let Some(tok) = self.current() {
            match tok {
                Token::Identifier(s) => {
                    let name = s.clone();
                    self.bump();
                    let var = id(&name);
                    if let Some(Token::LParen) = self.current() {
                        self.bump();
                        let mut args = Vec::new();
                        if let Some(Token::RParen) = self.current() {
                            // empty argument list
                        } else {
                            args.push(self.parse_expr()?);
                            while let Some(Token::Comma) = self.current() {
                                self.bump();
                                args.push(self.parse_expr()?);
                            }
                        }
                        self.expect(Token::RParen)?;
                        Ok(Expr::Call { callee: var, args })
                    } else {
                        Ok(Expr::Var(var))
                    }
                }
                Token::CharLiteral(_) => {
                    let c = self.expect_char_literal()?;
                    Ok(Expr::Sym(c))
                }
                Token::Number(_) => {
                    let n = self.expect_number()?;
                    Ok(Expr::Num(n, Type::NumT(n..(n + 1))))
                }
                Token::True => {
                    self.bump();
                    Ok(Expr::Bool(true))
                }
                Token::False => {
                    self.bump();
                    Ok(Expr::Bool(false))
                }
                Token::Match => {
                    self.bump();
                    let scrutinee = self.parse_expr()?;
                    self.expect(Token::LBrace)?;
                    let mut cases = Vec::new();
                    while let Some(tok) = self.current() {
                        if let Token::RBrace = tok {
                            break;
                        }
                        cases.push(self.parse_case()?);
                    }
                    self.expect(Token::RBrace)?;
                    Ok(Expr::Match {
                        scrutinee: Box::new(scrutinee),
                        cases,
                    })
                }
                Token::LParen => {
                    self.bump();
                    let expr = self.parse_expr()?;
                    self.expect(Token::RParen)?;
                    Ok(expr)
                }
                _ => Err(format!("Unexpected token in primary expression: {:?}", tok)),
            }
        } else {
            Err("Unexpected end of input in primary expression".to_string())
        }
    }

    // Parses a binary operator into a BOp.
    fn parse_binop(&mut self) -> Result<Option<BOp>, String> {
        if let Some(tok) = self.current() {
            let op = match tok {
                Token::Plus => {
                    self.bump();
                    Some(BOp::Add)
                }
                Token::Minus => {
                    self.bump();
                    Some(BOp::Sub)
                }
                Token::Star => {
                    self.bump();
                    Some(BOp::Mul)
                }
                Token::Slash => {
                    self.bump();
                    Some(BOp::Div)
                }
                Token::Percent => {
                    self.bump();
                    Some(BOp::Rem)
                }
                Token::Shl => {
                    self.bump();
                    Some(BOp::Shl)
                }
                Token::Shr => {
                    self.bump();
                    Some(BOp::Shr)
                }
                Token::Lt => {
                    self.bump();
                    Some(BOp::Lt)
                }
                Token::Lte => {
                    self.bump();
                    Some(BOp::Lte)
                }
                Token::Eq => {
                    self.bump();
                    Some(BOp::Eq)
                }
                Token::Ne => {
                    self.bump();
                    Some(BOp::Ne)
                }
                Token::And => {
                    self.bump();
                    Some(BOp::And)
                }
                Token::Or => {
                    self.bump();
                    Some(BOp::Or)
                }
                // For greater-than operators, you could transform them as needed.
                Token::Gt => {
                    self.bump();
                    Some(BOp::Lt)
                }
                Token::Gte => {
                    self.bump();
                    Some(BOp::Lte)
                }
                _ => None,
            };
            Ok(op)
        } else {
            Ok(None)
        }
    }

    // case ::= pattern ( "if" expr )? "->" expr
    fn parse_case(&mut self) -> Result<Case, String> {
        let pattern = self.parse_pattern()?;
        let guard = if let Some(Token::If) = self.current() {
            self.bump();
            self.parse_expr()?
        } else {
            Expr::Bool(true)
        };
        self.expect(Token::Arrow)?;
        let result = self.parse_expr()?;
        Ok(Case {
            pattern,
            guard,
            result,
        })
    }

    // pattern ::= char | num | `true` | `false` | id
    fn parse_pattern(&mut self) -> Result<Pattern, String> {
        if let Some(tok) = self.current() {
            match tok {
                Token::CharLiteral(_) => {
                    let c = self.expect_char_literal()?;
                    Ok(Pattern::Sym(Symbol(c)))
                }
                Token::Number(_) => {
                    let n = self.expect_number()?;
                    Ok(Pattern::Num(n))
                }
                Token::True => {
                    self.bump();
                    Ok(Pattern::Bool(true))
                }
                Token::False => {
                    self.bump();
                    Ok(Pattern::Bool(false))
                }
                Token::Identifier(_) => {
                    let name = self.expect_identifier()?;
                    Ok(Pattern::Var(id(&name)))
                }
                _ => Err(format!("Unexpected token in pattern: {:?}", tok)),
            }
        } else {
            Err("Unexpected end of input in pattern".to_string())
        }
    }
}

/// Entry point of the parser.
pub fn parse(input: &str) -> Result<Program, String> {
    let mut parser = Parser::new(input);
    parser.parse_program()
}

// This all looks great. Please generate test cases (using #[test]) for parse_program. Generate one test case for each statement case, one test case for function declarations, and one test case for each expression case.

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use super::*;

    #[test]
    fn empty_program() {
        let input = r#"
            alphabet: {'a'}
            on input {
            }
            accept if true
        "#;
        let program = parse(input).unwrap();
        assert_eq!(program.alphabet, HashSet::from([Symbol('a')]));
        assert_eq!(program.helpers, HashMap::new());
        assert_eq!(program.locals, HashMap::new());
        assert_eq!(program.start, vec![]);
        assert_eq!(program.action, (None, vec![]));
        assert_eq!(program.accept, Expr::Bool(true));
    }

    #[test]
    fn simple_program() {
        let input = r#"
                alphabet: {'a'}
                fn add(a: int[3], b: int[0..3]) -> int[0..3] = a + b
                let x: int[3];
                on input y {
                        x = add(1, 2);
                        x = 3 + - 4 as int[3];
                        x = 3 + 4 as int[3] wraparound;
                        x = 3 + 4 as int[3] saturate;
                        x = 3 + 4 as int[3] fail;
                        if x < 3 {
                                y = 'a';
                        } else {
                                x = match y {
                                        'a' -> 1
                                        x if true -> 2
                                };
                        }
                }
                accept if x == 3
                "#;
        let program = parse(input).unwrap();
        let expected = expect![[r#"
            Program {
                alphabet: {
                    Symbol(
                        'a',
                    ),
                },
                helpers: {
                    "add": Function {
                        params: [
                            (
                                "a",
                                NumT(
                                    0..3,
                                ),
                            ),
                            (
                                "b",
                                NumT(
                                    0..3,
                                ),
                            ),
                        ],
                        ret_typ: NumT(
                            0..3,
                        ),
                        body: BinOp {
                            lhs: Var(
                                "a",
                            ),
                            op: Add,
                            rhs: Var(
                                "b",
                            ),
                        },
                    },
                },
                locals: {
                    "x": NumT(
                        0..3,
                    ),
                },
                start: [],
                action: (
                    Some(
                        "y",
                    ),
                    [
                        Assign(
                            "x",
                            Call {
                                callee: "add",
                                args: [
                                    Num(
                                        1,
                                        NumT(
                                            1..2,
                                        ),
                                    ),
                                    Num(
                                        2,
                                        NumT(
                                            2..3,
                                        ),
                                    ),
                                ],
                            },
                        ),
                        Assign(
                            "x",
                            BinOp {
                                lhs: Num(
                                    3,
                                    NumT(
                                        3..4,
                                    ),
                                ),
                                op: Add,
                                rhs: Cast {
                                    inner: UOp {
                                        op: Negate,
                                        inner: Num(
                                            4,
                                            NumT(
                                                4..5,
                                            ),
                                        ),
                                    },
                                    typ: NumT(
                                        0..3,
                                    ),
                                    overflow: Wraparound,
                                },
                            },
                        ),
                        Assign(
                            "x",
                            BinOp {
                                lhs: Num(
                                    3,
                                    NumT(
                                        3..4,
                                    ),
                                ),
                                op: Add,
                                rhs: Cast {
                                    inner: Num(
                                        4,
                                        NumT(
                                            4..5,
                                        ),
                                    ),
                                    typ: NumT(
                                        0..3,
                                    ),
                                    overflow: Wraparound,
                                },
                            },
                        ),
                        Assign(
                            "x",
                            BinOp {
                                lhs: Num(
                                    3,
                                    NumT(
                                        3..4,
                                    ),
                                ),
                                op: Add,
                                rhs: Cast {
                                    inner: Num(
                                        4,
                                        NumT(
                                            4..5,
                                        ),
                                    ),
                                    typ: NumT(
                                        0..3,
                                    ),
                                    overflow: Saturate,
                                },
                            },
                        ),
                        Assign(
                            "x",
                            BinOp {
                                lhs: Num(
                                    3,
                                    NumT(
                                        3..4,
                                    ),
                                ),
                                op: Add,
                                rhs: Cast {
                                    inner: Num(
                                        4,
                                        NumT(
                                            4..5,
                                        ),
                                    ),
                                    typ: NumT(
                                        0..3,
                                    ),
                                    overflow: Fail,
                                },
                            },
                        ),
                        If {
                            cond: BinOp {
                                lhs: Var(
                                    "x",
                                ),
                                op: Lt,
                                rhs: Num(
                                    3,
                                    NumT(
                                        3..4,
                                    ),
                                ),
                            },
                            true_branch: [
                                Assign(
                                    "y",
                                    Sym(
                                        'a',
                                    ),
                                ),
                            ],
                            false_branch: [
                                Assign(
                                    "x",
                                    Match {
                                        scrutinee: Var(
                                            "y",
                                        ),
                                        cases: [
                                            Case {
                                                pattern: Sym(
                                                    Symbol(
                                                        'a',
                                                    ),
                                                ),
                                                guard: Bool(
                                                    true,
                                                ),
                                                result: Num(
                                                    1,
                                                    NumT(
                                                        1..2,
                                                    ),
                                                ),
                                            },
                                            Case {
                                                pattern: Var(
                                                    "x",
                                                ),
                                                guard: Bool(
                                                    true,
                                                ),
                                                result: Num(
                                                    2,
                                                    NumT(
                                                        2..3,
                                                    ),
                                                ),
                                            },
                                        ],
                                    },
                                ),
                            ],
                        },
                    ],
                ),
                accept: BinOp {
                    lhs: Var(
                        "x",
                    ),
                    op: Eq,
                    rhs: Num(
                        3,
                        NumT(
                            3..4,
                        ),
                    ),
                },
            }"#]];
        expected.assert_eq(&format!("{:#?}", program));
    }
}
