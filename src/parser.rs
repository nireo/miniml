use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, digit1, multispace1, not_line_ending, satisfy},
    combinator::{all_consuming, map, not, opt, peek, recognize, value, verify},
    multi::{fold_many0, many0, many1, separated_list1},
    sequence::{delimited, pair, preceded, terminated},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Neq,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Var(String),
    Bool(bool),
    Int(i64),
    If {
        cond: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Box<Expr>,
    },
    Lambda {
        params: Vec<String>,
        body: Box<Expr>,
    },
    App(Box<Expr>, Box<Expr>),
    Let {
        name: String,
        value: Box<Expr>,
        body: Box<Expr>,
    },
    BinOp {
        op: BinOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
}

type ParseError<'a> = nom::error::Error<&'a str>;
type Res<'a, O> = IResult<&'a str, O, ParseError<'a>>;

pub fn parse(input: &str) -> Result<Expr, nom::Err<ParseError<'_>>> {
    all_consuming(ws0(parse_expr))
        .parse(input)
        .map(|(_, expr)| expr)
}

fn parse_expr(input: &str) -> Res<'_, Expr> {
    parse_let(input)
}

fn parse_let(input: &str) -> Res<'_, Expr> {
    // let has the lowest precedence
    alt((parse_let_binding, parse_lambda)).parse(input)
}

fn parse_lambda(input: &str) -> Res<'_, Expr> {
    // fun / if / everything else (starting at || level)
    alt((parse_lambda_expr, parse_if, parse_or)).parse(input)
}

fn parse_if(input: &str) -> Res<'_, Expr> {
    (
        keyword("if"),
        space1,
        parse_expr,
        ws0(keyword("then")),
        parse_expr,
        ws0(keyword("else")),
        parse_expr,
    )
        .map(|(_, _, cond, _, then_branch, _, else_branch)| Expr::If {
            cond: Box::new(cond),
            then_branch: Box::new(then_branch),
            else_branch: Box::new(else_branch),
        })
        .parse(input)
}

/// || (lowest precedence among binary ops)
fn parse_or(input: &str) -> Res<'_, Expr> {
    let (rest, first) = parse_and(input)?;
    fold_many0(
        pair(ws0(tag("||")), parse_and),
        move || first.clone(),
        |left, (_, right)| Expr::BinOp {
            op: BinOp::Or,
            left: Box::new(left),
            right: Box::new(right),
        },
    )
    .parse(rest)
}

/// &&
fn parse_and(input: &str) -> Res<'_, Expr> {
    let (rest, first) = parse_eq(input)?;
    fold_many0(
        pair(ws0(tag("&&")), parse_eq),
        move || first.clone(),
        |left, (_, right)| Expr::BinOp {
            op: BinOp::And,
            left: Box::new(left),
            right: Box::new(right),
        },
    )
    .parse(rest)
}

/// ==, !=
fn parse_eq(input: &str) -> Res<'_, Expr> {
    let (rest, first) = parse_cmp(input)?;
    fold_many0(
        pair(ws0(alt((tag("=="), tag("!=")))), parse_cmp),
        move || first.clone(),
        |left, (op_str, right)| {
            let op = match op_str {
                "==" => BinOp::Eq,
                "!=" => BinOp::Neq,
                _ => unreachable!(),
            };
            Expr::BinOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            }
        },
    )
    .parse(rest)
}

/// <, <=, >, >=
fn parse_cmp(input: &str) -> Res<'_, Expr> {
    let (rest, first) = parse_add(input)?;
    fold_many0(
        pair(
            ws0(alt((tag("<="), tag(">="), tag("<"), tag(">")))),
            parse_add,
        ),
        move || first.clone(),
        |left, (op_str, right)| {
            let op = match op_str {
                "<" => BinOp::Lt,
                "<=" => BinOp::Le,
                ">" => BinOp::Gt,
                ">=" => BinOp::Ge,
                _ => unreachable!(),
            };
            Expr::BinOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            }
        },
    )
    .parse(rest)
}

/// +, -
fn parse_add(input: &str) -> Res<'_, Expr> {
    let (rest, first) = parse_mul(input)?;
    fold_many0(
        pair(ws0(alt((char('+'), char('-')))), parse_mul),
        move || first.clone(),
        |left, (op_ch, right)| {
            let op = match op_ch {
                '+' => BinOp::Add,
                '-' => BinOp::Sub,
                _ => unreachable!(),
            };
            Expr::BinOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            }
        },
    )
    .parse(rest)
}

/// *, /
fn parse_mul(input: &str) -> Res<'_, Expr> {
    let (rest, first) = parse_application(input)?;
    fold_many0(
        pair(ws0(alt((char('*'), char('/')))), parse_application),
        move || first.clone(),
        |left, (op_ch, right)| {
            let op = match op_ch {
                '*' => BinOp::Mul,
                '/' => BinOp::Div,
                _ => unreachable!(),
            };
            Expr::BinOp {
                op,
                left: Box::new(left),
                right: Box::new(right),
            }
        },
    )
    .parse(rest)
}

fn parse_let_binding(input: &str) -> Res<'_, Expr> {
    (
        keyword("let"),
        space1,
        identifier,
        ws0(char('=')),
        parse_expr,
        ws0(keyword("in")),
        parse_expr,
    )
        .map(|(_, _, name, _, value, _, body)| Expr::Let {
            name,
            value: Box::new(value),
            body: Box::new(body),
        })
        .parse(input)
}

fn parse_lambda_expr(input: &str) -> Res<'_, Expr> {
    (
        keyword("fun"),
        space1,
        parse_params,
        ws0(tag("->")),
        parse_expr,
    )
        .map(|(_, _, params, _, body)| Expr::Lambda {
            params,
            body: Box::new(body),
        })
        .parse(input)
}

fn parse_application(input: &str) -> Res<'_, Expr> {
    map(
        pair(parse_atom, many0(preceded(space1, parse_atom))),
        |(func, args)| {
            args.into_iter()
                .fold(func, |f, arg| Expr::App(Box::new(f), Box::new(arg)))
        },
    )
    .parse(input)
}

fn parse_atom(input: &str) -> Res<'_, Expr> {
    alt((parse_paren_expr, parse_bool, parse_int, parse_variable)).parse(input)
}

fn parse_paren_expr(input: &str) -> Res<'_, Expr> {
    delimited(ws0(char('(')), parse_expr, ws0(char(')'))).parse(input)
}

fn parse_params(input: &str) -> Res<'_, Vec<String>> {
    alt((
        delimited(
            ws0(char('(')),
            separated_list1(ws0(char(',')), identifier),
            ws0(char(')')),
        ),
        many1(ws0(identifier)),
    ))
    .parse(input)
}

fn parse_variable(input: &str) -> Res<'_, Expr> {
    map(identifier, Expr::Var).parse(input)
}

fn parse_int(input: &str) -> Res<'_, Expr> {
    map(recognize(pair(opt(char('-')), digit1)), |digits: &str| {
        Expr::Int(digits.parse().unwrap())
    })
    .parse(input)
}

fn parse_bool(input: &str) -> Res<'_, Expr> {
    alt((
        value(Expr::Bool(true), keyword("true")),
        value(Expr::Bool(false), keyword("false")),
    ))
    .parse(input)
}

fn identifier(input: &str) -> Res<'_, String> {
    map(
        verify(
            recognize(pair(
                satisfy(is_ident_start),
                many0(satisfy(is_ident_continue)),
            )),
            |s: &str| !matches_keyword(s),
        ),
        |s: &str| s.to_string(),
    )
    .parse(input)
}

fn keyword<'a>(kw: &'static str) -> impl Parser<&'a str, Output = &'a str, Error = ParseError<'a>> {
    terminated(tag(kw), peek(not(satisfy(is_ident_continue))))
}

fn matches_keyword(s: &str) -> bool {
    matches!(
        s,
        "let" | "in" | "fun" | "if" | "then" | "else" | "true" | "false"
    )
}

fn is_ident_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_ident_continue(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

fn ws0<'a, P, O>(parser: P) -> impl Parser<&'a str, Output = O, Error = ParseError<'a>>
where
    P: Parser<&'a str, Output = O, Error = ParseError<'a>>,
{
    delimited(space0, parser, space0)
}

fn space0(input: &str) -> Res<'_, ()> {
    value((), many0(alt((value((), multispace1), line_comment)))).parse(input)
}

fn space1(input: &str) -> Res<'_, ()> {
    value(
        (),
        pair(alt((value((), multispace1), line_comment)), space0),
    )
    .parse(input)
}

fn line_comment(input: &str) -> Res<'_, ()> {
    value((), pair(tag("--"), opt(not_line_ending))).parse(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_parse(source: &str, expected: Expr) {
        let parsed = parse(source).expect("parse failed");
        assert_eq!(parsed, expected);
    }

    #[test]
    fn parses_variables() {
        assert_parse("x", Expr::Var("x".into()));
        assert_parse("foo_bar", Expr::Var("foo_bar".into()));
    }

    #[test]
    fn parses_lambdas() {
        assert_parse(
            "fun x -> x + 1",
            Expr::Lambda {
                params: vec!["x".into()],
                body: Box::new(Expr::BinOp {
                    op: BinOp::Add,
                    left: Box::new(Expr::Var("x".into())),
                    right: Box::new(Expr::Int(1)),
                }),
            },
        );

        assert_parse(
            "fun (x, y) -> x",
            Expr::Lambda {
                params: vec!["x".into(), "y".into()],
                body: Box::new(Expr::Var("x".into())),
            },
        );
    }

    #[test]
    fn skips_comments_and_whitespace() {
        assert_parse("-- Variables\n\n  x", Expr::Var("x".into()));
    }

    #[test]
    fn parses_application() {
        let app = Expr::App(
            Box::new(Expr::App(
                Box::new(Expr::Var("f".into())),
                Box::new(Expr::Var("x".into())),
            )),
            Box::new(Expr::Var("y".into())),
        );
        assert_parse("f x y", app);
    }

    #[test]
    fn parses_let_bindings() {
        assert_parse(
            "let x = 10 in x + 1",
            Expr::Let {
                name: "x".into(),
                value: Box::new(Expr::Int(10)),
                body: Box::new(Expr::BinOp {
                    op: BinOp::Add,
                    left: Box::new(Expr::Var("x".into())),
                    right: Box::new(Expr::Int(1)),
                }),
            },
        );

        assert_parse(
            "let id = fun x -> x in id 5",
            Expr::Let {
                name: "id".into(),
                value: Box::new(Expr::Lambda {
                    params: vec!["x".into()],
                    body: Box::new(Expr::Var("x".into())),
                }),
                body: Box::new(Expr::App(
                    Box::new(Expr::Var("id".into())),
                    Box::new(Expr::Int(5)),
                )),
            },
        );
    }

    #[test]
    fn parses_bools() {
        assert_parse("true", Expr::Bool(true));
        assert_parse("false", Expr::Bool(false));
    }

    #[test]
    fn parses_if_expressions() {
        assert_parse(
            "if true then 1 else 2",
            Expr::If {
                cond: Box::new(Expr::Bool(true)),
                then_branch: Box::new(Expr::Int(1)),
                else_branch: Box::new(Expr::Int(2)),
            },
        );

        assert_parse(
            "if x then f 1 else g 2 3",
            Expr::If {
                cond: Box::new(Expr::Var("x".into())),
                then_branch: Box::new(Expr::App(
                    Box::new(Expr::Var("f".into())),
                    Box::new(Expr::Int(1)),
                )),
                else_branch: Box::new(Expr::App(
                    Box::new(Expr::App(
                        Box::new(Expr::Var("g".into())),
                        Box::new(Expr::Int(2)),
                    )),
                    Box::new(Expr::Int(3)),
                )),
            },
        );
    }

    // ---------- Operator & precedence tests ----------

    #[test]
    fn arithmetic_precedence_mul_higher_than_add() {
        // 1 + 2 * 3  =>  1 + (2 * 3)
        assert_parse(
            "1 + 2 * 3",
            Expr::BinOp {
                op: BinOp::Add,
                left: Box::new(Expr::Int(1)),
                right: Box::new(Expr::BinOp {
                    op: BinOp::Mul,
                    left: Box::new(Expr::Int(2)),
                    right: Box::new(Expr::Int(3)),
                }),
            },
        );
    }

    #[test]
    fn arithmetic_left_associative() {
        // 1 - 2 - 3  =>  (1 - 2) - 3
        assert_parse(
            "1 - 2 - 3",
            Expr::BinOp {
                op: BinOp::Sub,
                left: Box::new(Expr::BinOp {
                    op: BinOp::Sub,
                    left: Box::new(Expr::Int(1)),
                    right: Box::new(Expr::Int(2)),
                }),
                right: Box::new(Expr::Int(3)),
            },
        );
    }

    #[test]
    fn application_has_higher_precedence_than_mul() {
        // f x * 2  =>  (f x) * 2
        assert_parse(
            "f x * 2",
            Expr::BinOp {
                op: BinOp::Mul,
                left: Box::new(Expr::App(
                    Box::new(Expr::Var("f".into())),
                    Box::new(Expr::Var("x".into())),
                )),
                right: Box::new(Expr::Int(2)),
            },
        );
    }

    #[test]
    fn comparison_has_lower_precedence_than_add() {
        // 1 + 2 < 3 + 4  =>  (1 + 2) < (3 + 4)
        assert_parse(
            "1 + 2 < 3 + 4",
            Expr::BinOp {
                op: BinOp::Lt,
                left: Box::new(Expr::BinOp {
                    op: BinOp::Add,
                    left: Box::new(Expr::Int(1)),
                    right: Box::new(Expr::Int(2)),
                }),
                right: Box::new(Expr::BinOp {
                    op: BinOp::Add,
                    left: Box::new(Expr::Int(3)),
                    right: Box::new(Expr::Int(4)),
                }),
            },
        );
    }

    #[test]
    fn equality_has_lower_precedence_than_comparison() {
        // x < 3 == y < 4  =>  (x < 3) == (y < 4)
        assert_parse(
            "x < 3 == y < 4",
            Expr::BinOp {
                op: BinOp::Eq,
                left: Box::new(Expr::BinOp {
                    op: BinOp::Lt,
                    left: Box::new(Expr::Var("x".into())),
                    right: Box::new(Expr::Int(3)),
                }),
                right: Box::new(Expr::BinOp {
                    op: BinOp::Lt,
                    left: Box::new(Expr::Var("y".into())),
                    right: Box::new(Expr::Int(4)),
                }),
            },
        );
    }

    #[test]
    fn boolean_precedence_and_higher_than_or() {
        // true && false || true  =>  (true && false) || true
        assert_parse(
            "true && false || true",
            Expr::BinOp {
                op: BinOp::Or,
                left: Box::new(Expr::BinOp {
                    op: BinOp::And,
                    left: Box::new(Expr::Bool(true)),
                    right: Box::new(Expr::Bool(false)),
                }),
                right: Box::new(Expr::Bool(true)),
            },
        );
    }

    #[test]
    fn operators_inside_if() {
        assert_parse(
            "if x < 3 then 1 + 2 else 3 * 4",
            Expr::If {
                cond: Box::new(Expr::BinOp {
                    op: BinOp::Lt,
                    left: Box::new(Expr::Var("x".into())),
                    right: Box::new(Expr::Int(3)),
                }),
                then_branch: Box::new(Expr::BinOp {
                    op: BinOp::Add,
                    left: Box::new(Expr::Int(1)),
                    right: Box::new(Expr::Int(2)),
                }),
                else_branch: Box::new(Expr::BinOp {
                    op: BinOp::Mul,
                    left: Box::new(Expr::Int(3)),
                    right: Box::new(Expr::Int(4)),
                }),
            },
        );
    }
}
