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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Var(String),
    Int(i64),
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
    alt((parse_let_binding, parse_lambda)).parse(input)
}

fn parse_lambda(input: &str) -> Res<'_, Expr> {
    alt((parse_lambda_expr, parse_add)).parse(input)
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

fn parse_add(input: &str) -> Res<'_, Expr> {
    let (rest, first) = parse_application(input)?;
    fold_many0(
        pair(ws0(char('+')), parse_application),
        move || first.clone(),
        |left, (_, right)| Expr::BinOp {
            op: BinOp::Add,
            left: Box::new(left),
            right: Box::new(right),
        },
    )
    .parse(rest)
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
    alt((parse_paren_expr, parse_int, parse_variable)).parse(input)
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
    matches!(s, "let" | "in" | "fun")
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
}
