use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr<Span> {
    Name(Span, String),
    FunType(Span, String, Rc<Self>, Rc<Self>),
    FunExpr(Span, String, Rc<Self>),
    App(Span, Rc<Self>, Rc<Self>),
}
