use nom::IResult;
use nom_locate::LocatedSpan;

pub mod expr;
pub mod top_level;
pub mod ty;

pub trait Parser
where
    Self: Sized,
{
    fn parse(s: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, Self>;
}
