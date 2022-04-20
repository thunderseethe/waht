
use crate::SourceId;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Span {
    source_id: SourceId,
    start: usize,
    end: usize,
}
impl std::ops::Add for Span {
    type Output = Span;

    fn add(self, rhs: Span) -> Span {
        debug_assert_eq!(self.source_id, rhs.source_id);
        Self {
            start: self.start.min(rhs.start),
            end: self.end.max(rhs.end),
            ..self
        }
    }
}
impl std::ops::AddAssign for Span {
    fn add_assign(&mut self, rhs: Self) {
        debug_assert_eq!(self.source_id, rhs.source_id);
        self.start = self.start.min(rhs.start);
        self.end = self.end.max(rhs.end);
    }
}
impl chumsky::Span for Span {
    type Context = SourceId;
    type Offset = usize;

    fn new(context: Self::Context, range: std::ops::Range<Self::Offset>) -> Self {
        Self {
            source_id: context,
            start: range.start,
            end: range.end,
        }
    }

    fn context(&self) -> Self::Context {
        self.source_id
    }

    fn start(&self) -> Self::Offset {
        self.start
    }

    fn end(&self) -> Self::Offset {
        self.end
    }
}
impl ariadne::Span for Span {
    type SourceId = SourceId;

    fn source(&self) -> &Self::SourceId {
        &self.source_id
    }

    fn start(&self) -> usize {
        self.start
    }

    fn end(&self) -> usize {
        self.end
    }
}