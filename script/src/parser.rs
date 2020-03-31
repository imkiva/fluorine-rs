use std::result::Result;
use pest::Parser;
use pest::error::Error;
use pest::iterators::{Pair, Pairs};

#[derive(Parser)]
#[grammar = "fs.pest"]
pub struct FsParser;

impl FsParser {
}
