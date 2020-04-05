#[cfg(test)]
mod tests {
    use crate::parse::FsParser;

    #[test]
    fn test_pattern_match() {
        let a = FsParser::ast("let a = match \"hello\" { \n\
            true => 1, \n\
            false => 0 \n\
            _ => \"fuck\", \n\
            }").unwrap();
        println!("{:#?}", a);
    }
}
