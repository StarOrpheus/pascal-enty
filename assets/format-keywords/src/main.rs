use std::fs::File;
use std::io::{BufReader, BufRead};

fn main() {
    let file = File::open("keywords.list").unwrap();
    let mut tokenList = Vec::new();
    for line in BufReader::new(file).lines() {
        let line = line.unwrap();
        if line.is_empty() {
            continue;
        }

        let upperLine = line.trim().to_uppercase();
        for c in upperLine.chars() {
            print!("${} ", c);
        }
        print!("{}", " ".repeat(48 - 3 * upperLine.len()));
        let tokenName= String::new() + "Token" + &upperLine;
        print!("{{ {}{} }}", "\\_ -> ", tokenName);
        tokenList.push(tokenName);
        println!();
    }

    println!("Token list:\n");
    for token in tokenList {
        print!("{} | ", token);
    }
    println!();
}
