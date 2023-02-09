use std::collections::{HashMap, HashSet};

use relational_parsing::{Symbol, Nonterminal, Terminal, Word, Grammar};

pub fn basic_relational_parsing_example_grammar() -> Grammar {
    let terminals: HashSet<Terminal> = HashSet::from(['a', 'b', 'c']);
    let nonterminals: HashSet<Nonterminal> = HashSet::from(['S']);
    let start: Nonterminal = 'S';
    let mut rules: HashMap<Nonterminal, HashSet<Word>> = HashMap::new();
    rules.insert('S', HashSet::from([
        vec![Symbol::Terminal('a')],
        vec![Symbol::Nonterminal('S'), Symbol::Terminal('a')],
        vec![Symbol::Nonterminal('S'), Symbol::Terminal('b'), Symbol::Nonterminal('S'), Symbol::Terminal('c')]
    ]));
    Grammar::new(terminals, nonterminals, start, rules)
}

pub fn e_rule_relational_parsing_example_grammar() -> Grammar {
    let terminals: HashSet<Terminal> = HashSet::from(['a', 'b', 'c']);
    let nonterminals: HashSet<Nonterminal> = HashSet::from(['S']);
    let start: Nonterminal = 'S';
    let mut rules: HashMap<Nonterminal, HashSet<Word>> = HashMap::new();
    rules.insert('S', HashSet::from([
        vec![Symbol::Terminal('a')],
        vec![Symbol::Nonterminal('S'), Symbol::Terminal('a')],
        vec![Symbol::Nonterminal('S'), Symbol::Terminal('b'), Symbol::Nonterminal('S'), Symbol::Terminal('c')],
        vec![Symbol::Epsilon]
    ]));
    Grammar::new(terminals, nonterminals, start, rules)
}

pub fn extra_e_rule_relational_parsing_example_grammar() -> Grammar {
    let terminals: HashSet<Terminal> = HashSet::from(['a', 'b', 'c']);
    let nonterminals: HashSet<Nonterminal> = HashSet::from(['S']);
    let start: Nonterminal = 'S';
    let mut rules: HashMap<Nonterminal, HashSet<Word>> = HashMap::new();
    rules.insert('S', HashSet::from([
        vec![Symbol::Terminal('a')],
        vec![Symbol::Nonterminal('S'), Symbol::Terminal('a')],
        vec![Symbol::Nonterminal('S'), Symbol::Terminal('b'), Symbol::Nonterminal('S'), Symbol::Terminal('c')],
        vec![Symbol::Nonterminal('S'), Symbol::Terminal('b'), Symbol::Nonterminal('S'), Symbol::Nonterminal('S'), Symbol::Terminal('c')],
        vec![Symbol::Epsilon]
    ]));
    Grammar::new(terminals, nonterminals, start, rules)
}

pub fn three_rule_grammar() -> Grammar {
    let terminals: HashSet<Terminal> = HashSet::from(['a', 'b']);
    let nonterminals: HashSet<Nonterminal> = HashSet::from(['S']);
    let start: Nonterminal = 'S';
    let mut rules: HashMap<Nonterminal, HashSet<Word>> = HashMap::new();
    rules.insert('S', HashSet::from([
        vec![Symbol::Terminal('a'), Symbol::Nonterminal('S'), Symbol::Terminal('b')],
        vec![Symbol::Nonterminal('S'), Symbol::Terminal('a'), Symbol::Terminal('b')],
        vec![Symbol::Terminal('a'), Symbol::Terminal('a'), Symbol::Terminal('a')],
    ]));
    Grammar::new(terminals, nonterminals, start, rules)
}

pub fn difficult_bottom_up_grammar() -> Grammar {
    let terminals: HashSet<Terminal> = HashSet::from(['a', '+', '-']);
    let nonterminals: HashSet<Nonterminal> = HashSet::from(['S', 'E', 'F', 'Q']);
    let start: Nonterminal = 'S';
    let mut rules:  HashMap<Nonterminal, HashSet<Word>> = HashMap::new();
    rules.insert('S', HashSet::from([vec![Symbol::Nonterminal('E')]]));
    rules.insert('E', HashSet::from([
        vec![Symbol::Nonterminal('E'), Symbol::Nonterminal('Q'), Symbol::Nonterminal('F')],
        vec![Symbol::Nonterminal('F')]
    ]));
    rules.insert('F', HashSet::from([vec![Symbol::Terminal('a')]]));
    rules.insert('Q', HashSet::from([
        vec![Symbol::Terminal('+')],
        vec![Symbol::Terminal('-')],
    ]));
    Grammar::new(terminals, nonterminals, start, rules)
}

pub fn odd_number_of_a_grammar() -> Grammar {
    let terminals: HashSet<Terminal> = HashSet::from(['a']);
    let nonterminals: HashSet<Nonterminal> = HashSet::from(['S']);
    let start: Nonterminal = 'S';
    let mut rules: HashMap<Nonterminal, HashSet<Word>> = HashMap::new();
    rules.insert('S', HashSet::from([
        vec![Symbol::Terminal('a'), Symbol::Nonterminal('S'), Symbol::Terminal('a')],
        vec![Symbol::Terminal('a')]
    ]));
    Grammar::new(terminals, nonterminals, start, rules)
}

pub fn even_a_middle_b_grammar() -> Grammar {
    let terminals: HashSet<Terminal> = HashSet::from(['a', 'b']);
    let nonterminals: HashSet<Nonterminal> = HashSet::from(['S']);
    let start: Nonterminal = 'S';
    let mut rules: HashMap<Nonterminal, HashSet<Word>> = HashMap::new();
    rules.insert('S', HashSet::from([
        vec![Symbol::Terminal('a'), Symbol::Nonterminal('S'), Symbol::Terminal('a')],
        vec![Symbol::Terminal('b')]
    ]));
    Grammar::new(terminals, nonterminals, start, rules)
}

pub fn direct_left_recursive_grammar() -> Grammar {
    let terminals: HashSet<Terminal> = HashSet::from(['a']);
    let nonterminals: HashSet<Nonterminal> = HashSet::from(['A']);
    let start: Nonterminal = 'A';
    let mut rules: HashMap<Nonterminal, HashSet<Word>> = HashMap::new();
    rules.insert('A', HashSet::from([
        vec![Symbol::Nonterminal('A'), Symbol::Terminal('a')],
        vec![Symbol::Epsilon]
    ]));
    Grammar::new(terminals, nonterminals, start, rules)
}

pub fn indirect_left_recursive_grammar() -> Grammar {
    let terminals: HashSet<Terminal> = HashSet::from(['a', 'b']);
    let nonterminals: HashSet<Nonterminal> = HashSet::from(['A', 'B']);
    let start: Nonterminal = 'A';
    let mut rules: HashMap<Nonterminal, HashSet<Word>> = HashMap::new();
    rules.insert('A', HashSet::from([
        vec![Symbol::Nonterminal('B'), Symbol::Terminal('a')],
        vec![Symbol::Terminal('a')],
    ]));
    rules.insert('B', HashSet::from([
        vec![Symbol::Nonterminal('A'), Symbol::Terminal('b')],
        vec![Symbol::Terminal('b')],
    ]));
    Grammar::new(terminals, nonterminals, start, rules) 
}

pub fn even_more_indirect_left_recursive_grammar() -> Grammar {
    let terminals: HashSet<Terminal> = HashSet::from(['a', 'b', 'c']);
    let nonterminals: HashSet<Nonterminal> = HashSet::from(['A', 'B', 'C']);
    let start: Nonterminal = 'A';
    let mut rules: HashMap<Nonterminal, HashSet<Word>> = HashMap::new();
    rules.insert('A', HashSet::from([
        vec![Symbol::Nonterminal('B'), Symbol::Terminal('a')],
        vec![Symbol::Terminal('a')],
    ]));
    rules.insert('B', HashSet::from([
        vec![Symbol::Nonterminal('C'), Symbol::Terminal('a')],
        vec![Symbol::Terminal('b')],
    ]));
    rules.insert('C', HashSet::from([
        vec![Symbol::Nonterminal('A'), Symbol::Terminal('a')],
        vec![Symbol::Terminal('c')],
    ]));
    Grammar::new(terminals, nonterminals, start, rules)
}

pub fn direct_right_recursive_grammar() -> Grammar {
    let terminals: HashSet<Terminal> = HashSet::from(['a']);
    let nonterminals: HashSet<Nonterminal> = HashSet::from(['A']);
    let start: Nonterminal = 'A';
    let mut rules: HashMap<Nonterminal, HashSet<Word>> = HashMap::new();
    rules.insert('A', HashSet::from([
        vec![Symbol::Terminal('a'), Symbol::Nonterminal('A')],
        vec![Symbol::Terminal('a')],
    ]));
    Grammar::new(terminals, nonterminals, start, rules) 
}

pub fn indirect_right_recursive_grammar() -> Grammar {
    let terminals: HashSet<Terminal> = HashSet::from(['a', 'b']);
    let nonterminals: HashSet<Nonterminal> = HashSet::from(['A', 'B']);
    let start: Nonterminal = 'A';
    let mut rules: HashMap<Nonterminal, HashSet<Word>> = HashMap::new();
    rules.insert('A', HashSet::from([
        vec![Symbol::Terminal('a'), Symbol::Nonterminal('B')],
        vec![Symbol::Terminal('a')],
    ]));
    rules.insert('B', HashSet::from([
        vec![Symbol::Terminal('b'), Symbol::Nonterminal('A')],
        vec![Symbol::Terminal('b')],
    ]));
    Grammar::new(terminals, nonterminals, start, rules)
}

pub fn test_grammar() -> Grammar {
    let terminals: HashSet<Terminal> = HashSet::from(['a']);
    let nonterminals: HashSet<Nonterminal> = HashSet::from(['A']);
    let start: Nonterminal = 'A';
    let mut rules: HashMap<Nonterminal, HashSet<Word>> = HashMap::new();
    rules.insert('A', HashSet::from([
        vec![Symbol::Nonterminal('A'), Symbol::Terminal('a'), Symbol::Nonterminal('A')],
        vec![Symbol::Epsilon],
    ]));
    Grammar::new(terminals, nonterminals, start, rules)
}

pub fn strange_recursive_grammar() -> Grammar {
    let terminals: HashSet<Terminal> = HashSet::from(['a']);
    let nonterminals: HashSet<Nonterminal> = HashSet::from(['S', 'A', 'B']);
    let start: Nonterminal = 'S';
    let mut rules: HashMap<Nonterminal, HashSet<Word>> = HashMap::new();
    rules.insert('S', HashSet::from([
        vec![Symbol::Nonterminal('S'), Symbol::Nonterminal('A'), Symbol::Nonterminal('B')],
        vec![Symbol::Terminal('a'), Symbol::Terminal('a')],
        vec![Symbol::Terminal('a')],
    ]));
    rules.insert('A', HashSet::from([
        vec![Symbol::Terminal('a'), Symbol::Terminal('a')],
        vec![Symbol::Terminal('a')],
    ]));
    rules.insert('B', HashSet::from([
        vec![Symbol::Terminal('a'), Symbol::Terminal('a')],
        vec![Symbol::Terminal('a')],
    ]));
    Grammar::new(terminals, nonterminals, start, rules)
}

pub fn lua_like_grammar() -> Grammar {
    
    let terminals: HashSet<Terminal> = HashSet::from([
        'a', //break
        'b', //for
        'c', //in
        'd', //do
        'e', //end
        'f', //function
        'g', //return
        'i', //if
        'j', //elseif
        'k', //else
        'l', //local
        'm', //nil
        'n', //Name
        'o', //Number
        'p', //false
        'q', //true
        'r', //repeat
        's', //String
        't', //then
        'u', //until
        'w', //while
        'x', //and
        'y', //or
        'z', //not
        ';', 
        ':', 
        ',', 
        '.', 
        '=',
        '+',
        '-',
        '*',
        '/',
        '^',
        '%',
        '<',
        '>',
        '~',
        '#',
        '[',
        ']',
        '(',
        ')',
        '{',
        '}']);
    let nonterminals: HashSet<Nonterminal> = HashSet::from([
        'A', //extraexp
        'B', //block
        'C', //chunk
        'D', //explist
        'E', //exp
        'F', //functioncall
        'G', //funcname
        'H', //funcbody
        'J', //function
        'K', //tableconstructor
        'L', //fieldlist
        'M', //fieldsep
        'N', //namelist
        'O', //args
        'P', //parlist
        'Q', //field
        'R', //laststat
        'S', //stat
        'T', //elseif block
        'U', //else block
        'V', //varlist
        'W', //var
        'X', //prefixexp
        'Y', //binop
        'Z', //unop
        ]);
    let start: Nonterminal = 'C';
    let mut rules: HashMap<Nonterminal, HashSet<Word>> = HashMap::new();
    rules.insert('C', HashSet::from([//chunk
        vec![Symbol::Nonterminal('S'), Symbol::Terminal(';'), Symbol::Nonterminal('C')], //{stat ;}
        vec![Symbol::Nonterminal('L'), Symbol::Terminal(';')], //[laststat ;]
        vec![Symbol::Epsilon],
    ]));
    rules.insert('B', HashSet::from([//block
        vec![Symbol::Nonterminal('C')], //chunk
    ]));
    rules.insert('S', HashSet::from([//stat
        vec![Symbol::Nonterminal('V'), Symbol::Terminal('='), Symbol::Nonterminal('D')], //varlist '=' explist
        vec![Symbol::Nonterminal('F')], //functioncall
        vec![Symbol::Terminal('d'), Symbol::Nonterminal('B'), Symbol::Terminal('e')], //do block end
        vec![Symbol::Terminal('w'), Symbol::Nonterminal('E'), Symbol::Terminal('d'), Symbol::Nonterminal('B'), Symbol::Terminal('e')], //while exp do block end
        vec![Symbol::Terminal('r'), Symbol::Nonterminal('B'), Symbol::Terminal('u'), Symbol::Nonterminal('E')], //repeat block until exp
        vec![Symbol::Terminal('i'), Symbol::Nonterminal('E'), Symbol::Terminal('t'), Symbol::Nonterminal('B'), Symbol::Nonterminal('T'), Symbol::Nonterminal('U'), Symbol::Terminal('e')], //if exp then block {elseif exp then block} [else block] end
        vec![Symbol::Terminal('b'), Symbol::Terminal('n'), Symbol::Terminal('='), Symbol::Nonterminal('E'), Symbol::Terminal(','), Symbol::Nonterminal('E'), Symbol::Terminal('d'), Symbol::Nonterminal('B'), Symbol::Terminal('e')], //for Name '=' exp ',' exp do block end
        vec![Symbol::Terminal('b'), Symbol::Terminal('n'), Symbol::Terminal('='), Symbol::Nonterminal('E'), Symbol::Terminal(','), Symbol::Nonterminal('E'), Symbol::Terminal(','), Symbol::Nonterminal('E'), Symbol::Terminal('d'), Symbol::Nonterminal('B'), Symbol::Terminal('e')], //for Name '=' exp ',' exp ',' exp do block end
        vec![Symbol::Terminal('b'), Symbol::Nonterminal('N'), Symbol::Terminal('c'), Symbol::Nonterminal('D'), Symbol::Terminal('d'), Symbol::Nonterminal('B'), Symbol::Terminal('e')], //for namelist in explist do block end
        vec![Symbol::Terminal('f'), Symbol::Nonterminal('G'), Symbol::Nonterminal('H')], //function funcname funcbody
        vec![Symbol::Terminal('l'), Symbol::Terminal('f'), Symbol::Terminal('n'), Symbol::Nonterminal('H')], // local function Name funcbody
        vec![Symbol::Terminal('l'), Symbol::Nonterminal('N')], //local namelist
        vec![Symbol::Terminal('l'), Symbol::Nonterminal('N'), Symbol::Terminal('='), Symbol::Nonterminal('D')],
    
    ]));
    rules.insert('T', HashSet::from([//elseif
        vec![Symbol::Terminal('j'), Symbol::Nonterminal('E'), Symbol::Terminal('t'), Symbol::Nonterminal('B'), Symbol::Nonterminal('T')],
        vec![Symbol::Epsilon],
    ]));
    rules.insert('U', HashSet::from([//else
        vec![Symbol::Terminal('k'), Symbol::Nonterminal('B')],
        vec![Symbol::Epsilon],
    ]));
    rules.insert('R', HashSet::from([//laststat
        vec![Symbol::Terminal('g')], //return
        vec![Symbol::Terminal('g'), Symbol::Nonterminal('D')], //return explist
        vec![Symbol::Terminal('a')], //break
    ]));
    rules.insert('G', HashSet::from([//funcname
        vec![Symbol::Nonterminal('G'), Symbol::Terminal('.'), Symbol::Terminal('n')],
        vec![Symbol::Terminal('n')],
    ]));
    rules.insert('V', HashSet::from([//varlist LEFT RECURSIVE
        vec![Symbol::Nonterminal('V'), Symbol::Terminal(','), Symbol::Nonterminal('W')],
        vec![Symbol::Nonterminal('W')],
    ]));
    rules.insert('W', HashSet::from([//var
        vec![Symbol::Terminal('n')], //Name
        vec![Symbol::Nonterminal('X'), Symbol::Terminal('['), Symbol::Nonterminal('E'), Symbol::Terminal(']')], //prefixexp '[' exp ']'
        vec![Symbol::Nonterminal('X'), Symbol::Terminal('.'), Symbol::Terminal('n')], //prefixexp '.' Name
    ]));
    rules.insert('N', HashSet::from([//namelist LEFT RECURSIVE
        vec![Symbol::Nonterminal('N'), Symbol::Terminal(','), Symbol::Terminal('n')],
        vec![Symbol::Terminal('n')],
    ]));
    rules.insert('D', HashSet::from([//explist LEFT RECURSIVE
        vec![Symbol::Nonterminal('D'), Symbol::Terminal(','), Symbol::Nonterminal('E')],
        vec![Symbol::Nonterminal('E')],
    ]));
    rules.insert('E', HashSet::from([//exp
        vec![Symbol::Terminal('n')], //nil
        vec![Symbol::Terminal('p')], //false
        vec![Symbol::Terminal('q')], //true
        vec![Symbol::Terminal('o')], //Number
        vec![Symbol::Terminal('s')], //String
        vec![Symbol::Nonterminal('J')], //function
        vec![Symbol::Nonterminal('X')], //prefixexp
        vec![Symbol::Nonterminal('K')], //tableconstructor
        vec![Symbol::Nonterminal('E'), Symbol::Nonterminal('Y'), Symbol::Nonterminal('E')], //exp binop exp
        vec![Symbol::Nonterminal('Z'), Symbol::Nonterminal('E')], //unop exp
    ]));
    rules.insert('X', HashSet::from([//prefixexp
        vec![Symbol::Nonterminal('W')], //var
        vec![Symbol::Nonterminal('F')], //functioncall
        vec![Symbol::Terminal('('), Symbol::Nonterminal('E'), Symbol::Terminal(')')], //'(' exp ')'
    ]));
    rules.insert('F', HashSet::from([//functioncall
        vec![Symbol::Nonterminal('X'), Symbol::Nonterminal('O')], //prefixexp args
        vec![Symbol::Nonterminal('X'), Symbol::Terminal(':'), Symbol::Terminal('n'), Symbol::Nonterminal('O')], //prefixexp ':' Name args
    ]));
    rules.insert('O', HashSet::from([//args
        vec![Symbol::Terminal('('), Symbol::Terminal(')')], //'(' ')'
        vec![Symbol::Terminal('('), Symbol::Nonterminal('D'), Symbol::Terminal(')')], //'(' explist ')'
        vec![Symbol::Nonterminal('K')], //tableconstructor
        vec![Symbol::Terminal('s')], //String
    ]));
    rules.insert('J', HashSet::from([//function
        vec![Symbol::Terminal('f'), Symbol::Nonterminal('H')], //function funcbody
    ]));
    rules.insert('H', HashSet::from([//funcbody
        vec![Symbol::Terminal('('), Symbol::Terminal(')'), Symbol::Nonterminal('B'), Symbol::Terminal('e')], //'(' ')' block end
        vec![Symbol::Terminal('('), Symbol::Nonterminal('P'), Symbol::Terminal(')'), Symbol::Nonterminal('B'), Symbol::Terminal('e')], //'(' parlist ')' block end
    ]));
    rules.insert('P', HashSet::from([//parlist
        vec![Symbol::Nonterminal('N')], //namelist no support for varargs
    ]));
    rules.insert('K', HashSet::from([//tableconstructor
        vec![Symbol::Terminal('{'), Symbol::Terminal('}')], //'{' '}'
        vec![Symbol::Terminal('{'), Symbol::Nonterminal('L'), Symbol::Terminal('}')], //'{' fieldlist '}'
    ]));
    rules.insert('L', HashSet::from([//fieldlist LEFT RECURSIVE
        vec![Symbol::Nonterminal('L'), Symbol::Nonterminal('M'), Symbol::Nonterminal('Q')], //fieldsep field
        vec![Symbol::Nonterminal('Q')], //field
    ]));
    rules.insert('Q', HashSet::from([//field
        vec![Symbol::Terminal('['), Symbol::Nonterminal('E'), Symbol::Terminal(']'), Symbol::Terminal('='), Symbol::Nonterminal('E')], //'[' exp ']' '=' exp
        vec![Symbol::Terminal('n'), Symbol::Terminal('='), Symbol::Nonterminal('E')], //Name '=' exp
        vec![Symbol::Nonterminal('E')], //exp
    ]));
    rules.insert('M', HashSet::from([//fieldsep
        vec![Symbol::Terminal(',')], //','
        vec![Symbol::Terminal(';')], //';'
    ]));
    rules.insert('Y', HashSet::from([//binop
        vec![Symbol::Terminal('+')], //'+'
        vec![Symbol::Terminal('-')], //'-'
        vec![Symbol::Terminal('*')], //'*'
        vec![Symbol::Terminal('/')], //'/'
        vec![Symbol::Terminal('^')], //'^'
        vec![Symbol::Terminal('%')], //'%'
        vec![Symbol::Terminal('<')], //'<'
        vec![Symbol::Terminal('>')], //'>'
        vec![Symbol::Terminal('~')], //'~='
        vec![Symbol::Terminal('x')], //and
        vec![Symbol::Terminal('y')], //or
    ]));
    rules.insert('Z', HashSet::from([//unop
        vec![Symbol::Terminal('-')], //'-'
        vec![Symbol::Terminal('z')], //not
        vec![Symbol::Terminal('#')], //'#'
    ]));
    Grammar::new(terminals, nonterminals, start, rules)
}

pub fn another_broken_grammar() -> Grammar {
    // A -> B | C
    // B -> D | B b
    // C -> D | C c
    // D -> a
    // ((a(b)*) + (a(c)*))
    // (a + a) (b + c)*  
    let terminals: HashSet<Terminal> = HashSet::from(['a', 'b', 'c']);
    let nonterminals: HashSet<Nonterminal> = HashSet::from(['A', 'B', 'C', 'D']);
    let start: Nonterminal = 'A';
    let mut rules: HashMap<Nonterminal, HashSet<Word>> = HashMap::new();
    rules.insert('A', HashSet::from([
        vec![Symbol::Nonterminal('B')],
        vec![Symbol::Nonterminal('C')],
    ]));
    rules.insert('B', HashSet::from([
        vec![Symbol::Nonterminal('D')],
        vec![Symbol::Nonterminal('B'), Symbol::Terminal('b')],
    ]));
    rules.insert('C', HashSet::from([
        vec![Symbol::Nonterminal('D')],
        vec![Symbol::Nonterminal('C'), Symbol::Terminal('c')],
    ]));
    rules.insert('D', HashSet::from([
        vec![Symbol::Terminal('a'), Symbol::Terminal('a')],
    ]));
    Grammar::new(terminals, nonterminals, start, rules)
}