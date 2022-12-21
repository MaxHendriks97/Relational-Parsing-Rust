# Relational-Parsing-Rust
Rust implementation based on Herman's Relational Parsing: https://doi.org/10.1145/3385412.3386032

As of now, the package cannot be run as a stand-alone program. The code can be cun using the tests supplied in integration_tests.rs under the tests folder.
This implementation as of now unly supports char types as terminal and nonterminal symbols. This type can be changed in a single location in the code, but changing this value to another data type may require further code alterations.
