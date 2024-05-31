# Haskell Parser Combinators Library

This very minimal library provides a powerful set of tools for building efficient, expressive, and flexible parsers in Haskell. Leveraging the concept of monadic parser combinators based on this beautiful [paper](https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf) by Graham Hutton, it allows you to construct complex parsers from simpler ones, making your code modular, reusable, and easy to understand.

## Features

- **Comprehensive Error Handling**: Detailed error reporting helps pinpoint issues during parsing.
- **Modular Design**: Build complex parsers from simple components, promoting reusability.
- **Flexible Parsing Capabilities**: Handle various parsing scenarios with ease.
- **Monad, Functor, and Applicative Instances**: Utilize standard Haskell abstractions for composability and flexibility.

## Getting Started

### Prerequisites

Ensure you have Haskell installed on your system. GHC (Glasgow Haskell Compiler) version 8.0 or later is recommended.

### Installation

Clone the repository and navigate to the project directory:

```bash
git clone https://github.com/akatau/tiny-parsec.git
cd tiny-parsec
```

Add the project to your `stack.yaml` or `.cabal` file, depending on your setup.

### Usage

Here's a basic example to demonstrate how to use the library:

```haskell
import YourLibrary.TinyParsec

-- Define a simple parser that matches digits
digitParser :: Parser Int
digitParser = satisfyParser isDigit >>= return. digitToInt

-- Combine parsers to parse integers
integerParser :: Parser Int
integerParser = many1 digitParser

main :: IO ()
main = do
    let result = runParser integerParser "12345"
    print result
```

This example demonstrates defining a parser for digits (`digitParser`) and combining it with `many1` to parse integers (`integerParser`). The `runParser` function is used to execute the parser against an input string.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

