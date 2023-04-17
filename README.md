# Pratt-parser
Pratt parsing algorithm based compiler for expressions written in Rust.

# Usage
```bash
git clone https://github.com/LLayta/Pratt-parser ; cd Pratt-parser ; cargo r
```

# Pratt parsing
Pratt parsing is a top-down parsing techinque for operator precedence parsing. These parsers can only parse a very small subset of context-free grammars. It's very similar to recursive descent parsing but instead of associating functions to non-terminals, it associates functions to tokens themselves. The philosophy behind pratt parsing is assigning precedence levels to sub-expressions inside of an expression, it'll parse those sub-expressions first then fetch the left binding power of a given operator and start to parse that. The parsing function takes the left-hand side of an expression and a precedence level, it'll loop over the tokens until the end is reached. For each token the parser will fetch it's binding power (precedence level) and if it's higher than the current precedence level then it'll recall the function with the new precedence. Once a lower precedence is found, it'll call the appropriate function for parsing what's been found. Some of these functions include: null denomination function, left denomination function and more. These are cryptic words that Vaughan Pratt used in his original paper, these are basically just functions that parse prefix, postfix, and infix expressions.

# Why's it good?
Pratt parsing shines so much because of it's simplistic elegance but good efficiency, I find it very beautiful for its modularity, you can very trivially add support for new operations by adding them into the binding power lookup table or function.

# References
Wikipedia - https://en.wikipedia.org/wiki/Operator-precedence_parser#Pratt_parsing 

Rust implementation - https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html

Eli bendersky implementation - https://eli.thegreenplace.net/2010/01/02/top-down-operator-precedence-parsing/

Typescript implementation for desmos - https://engineering.desmos.com/articles/pratt-parser/
