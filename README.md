# Lark

LALR parser implementation for Julia, based on the popular [Lark for Python](https://github.com/lark-parser/lark/)

It's currently working and useable, but still work in progress. Especially in terms of usability and performance

Features:

- LALR(1) parser (runs in O(n) time)
- EBNF Grammar
- Can build a parse-tree automatically, no construction code required
- Automatic line & column tracking
- Automatic terminal collision resolution (doesn't work for regexps colliding with other regexps)
- Standard library of terminals (strings, numbers, names, etc.)


### How to use

1.  Install Lark on Python

```bash
$ pip install lark-parser
```

2. Generate a JSON serialization
```bash
$ python -m lark.tools.serialize my_grammar.lark -o my_grammar.json
```

3. Import the Lark module in Julia and use the parser
```julia
import Lark
lark = Lark.load_json_file("my_grammar.json")
callback = Dict(
	"rule1" => some_function1,
	"rule2" => some_function2
    )
tree = Lark.lalr_parse(lark, "some string of input", "start", callbacks)
```

