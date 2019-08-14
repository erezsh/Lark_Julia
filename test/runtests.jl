import JSON
using Lark
using Lark: Pattern, RegexString, TerminalDef, LexerConf, Lexer, lex
using Test

@testset "Lark.jl" begin

function test_lexer()
    terminals = [
        TerminalDef("NAME", Pattern(RegexString("[A-Za-z]\\w*"))),
        TerminalDef("NUMBER", Pattern(RegexString("\\d+"))),
        TerminalDef("EQ", Pattern(String("="))),
        TerminalDef("DOT", Pattern(String("."))),
        TerminalDef("WS", Pattern(RegexString("\\s"))),
    ]
    conf = LexerConf(terminals, ["WS"])
    lexer = Lexer(conf)
    for tok in lex(lexer, "Hello =\n 3.14")
        print("\n")
        show(tok)
    end
end




function test_json()

    callbacks = Dict(
        "number" => num -> parse(Float64, num.value),
        "string" => str -> str.value[2:end-1],
        "pair" => Pair,
        "array[]" => Array,
        "object[]" => Dict,

        "null" => () -> nothing,
        "false" => () -> false,
        "true" => () -> true,
    )

    # res = lalr_parse(lark, "{\"a\":[1,2, 3.4, false, true, null]}", "start", callbacks)
    @time lark = load_lark_json_file("json.json")
    @time res = lalr_parse_file(lark, "test.json", callbacks)

    # print(pretty(res))
end

function test_python()
    indent_conf = IndentConf(
        "_INDENT",
        "_DEDENT",
        "_NEWLINE",
        ["LPAR", "LSQB", "LBRACE"],
        ["RPAR", "RSQB", "RBRACE"],
    )

    @time lark = load_lark_json_file("python3.json")
    @time res = lalr_parse_file(lark, "calc.py", postlex=tokens->indenter(tokens, indent_conf))
    # show(res)
end


@test test_lexer() === nothing
@test isa(test_json(), Array)
@test isa(test_python(), Tree)
end