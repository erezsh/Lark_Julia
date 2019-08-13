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
    g = open("json.json") do file
        JSON.parse(read(file, String))
    end
    s = open("test.json") do file
        read(file, String)
    end
    print("Start")

    @time lark = load_json(g)
    @time res = lalr_parse(lark, s, "start", callbacks)
    # print(pretty(res))
end



# test_lexer()
# test_parser()
# test_json()

function test_python()
    callbacks = Dict()

    g = open("python3.json") do file
        JSON.parse(read(file, String))
    end
    s = open("calc.py") do file
        read(file, String)
    end

    indent_conf = IndentConf(
        "_INDENT",
        "_DEDENT",
        ["LPAR", "LSQB", "LBRACE"],
        ["RPAR", "RSQB", "RBRACE"],
        "_NEWLINE",
    )

    @time lark = load_json(g)
    @time res = lalr_parse(lark, s, "file_input", callbacks, indent_conf)
    @time res = lalr_parse(lark, s, "file_input", callbacks, indent_conf)
    # show(res)
end


@test test_lexer() === nothing
@test isa(test_json(), Array)
@test isa(test_python(), Tree)
end