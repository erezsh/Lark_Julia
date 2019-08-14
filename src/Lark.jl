module Lark

import JSON

export Tree, pretty, lalr_parse, lalr_parse_file, load_lark_json, load_lark_json_file, IndentConf, indenter

# Utils

Maybe{T} = Union{T, Nothing}

function escape_regex(s::AbstractString)
    res = replace(s, r"([()[\]{}?*+\-|^\$\\.&~#\s=!<>|:])" => s"\\\1")
    replace(res, "\0" => "\\0")
end

struct RegexString
    value :: String
end
Base.show(r::RegexString) = show(r.value)
Base.length(r::RegexString) = length(r.value)


# ===================
#       Trees
# ===================

struct Tree
    data::Any
    children::Array{Any}
end

pretty(t::Tree, indent) = string(pretty(t.data, indent), [pretty(c, indent+1) for c in t.children]...)
pretty(x, indent) = string(repeat(" ", indent*2), x, "\n")
pretty(x) = pretty(x, 0)

# ===================
#       Lexer
# ===================

TokenType = String

PatternValue = Union{String, RegexString}
struct Pattern
    value::PatternValue
    flags::Array{String}
    width::Tuple{Number, Number}

end

Pattern(v::String) = Pattern(v, [], (length(v), length(v)))
Pattern(v::String, flags) = Pattern(v, flags, (length(v), length(v)))
Pattern(v::RegexString) = Pattern(v, [], (0, Inf))
# Pattern(v::RegexString, flags, width) = Pattern(v, flags, width)
# convert(::Type{Pattern}, r::String) = Pattern(
# convert(::Type{Pattern}, r::RegexString) = Pattern(r, [], (0, Inf))

struct TerminalDef
    type::TokenType
    pattern::Pattern
    priority::Int

end
TerminalDef(t,p) = TerminalDef(t,p,1)

mutable struct Token
    type::TokenType
    value:: String
    pos_in_stream::Int
    line::Int
    column::Int
    end_line::Int
    end_column::Int

end
Token(t,v,p,l,c) = Token(t,v,p,l,c,0,0)
Token(t,v) = Token(t,v,0,0,0)

pretty(t::Token, indent) = pretty(string(t.type, " = '", escape_string(t.value), "'"), indent)

mutable struct LineCounter
    char_pos::Int
    line::Int
    column::Int
    line_start_pos::Int
    newline_char::Char

    LineCounter() = new(1, 1, 1, 0, '\n')
end

function feed(lc::LineCounter, value::AbstractString, test_newline::Bool=true)
    if test_newline
        newlines = count(c-> c == lc.newline_char, value)
        if newlines > 0
            lc.line += newlines
            lc.line_start_pos = lc.char_pos + findlast(string(lc.newline_char), value).start + 1
        end
    end

    lc.char_pos += sizeof(value)
    lc.column = lc.char_pos - lc.line_start_pos + 1
end



_escape(s::String) = escape_regex(s)
_escape(s::RegexString) = s.value

function to_regex(p::Pattern) :: String
    prefixes = ["(?$f)" for f in p.flags]
    string(string(prefixes...), _escape(p.value))
end
to_regex(t::TerminalDef) = to_regex(t.pattern)




struct LexerConf
    terminals::Array{TerminalDef}
    ignore::Array{TokenType}
    # user_callbacks::Dict
end

struct Lexer
    terminals::Array{TerminalDef}
    regex::Regex
    callback::Dict{String,Function}
    newline_types::Array{TokenType}
    ignore_types::Array{TokenType}
end

function classify(a::Array,pred)
    res=Dict()
    for x in a
      push!(get!(res,pred(x),[]),x) 
    end
    res
end

function unless_callback(d)
    function (t::Token)
        t.type = get(d, t.value, t.type)
        t
    end
end

function _create_unless(terminals)
    tokens_by_type = classify(terminals, t->typeof(t.pattern.value))
    @assert length(tokens_by_type) <= 2
    embedded_strs = Set()
    callback = Dict{TokenType, Function}()
    for retok in get(tokens_by_type, RegexString, [])
        unless = Dict{String, TokenType}()
        for strtok in get(tokens_by_type, String, [])
            if strtok.priority > retok.priority
                continue
            end
            s = strtok.pattern.value
            re = Regex( string("^", to_regex(retok.pattern)) )
            m = match(re, s)
            if m !== nothing && m.match == s
                unless[s] = strtok.type
                if strtok.pattern.flags <= retok.pattern.flags
                    push!(embedded_strs,strtok)
                end
            end
        end

        if length(unless) > 0
            callback[retok.type] = unless_callback(unless) #UnlessCallback(build_mres(unless, match_whole=True))
        end
    end

    terminals = [t for t in terminals if !(t in embedded_strs)]
    return terminals, callback
end


_has_newline(r::AbstractString) = '\n' in r || occursin("\\n", r) || occursin("\\s", r) || occursin("[^", r) || (occursin("(?s", r) && '.' in r)
_has_newline(r::Regex) = _has_newline(r.pattern)
function Lexer(conf :: LexerConf)
    newline_types = [t.type for t in conf.terminals if _has_newline(to_regex(t))]
    ignore_types = conf.ignore

    terminals = sort(conf.terminals, by=x -> (-x.priority, -x.pattern.width[1], -length(x.pattern.value), x.type))

    terminals, callback = _create_unless(terminals)

    regex = Regex( string("^(?:", join( ["(?P<$(t.type)>$(to_regex(t)))" for t in terminals], "|" ), ")" ) )

    Lexer(terminals, regex, callback, newline_types, conf.ignore)
end


Base.match(lexer::Lexer, stream::String, pos::Int) = match(lexer.regex, SubString(stream, pos))

function find_match(m::RegexMatch, types::Array{TokenType}) :: TokenType
    # XXX Yach! So bad! Julia you should be ashamed
    for t in types
        if m[t] !== nothing
            return t
        end
    end
end

function lex(lexer, stream::AbstractString, newline_types, ignore_types) 
    tokens = Token[]
    terminal_types::Array{TokenType} = [t.type for t in lexer.terminals]

    line_ctr = LineCounter()
    # last_token = nothing

    len = sizeof(stream)
    while line_ctr.char_pos <= len
        m = match(lexer, stream, line_ctr.char_pos)
        if m === nothing
            throw("error")
            # allowed = {v for m, tfi in lexer.mres for v in tfi.values()}
            # raise UnexpectedCharacters(stream, line_ctr.char_pos, line_ctr.line, line_ctr.column, allowed=allowed, state=self.state, token_history=last_token and [last_token])
        end

        t = nothing
        value = m.match
        type::TokenType = find_match(m, terminal_types)
        if !(type in ignore_types)
            t = Token(type, value, line_ctr.char_pos, line_ctr.line, line_ctr.column)
            if haskey(lexer.callback, t.type)
                t = lexer.callback[t.type](t)
                isa(t, Token) || throw(("Callbacks must return a token (returned %r)" % t))
            end
            # last_token = t
            push!(tokens, t)
        else   # Ignored
            if haskey(lexer.callback, type)
                t = Token(type_, value, line_ctr.char_pos, line_ctr.line, line_ctr.column)
                lexer.callback[type_](t)
            end
        end

        feed(line_ctr, value, type in newline_types)
        if t !== nothing
            t.end_line = line_ctr.line
            t.end_column = line_ctr.column
        end

    end
    tokens
end
lex(lexer, stream) = lex(lexer, stream, lexer.newline_types, lexer.ignore_types)


struct IndentConf
    indent_type::String
    dedent_type::String
    newline_type::String
    open_parens_types::Array{String}
    close_parens_types::Array{String}
    tab_len::Int
end
IndentConf(i,d,o,c,n) = IndentConf(i,d,o,c,n,8)

function indenter(tokens::Array{Token}, conf::IndentConf)::Array{Token}
    paren_level = 0
    indent_level = [0]
    @assert conf.tab_len > 0

    new_tokens = Token[]
    for token in tokens
        if token.type == conf.newline_type
            if paren_level > 0
                continue
            end

            push!(new_tokens, token)

            indent_str = rsplit(token.value, '\n')[end] # Tabs and spaces
            indent = count(c->c==' ', indent_str) + count(c->c=='\t', indent_str) * conf.tab_len

            if indent > indent_level[end]
                push!(indent_level, indent)
                # yield Token.new_borrow_pos(self.INDENT_type, indent_str, token)
                push!(new_tokens, Token(conf.indent_type, indent_str))
            else
                while indent < indent_level[end]
                    pop!(indent_level)
                    # push!(new_tokens, Token.new_borrow_pos(self.DEDENT_type, indent_str, token)
                    push!(new_tokens, Token(conf.dedent_type, indent_str))
                end
                @assert indent == indent_level[end]
            end
        else
            push!(new_tokens, token)
        end

        if token.type in conf.open_parens_types
            paren_level += 1
        elseif token.type in conf.close_parens_types
            paren_level -= 1
            @assert paren_level >= 0
        end
    end

    while length(indent_level) > 1
        pop!(indent_level)
        push!(new_tokens, Token(conf.dedent_type, ""))
    end

    @assert indent_level == [0]
    new_tokens
end

    # XXX Hack for ContextualLexer. Maybe there's a more elegant solution?
    # @property
    # def always_accept(self):
    #     return (self.NL_type,)


## Parser

struct Terminal
    name::String
    filter_out::Bool
end
struct NonTerminal
    name::String
end

# is_term(::Terminal) = true
# is_term(::NonTerminal) = false


RuleElement = Union{Terminal, NonTerminal}


struct RuleOptions
    keep_all_tokens::Bool
    expand1::Bool
    priority::Maybe{Int}
    empty_indices::Array{Int}
end


struct Rule
    origin::NonTerminal
    expansion::Array{RuleElement}
    order::Int
    alias::Maybe{String}
    options::Maybe{RuleOptions}
end


struct Shift
    state::Int
end
struct Reduce
    rule::Rule
end

Action = Union{Shift, Reduce}


struct ParseTable
    states::Dict{Int,Dict{TokenType,Action}}
    start_states::Dict{String,Int}
    end_states::Dict{String,Int}
end


struct LALR_TraditionalLexer
    lexer_conf::LexerConf
    parse_table::ParseTable
    start::Array{String}
    lexer::Lexer

    LALR_TraditionalLexer(conf,table,start) = new(conf,table,start,Lexer(conf))
end

function _lalr_parse(conf::LALR_TraditionalLexer, tokens::Array{Token}, start::String, callback::Dict)
    # token::Token = nothing
    table = conf.parse_table

    states = table.states

    start_state = table.start_states[start]
    end_state = table.end_states[start]

    state_stack = Int[start_state]
    value_stack = []

    # if set_state: set_state(start_state)

    function get_action(token::Token) :: Action
        state = state_stack[end]
        try
            states[state][token.type]
        catch KeyError
            # if !(haskey(states[state], token.type))
            # expected = [s for s in states[state].keys() if s.isupper()]
            # raise UnexpectedToken(token, expected, state=state)
            throw("UnexpectedToken: $(token.type) = $(token.value) ($token). Expected: $(keys(states[state]))")
        end

    end

    function reduce(rule::Rule)
        size = length(rule.expansion)
        if size > 0
            sl = length(state_stack)
            vl = length(value_stack)

            s = value_stack[end+1-size:end]

            deleteat!(state_stack, sl+1-size:sl)
            deleteat!(value_stack, vl+1-size:vl)
            # resize!(state_stack, sl-size)
            # resize!(value_stack, vl-size)
        else
            s = []
        end

        value = callback[rule](s)

        action = states[state_stack[end]][rule.origin.name]
        @assert(isa(action, Shift))
        push!(state_stack, action.state)
        push!(value_stack, value)
    end

    # Main LALR-parser loop
    for token in tokens
        while true
            action = get_action(token)
            if isa(action, Shift)
                state = action.state
                @assert(state != end_state)
                push!(state_stack, state)
                push!(value_stack, token)
                break
            else
                reduce(action.rule)
            end
        end
    end

    # token = Token.new_borrow_pos('$END', '', token) if token else Token('$END', '', 0, 1, 1)
    token = Token(s"$END", "")
    while true
        action = get_action(token)
        if isa(action, Shift)
            state = action.state
            @assert(state == end_state)
            val ,= value_stack
            return val
        else
            reduce(action.rule)
        end
    end
end


## ===========================
##       Tree builder
## ===========================

struct TreeBuilderOptions
    tree_class
    propagate_positions::Bool
    always_keep_all_tokens::Bool
    ambiguous::Bool
    maybe_placeholders::Bool
end
TreeBuilderOptions() = TreeBuilderOptions(Tree,false,false,false,false)

_expand_single_child_wrapper(node_builder) = children -> length(children) == 1 ? children[1] : node_builder(children)

_should_expand(sym::Terminal) = false
_should_expand(sym::NonTerminal) = startswith(sym.name, "_")
_to_include(sym::Terminal) = !sym.filter_out
_to_include(sym::NonTerminal) = true

function _child_filter(to_include)
    node_builder -> function f(children)
        filtered = []
        for (i, to_expand) in to_include
            if to_expand
                if length(filtered) > 0
                    filtered = [filtered; children[i].children] # TODO optimize with in-place extend?
                else   # Optimize for left-recursion
                    filtered = children[i].children
                end
            else
                push!(filtered, children[i])
            end
        end
        node_builder(filtered)
    end
end


function maybe_create_child_filter(expansion)
    to_include = [(i, _should_expand(sym)) for (i, sym) in enumerate(expansion) if _to_include(sym)]

    if length(to_include) < length(expansion) || any(to_expand for (i,to_expand) in to_include)
        return _child_filter(to_include)
    else
        return nothing
    end
end

function _create_tree_builder(rule:: Rule, options::TreeBuilderOptions) :: Array{Function}
    @assert !options.always_keep_all_tokens
    @assert !options.ambiguous
    @assert !options.maybe_placeholders

    ro = rule.options
    keep_all_tokens = options.always_keep_all_tokens || (ro !== nothing ? ro.keep_all_tokens : false)
    expand_single_child = ro !== nothing ? ro.expand1 : false

    wrapper_chain = []
    if expand_single_child && rule.alias === nothing
        push!(wrapper_chain, _expand_single_child_wrapper)
    end

    maybe_child_filter = maybe_create_child_filter(rule.expansion)
    if maybe_child_filter !== nothing
        push!(wrapper_chain, maybe_child_filter)
    end

    wrapper_chain
end

function create_callback(rules, user_callback)
    callbacks = Dict()

    options = TreeBuilderOptions()

    # for rule, wrapper_chain in rules
    for rule in rules
        user_callback_name = rule.alias !== nothing ? rule.alias : rule.origin.name
        n2 = string(user_callback_name, "[]")
        if haskey(user_callback, user_callback_name)
            user_f = user_callback[user_callback_name]
            f = (tail::Array -> user_f(tail...))
        elseif haskey(user_callback, n2)
            f = user_callback[n2]
        else
            f = (tail::Array) -> options.tree_class(user_callback_name, tail)
        end

        for b in _create_tree_builder(rule, options)
            f = b(f)
        end

        @assert(!haskey(callbacks, rule))
        callbacks[rule] = f
    end

    callbacks
end



## Loader

load_obj(nothing) = nothing
function load_obj(obj::Dict)
    t = obj["__type__"]
    if t == "NonTerminal"
            return NonTerminal(obj["name"])
    elseif t == "Terminal"
            return Terminal(obj["name"], obj["filter_out"])
    elseif t == "PatternRE"
            return Pattern(RegexString(obj["value"]), obj["flags"], Tuple{Int, Int}(obj["_width"]))
    elseif t == "PatternStr"
            return Pattern(obj["value"], obj["flags"])

    elseif t == "TerminalDef"
            pattern = load_obj(obj["pattern"])
            return TerminalDef(obj["name"], pattern, obj["priority"])
    elseif t == "Rule"
            options = load_obj(get(obj, "options", nothing))
            return Rule(load_obj(obj["origin"]), load_obj.(obj["expansion"]), obj["order"], obj["alias"], options)
    elseif t == "RuleOptions"
            return RuleOptions(obj["keep_all_tokens"], obj["expand1"], obj["priority"], obj["empty_indices"])
    else
        throw("error")
    end
end


struct LarkConf
    parser::LALR_TraditionalLexer
    rules::Array{Rule}
    options::Dict
end


ActionTable = Dict{TokenType, Action}

load_memo(memo_json) = Dict{Int, Any}(
        parse(Int,key) => load_obj(obj_json)
        for (key, obj_json) in memo_json
    )


function load_lark_json_file(json_file::String) :: LarkConf
    open(json_file) do file
        load_lark_json( JSON.parse(read(file, String)) )
    end
end

function load_lark_json(g::Dict) :: LarkConf
    data_json = (g["data"])
    memo_json = (g["memo"])

    # show(memo)
    memo = load_memo(memo_json)

    function load_obj2(obj)
        t = obj["__type__"]
        if t == "Lark"
            return LarkConf(load_obj2(obj["parser"]), map(x->memo[x["@"]], obj["rules"]), obj["options"])
        elseif t == "LALR_ContextualLexer"
            # parse_table = load_parse_table(obj.parser)
            # return LALR_ContextualLexer(load_obj2(obj.lexer_conf), parse_table, obj.start)
            throw("Not implemented")
        elseif t == "LALR_TraditionalLexer"
            parse_table = load_parse_table(obj["parser"])
            lexer_conf::LexerConf = load_obj2(obj["lexer_conf"])
            return LALR_TraditionalLexer(lexer_conf, parse_table, obj["start"])
        elseif t == "LexerConf"
            terminals = map(x->memo[x["@"]], obj["tokens"])
            return LexerConf(terminals, obj["ignore"])
        else
            throw(obj)
        end
    end

    function load_action(action) :: Action
        action_type, arg = action
        if action_type == 0
            Shift(arg)
        elseif action_type == 1
            rule_id = arg["@"]
            Reduce(memo[rule_id])
        else
            throw(action)
        end
    end

    function load_parse_table(obj) :: ParseTable
        tokens = obj["tokens"]

        states = Dict{Int, ActionTable}()
        for (state, actions) in obj["states"]

            states[parse(Int,state)] = ActionTable(
                tokens[token] => load_action(action) for (token, action) in actions
            )
        end

        start_states::Dict{String,Int} = obj["start_states"]
        end_states::Dict{String,Int} = obj["end_states"]

        ParseTable(states, start_states, end_states)
    end

    load_obj2(data_json)
end


# =====================
#       Interface
# =====================


function lalr_parse(lark::LarkConf, text::Union{AbstractString,IOStream}, callbacks::Dict=Dict(); start::Maybe{String}=nothing, postlex=nothing)
    if start === nothing
        @assert length(lark.options["start"]) == 1
        start = lark.options["start"][1]
    end
    if isa(text, IOStream)
        text = read(text, String)
    end
    callback = create_callback(lark.rules, callbacks)
    tokens = lex(lark.parser.lexer, text)
    if postlex !== nothing
        tokens = postlex(tokens)
    end
    _lalr_parse(lark.parser, tokens, start, callback)
end

function lalr_parse_file(lark::LarkConf, input_file::AbstractString, callbacks::Dict=Dict(); start::Maybe{String}=nothing, postlex=nothing)
    open(input_file) do file
        lalr_parse(lark, file, callbacks, start=start, postlex=postlex)
    end
end

end # module
