%%%-----------------------------------------------------------------------------
%% @doc Set of helper utility functions, for internal use.
%%
%% @author Andrey Latnikov <alatnikov@alertlogic.com>
%% @copyright 2013 Alert Logic, Inc.
%%%-----------------------------------------------------------------------------
-module(jesse_utils).
-author("Andrey Latnikov <mailto:alatnikov@alertlogic.com>").

%% API
-export([explain/1, explain/2, collect/3, failfast/3]).

%% @doc Utility function to be used as a dummy accumulator callback which fails
%%      on the first error.
%% @private
failfast(_Path, Error, _) ->
    throw(Error).


%% @doc Utility function to accumulate errors found during validation.
%% @private
collect(StringPath, Error, List) ->
    [{StringPath, Error} | List].


%% @doc Explain list of errors in the internal format and return
%%      <code>iolist()</code> in human-readable format.
explain(Errors) ->
    explain(Errors, fun encode/1).

explain(Errors, Encoder)->
    [ [ Path, " @ "
      , format_error(Error, Encoder)
      , io_lib:nl()]
      || {Path, Error} <- Errors
    ].


%%% ----------------------------------------------------------------------------
%%% Internal functions
%%% ----------------------------------------------------------------------------

format_error({data_invalid, Schema, wrong_type, Value}, Encoder) ->
    case json_path("type", Schema) of
        UniounType when is_list(UniounType) ->
            Args = [get_desc(Schema), Encoder(Value)],
            print_tabbed_line("~s: ~s [bad type (check schema)]", Args);
        Type ->
            Args = [get_desc(Schema), Encoder(Value), Type],
            print_tabbed_line("~s: ~s [must be an ~s]", Args)
    end;

format_error( {data_invalid, Schema, no_extra_properties_allowed, Value}
            ,  Encoder
            ) ->
    Allowed  = get_props(json_path("properties", Schema)),
    ValProps = get_props(Value),
    Props = string:join(lists:map(Encoder, ValProps -- Allowed), ", "),
    print_tabbed_line( "~s has unknown properties: [~s]",
                       [get_desc(Schema), Props] );

format_error( {data_invalid, Schema, {missing_required_property, Prop}, _}
            , Encoder
            ) ->
    print_tabbed_line( "~s missing property: ~s",
                       [get_desc(Schema), Encoder(Prop)] );

format_error({data_invalid, Schema, {not_unique, Item}, _Value}, Encoder) ->
    print_tabbed_line( "~s: ~s [must be unique]",
                       [get_desc(Schema), Encoder(Item)] );

format_error( {data_invalid, Schema, no_extra_items_allowed, _Value}
            , _Encoder
            ) ->
    print_tabbed_line("~s has unexpected items", [get_desc(Schema)]);

format_error({data_invalid, Schema, not_enough_items, _Value}, _Encoder) ->
    print_tabbed_line("~s has not enough items", [get_desc(Schema)]);

format_error({data_invalid, Schema, {no_match, Pattern}, Value}, Encoder) ->
    Args = [get_desc(Schema), Encoder(Value), Encoder(Pattern)],
    print_tabbed_line("~s: ~s [must match ~s]", Args);

format_error({data_invalid, Schema, wrong_format, Value}, Encoder) ->
    Format = json_path("format", Schema),
    Args = [get_desc(Schema), Encoder(Value), Encoder(Format)],
    print_tabbed_line("~s: ~s [must be of format: ~s]", Args);

format_error({data_invalid, Schema, not_allowed, Value}, Encoder) ->
    Disallowed = json_path("disallow", Schema),
    Args = [get_desc(Schema), Encoder(Value), Encoder(Disallowed)],
    print_tabbed_line("~s: ~s [must not be any of: ~s]", Args);

format_error({data_invalid, Schema, not_divisible, Value}, Encoder) ->
    DivisibleBy = json_path("divisibleBy", Schema),
    Args = [get_desc(Schema), Encoder(Value), Encoder(DivisibleBy)],
    print_tabbed_line("~s: ~s [must be divisible by: ~s]", Args);

format_error( {data_invalid, Schema, {missing_dependency, Dependency}, _Val}
            , Encoder
            ) ->
    Args = [get_desc(Schema), Encoder(Dependency)],
    print_tabbed_line("~s missing dependency: ~s", Args);

format_error({data_invalid, Schema, not_in_range, Value}, Encoder) ->
    case json_path("enum", Schema, undefined) of
        undefined ->
            Min = json_path("minimum", Schema, '-inf'),
            Max = json_path("maximum", Schema, 'inf+'),
            Args = [get_desc(Schema), Encoder(Value), Min, Max],
            print_tabbed_line("~s: ~s [must be from ~p to ~p]", Args);
        Enum ->
            S = string:join(lists:map(Encoder, Enum), ", "),
            Args = [get_desc(Schema), Encoder(Value), S],
            print_tabbed_line("~s: ~s [must be in [~s]]", Args)
    end;

format_error({data_invalid, Schema, wrong_length, Value}, Encoder) ->
    Min = json_path("minLength", Schema, 0),
    Max = json_path("maxLength", Schema, inf),
    Len = length(binary_to_list(Value)),
    Args = [get_desc(Schema), Encoder(Value), Len, Min, Max],
    print_tabbed_line( "~s: ~s has wrong length: ~p [must be from ~p to ~p]",
                       Args );

format_error({data_invalid, Schema, wrong_size, Value}, _Encoder) ->
    Min = json_path("minItems", Schema, 0),
    Max = json_path("maxItems", Schema, inf),
    Len = length(Value),
    Args = [get_desc(Schema), Len, Min, Max],
    print_tabbed_line( "~s has wrong number of items: ~p "
                       "[must be from ~p to ~p]", Args ).

print_tabbed_line(Format, Args) ->
    io_lib:format(Format, Args).

get_desc(Schema) ->
    encode(json_path("description", Schema)).

get_props({struct, Value}) when is_list(Value) ->
    proplists:get_keys(Value);
get_props(Value) when is_list(Value) ->
    proplists:get_keys(Value).

encode(Integer) when is_integer(Integer) -> integer_to_list(Integer);
encode(Binary)  when is_binary(Binary)   -> binary_to_list(Binary);
encode(Atom)    when is_atom(Atom)       -> atom_to_list(Atom);
encode(List)    when is_list(List)       -> List;
encode(_)                                -> "<VALUE>".

json_path(K, Json, Default) ->
    case json_path(K, Json) of
        [] -> Default;
        Value -> Value
    end.

json_path(K, Json) when is_list(K) ->
    json_path(list_to_binary(K), Json);
json_path(K, Json) ->
    jesse_json_path:path(K, Json).