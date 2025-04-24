-module(purl_special_case).

-feature(maybe_expr, enable).

-include("internal/doc.hrl").
-include("purl.hrl").

?MODULEDOC(false).

-export([apply/1]).

-define(RULES, #{
    <<"bitbucket">> => #{
        name_case_sensitive => false,
        name_normalize => hyphen_case,
        namespace_case_sensitive => false
    },
    <<"cran">> => #{version_required => true},
    <<"composer">> => #{
        name_case_sensitive => false,
        namespace_case_sensitive => false
    },
    <<"github">> => #{
        name_case_sensitive => false,
        namespace_case_sensitive => false
    },
    <<"huggingface">> => #{version_case_sensitive => false},
    <<"pypi">> => #{
        name_case_sensitive => false,
        name_normalize => hyphen_case
    },
    <<"swift">> => #{
        version_required => true,
        namespace_min_length => 1
    }
}).

-spec apply(Purl) -> {ok, Purl} | purl:parse_error() when Purl :: purl:t().
apply(Purl) ->
    maybe
        {ok, Purl1} ?= downcase_namespace(Purl),
        {ok, Purl2} ?= enforce_namespace_length(Purl1),
        {ok, Purl3} ?= downcase_name(Purl2),
        {ok, Purl4} ?= normalize_name(Purl3),
        {ok, Purl5} ?= downcase_version(Purl4),
        {ok, Purl6} ?= version_required(Purl5),
        {ok, Purl7} ?= conan_enforce_channel(Purl6),
        cpan_verify_distribution(Purl7)
    end.

-spec downcase_namespace(Purl) -> {ok, Purl} when Purl :: purl:t().
downcase_namespace(#purl{type = Type, namespace = Namespace} = Purl) ->
    case maps:get(Type, ?RULES, #{}) of
        #{namespace_case_sensitive := false} ->
            {ok, Purl#purl{namespace = lists:map(fun string:lowercase/1, Namespace)}};
        _ ->
            {ok, Purl}
    end.

-spec enforce_namespace_length(Purl) -> {ok, Purl} | purl:parse_error() when Purl :: purl:t().
enforce_namespace_length(#purl{type = Type, namespace = Namespace} = Purl) ->
    case maps:get(Type, ?RULES, #{}) of
        #{namespace_min_length := MinLength} ->
            case length(Namespace) >= MinLength of
                true -> {ok, Purl};
                false -> {error, {special_case_failed, <<"namespace missing">>}}
            end;
        _ ->
            {ok, Purl}
    end.

-spec downcase_name(Purl) -> {ok, Purl} when Purl :: purl:t().
downcase_name(#purl{type = Type, name = Name, qualifiers = Qualifiers} = Purl) ->
    case maps:get(Type, ?RULES, #{}) of
        #{name_case_sensitive := false} ->
            {ok, Purl#purl{name = string:lowercase(Name)}};
        _ when Type =:= <<"mlflow">>, is_map_key(<<"repository_url">>, Qualifiers) ->
            case uri_string:parse(maps:get(<<"repository_url">>, Qualifiers)) of
                #{host := Host} ->
                    case
                        binary:part(
                            Host,
                            byte_size(Host) - byte_size(<<"azuredatabricks.net">>),
                            byte_size(<<"azuredatabricks.net">>)
                        ) =:= <<"azuredatabricks.net">>
                    of
                        true -> {ok, Purl#purl{name = string:lowercase(Name)}};
                        false -> {ok, Purl}
                    end;
                _ ->
                    {ok, Purl}
            end;
        _ ->
            {ok, Purl}
    end.

-spec normalize_name(Purl) -> {ok, Purl} when Purl :: purl:t().
normalize_name(#purl{type = Type, name = Name} = Purl) ->
    case maps:get(Type, ?RULES, #{}) of
        #{name_normalize := hyphen_case} ->
            {ok, Purl#purl{name = iolist_to_binary(string:replace(Name, "_", "-", all))}};
        _ ->
            {ok, Purl}
    end.

-spec downcase_version(Purl) -> {ok, Purl} when Purl :: purl:t().
downcase_version(#purl{type = Type, version = Version} = Purl) ->
    case maps:get(Type, ?RULES, #{}) of
        #{version_case_sensitive := false} when is_binary(Version) ->
            {ok, Purl#purl{version = string:lowercase(Version)}};
        _ ->
            {ok, Purl}
    end.

-spec version_required(Purl) -> {ok, Purl} | purl:parse_error() when Purl :: purl:t().
version_required(#purl{type = Type, version = Version} = Purl) ->
    case maps:get(Type, ?RULES, #{}) of
        #{version_required := true} when Version =:= undefined ->
            {error, {special_case_failed, <<"version missing">>}};
        _ ->
            {ok, Purl}
    end.

-spec conan_enforce_channel(Purl) -> {ok, Purl} | purl:parse_error() when Purl :: purl:t().
conan_enforce_channel(
    #purl{type = <<"conan">>, namespace = Namespace, qualifiers = Qualifiers} = Purl
) ->
    case {Namespace, maps:is_key(<<"channel">>, Qualifiers)} of
        {[_ | _], true} ->
            {ok, Purl};
        {[], false} ->
            {ok, Purl};
        _ ->
            {error,
                {special_case_failed,
                    <<"either namespace & channel must both be present or both be absent">>}}
    end;
conan_enforce_channel(Purl) ->
    {ok, Purl}.

-spec cpan_verify_distribution(Purl) -> {ok, Purl} | purl:parse_error() when Purl :: purl:t().
cpan_verify_distribution(#purl{type = <<"cpan">>, namespace = [], name = Name} = Purl) ->
    case binary:match(Name, <<"-">>) of
        {_, _} -> {error, {special_case_failed, <<"cpan modules must not contain \"-\"">>}};
        nomatch -> {ok, Purl}
    end;
cpan_verify_distribution(#purl{type = <<"cpan">>, namespace = Namespace, name = Name} = Purl) ->
    case binary:match(Name, <<"::">>) of
        {_, _} ->
            {error, {special_case_failed, <<"cpan distribution name must not contain \"::\"">>}};
        nomatch ->
            {ok, Purl#purl{namespace = lists:map(fun string:uppercase/1, Namespace)}}
    end;
cpan_verify_distribution(Purl) ->
    {ok, Purl}.
