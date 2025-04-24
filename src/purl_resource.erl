-module(purl_resource).

-feature(maybe_expr, enable).

-include("internal/doc.hrl").
-include("purl.hrl").

?MODULEDOC(false).

-export([from_uri/1]).

-spec from_uri(Uri) -> {ok, purl:t()} | error when
    Uri :: uri_string:uri_map() | uri_string:uri_string().
from_uri(#{scheme := <<"https">>, path := Path, host := <<"github.com">>}) ->
    PathWithoutGit = remove_git_suffix(Path),
    PathSegments = split_path(PathWithoutGit),

    case PathSegments of
        [User, Repository] ->
            purl:new(#purl{type = <<"github">>, namespace = [User], name = Repository});
        _Other ->
            error
    end;
from_uri(#{scheme := <<"git+ssh">>, path := Path, userinfo := <<"git">>, host := <<"github.com">>}) ->
    PathWithoutGit = remove_git_suffix(Path),
    PathSegments = split_path(PathWithoutGit),

    case PathSegments of
        [User, Repository] ->
            purl:new(#purl{type = <<"github">>, namespace = [User], name = Repository});
        _Other ->
            error
    end;
from_uri(#{scheme := <<"https">>, path := Path, host := <<"bitbucket.org">>}) ->
    PathWithoutGit = remove_git_suffix(Path),
    PathSegments = split_path(PathWithoutGit),

    case PathSegments of
        [User, Repository] ->
            purl:new(#purl{type = <<"bitbucket">>, namespace = [User], name = Repository});
        _Other ->
            error
    end;
from_uri(#{
    scheme := <<"git+ssh">>, path := Path, userinfo := <<"git">>, host := <<"bitbucket.org">>
}) ->
    PathWithoutGit = remove_git_suffix(Path),
    PathSegments = split_path(PathWithoutGit),

    case PathSegments of
        [User, Repository] ->
            purl:new(#purl{type = <<"bitbucket">>, namespace = [User], name = Repository});
        _Other ->
            error
    end;
from_uri(#{scheme := <<"https">>, path := <<"/packages/"/utf8, Path/binary>>, host := <<"hex.pm">>}) ->
    PathSegments = split_path(Path),

    case PathSegments of
        [Package] ->
            purl:new(#purl{type = <<"hex">>, namespace = [], name = Package});
        _Other ->
            error
    end;
from_uri(#{}) ->
    error;
from_uri(Uri) ->
    case uri_string:parse(Uri) of
        #{} = UriMap ->
            from_uri(UriMap);
        {error, _Type, _Term} ->
            case uri_string:parse(convert_git_to_normal_uri(Uri)) of
                #{} = UriMap -> from_uri(UriMap);
                {error, _GitType, _GitTerm} -> error
            end
    end.

-spec remove_git_suffix(Path) -> Path when Path :: binary().
remove_git_suffix(Path) -> re:replace(Path, "\\.git$", "", [{return, binary}, unicode]).

-spec split_path(Path) -> [binary()] when Path :: binary().
split_path(Path) -> binary:split(Path, <<"/">>, [trim_all, global]).

-spec convert_git_to_normal_uri(Uri) -> binary() when Uri :: binary().
convert_git_to_normal_uri(Uri) ->
    Replaced = re:replace(Uri, ":", "/", [{return, binary}]),
    <<"git+ssh://", Replaced/binary>>.
