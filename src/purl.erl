-module(purl).

-feature(maybe_expr, enable).

-include("internal/doc.hrl").
-include("purl.hrl").

?MODULEDOC("""
Erlang Implementation of the purl (package url) specification.

## Specification

https://github.com/package-url/purl-spec

**Format**: `pkg:type/namespace/name@version?qualifiers#subpath`

> #### License {: .neutral}
>
> A lot of the documentation was taken directly from the specification. It is
> licensed under the MIT License:
> ```
> Copyright (c) the purl authors
>
> Permission is hereby granted, free of charge, to any person obtaining a copy of
> this software and associated documentation files (the "Software"), to deal in
> the Software without restriction, including without limitation the rights to
> use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
> the Software, and to permit persons to whom the Software is furnished to do so,
> subject to the following conditions:
>
> The above copyright notice and this permission notice shall be included in all
> copies or substantial portions of the Software.
>
> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
> IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
> FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
> COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
> IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
> CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
> ```
""").
?MODULEDOC(#{since => <<"0.3.0">>}).

-export_type([
    parse_error/0,
    type/0,
    namespace_segment/0,
    namespace/0,
    name/0,
    version/0,
    qualifier_key/0,
    qualifier_value/0,
    qualifiers/0,
    subpath_segment/0,
    subpath/0,
    t/0
]).

-export([to_binary/1, to_uri/1, new/1, from_resource_uri/1, from_resource_uri/2]).

?DOC(#{since => <<"0.3.0">>}).
-type parse_error() ::
    {error,
        {invalid_field, Field :: atom(), Value :: binary()}
        | {duplicate_qualifier, Key :: binary()}
        | {invalid_scheme, Scheme :: binary() | undefined}
        | {special_case_failed, Message :: binary()}}
    | uri_string:error().

?DOC("""
the package "type" or package "protocol" such as `maven`, `npm`, `nuget`,
`gem`, `pypi`, etc.

Known types: https://github.com/package-url/purl-spec/blob/master/PURL-TYPES.rst

## Validation

* The package type is composed only of ASCII letters and numbers, '.', '+' and '-' (period, plus, and dash)
* The type cannot start with a number
* The type cannot contains spaces
* The type must NOT be percent-encoded
* The type is case insensitive. The canonical form is lowercase
""").
?DOC(#{since => <<"0.3.0">>}).
-type type() :: binary().

?DOC("""
Segment of the namespace

## Validation

* must not contain a '/'
* must not be empty
* A URL host or Authority must NOT be used as a namespace. Use instead a
`repository_url` qualifier. Note however that for some types, the namespace
may look like a host.
""").
?DOC(#{since => <<"0.3.0">>}).
-type namespace_segment() :: binary().

?DOC("""
some name prefix such as a Maven groupid, a Docker image owner, a GitHub user
or organization

The values are type-specific.
""").
?DOC(#{since => <<"0.3.0">>}).
-type namespace() :: [namespace_segment()].

?DOC("""
the name of the package
""").
?DOC(#{since => <<"0.3.0">>}).
-type name() :: binary().

?DOC("""
the version of the package

A version is a plain and opaque string. Some package types use versioning
conventions such as semver for NPMs or nevra conventions for RPMS. A type may
define a procedure to compare and sort versions, but there is no reliable and
uniform way to do such comparison consistently.
""").
?DOC(#{since => <<"0.3.0">>}).
-type version() :: binary().

?DOC("""
qualifier key

## Validation

* The key must be composed only of ASCII letters and numbers, '.', '-' and '_' (period, dash and underscore)
* A key cannot start with a number
* A key must NOT be percent-encoded
* A key is case insensitive. The canonical form is lowercase
* A key cannot contains spaces
""").
?DOC(#{since => <<"0.3.0">>}).
-type qualifier_key() :: binary().

?DOC("""
qualifier value

## Validation
* value cannot be an empty string: a key=value pair with an empty value is the
same as no key/value at all for this key
""").
?DOC(#{since => <<"0.3.0">>}).
-type qualifier_value() :: binary().

?DOC("""
extra qualifying data for a package such as an OS, architecture, a distro,
etc.

The values are type-specific.

## Validation
* key must be unique within the keys of the qualifiers string
""").
?DOC(#{since => <<"0.3.0">>}).
-type qualifiers() :: #{qualifier_key() => qualifier_value()}.

?DOC("""
subpath segment

## Validation
* must not contain a '/'
* must not be any of '..' or '.'
* must not be empty
""").
?DOC(#{since => <<"0.3.0">>}).
-type subpath_segment() :: binary().

?DOC("""
extra subpath within a package, relative to the package root
""").
?DOC(#{since => <<"0.3.0">>}).
-type subpath() :: [subpath_segment()].

?DOC("""
Package URL record
""").
?DOC(#{since => <<"0.3.0">>}).
-type t() :: #purl{
    type :: purl:type(),
    namespace :: purl:namespace(),
    name :: purl:name(),
    version :: purl:version() | undefined,
    qualifiers :: purl:qualifiers(),
    subpath :: purl:subpath()
}.

?DOC("""
Formats purl as binary

## Examples

```
> purl:to_binary(#purl{type = "hex", name = "purl", namespace = [], subpath = [], qualifiers = #{}})
<<"pkg:hex/purl">>
```

""").
?DOC(#{since => <<"0.3.0">>}).
-spec to_binary(Purl) -> unicode:chardata() when Purl :: t().
to_binary(#purl{} = Purl) -> uri_string:recompose(to_uri(Purl)).

?DOC("""
Converts a purl to a `uri_string:uri_map()`

## Examples

```
> purl:to_uri(#purl{type = "hex", name = "purl", namespace = [], subpath = [], qualifiers = #{}})
#{scheme=><<"pkg">>,path=><<"hex/purl">>}
```

""").
?DOC(#{since => <<"0.3.0">>}).
-spec to_uri(Purl) -> uri_string:uri_map() when Purl :: t().
to_uri(#purl{} = Purl) -> purl_composer:compose_uri(Purl).

?DOC("""
Creates a new purl struct from a `Purl`, `URI` or string.

## Examples

```
> purl:new(<<"pkg:hex/purl">>)
{ok, #purl{type = <<"hex">>, name = <<"purl">>, namespace = [], subpath = [], qualifiers = #{}}}
```

""").
?DOC(#{since => <<"0.3.0">>}).
-spec new(Purl) -> {ok, t()} | parse_error() when
    Purl :: uri_string:uri_string() | uri_string:uri_map() | t().
new(Purl) ->
    maybe
        {ok, Parsed} ?= purl_parser:parse(Purl),
        purl_special_case:apply(Parsed)
    end.

?DOC("""
Convert known URLs to purl

## Currently Supported

* GitHub: Repository HTTP / Git URL, Project URL
* BitBucket: Repository HTTTP / Git URL, Project URL
* Hex.pm package URL

""").
?DOC(#{since => <<"0.3.0">>}).
-spec from_resource_uri(Uri, FallbackVersion) -> {ok, t()} | error when
    Uri :: uri_string:uri_map() | uri_string:uri_string(),
    FallbackVersion :: undefined | binary().
from_resource_uri(Uri, FallbackVersion) -> purl_resource:from_uri(Uri, FallbackVersion).

?DOC("""
Convert known URLs to purl

See `from_resource_uri/2`.
""").
?DOC(#{since => <<"0.3.0">>}).
-spec from_resource_uri(Uri) -> {ok, t()} | error when
    Uri :: uri_string:uri_map() | uri_string:uri_string().
from_resource_uri(Uri) -> from_resource_uri(Uri, undefined).
