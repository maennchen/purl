-ifndef(PURL_HRL).

-record(purl, {
    type :: purl:type(),
    namespace = [] :: purl:namespace(),
    name :: purl:name(),
    version = undefined :: purl:version() | undefined,
    qualifiers = #{} :: purl:qualifiers(),
    subpath = [] :: purl:subpath()
}).

-define(PURL_HRL, 1).

-endif.
