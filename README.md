# Purl

[![hex.pm badge](https://img.shields.io/badge/Package%20on%20hex.pm-informational)](https://hex.pm/packages/purl)
[![Documentation badge](https://img.shields.io/badge/Documentation-ff69b4)][docs]
[![.github/workflows/branch_main.yml](https://github.com/maennchen/purl/actions/workflows/branch_main.yml/badge.svg)](https://github.com/maennchen/purl/actions/workflows/branch_main.yml)
[![Coverage Status](https://coveralls.io/repos/github/maennchen/purl/badge.svg?branch=main)](https://coveralls.io/github/maennchen/purl?branch=main)

> Implementation of the purl (package url) specification.

See [the documentation][docs].

## Usage

```elixir
iex> Purl.new("pkg:hex/purl")
{:ok, %Purl{type: "hex", name: "purl"}}

iex> Purl.to_string(%Purl{type: "hex", name: "purl"})
"pkg:hex/purl"
```

## Installation

The package can be installed by adding `purl` to your list of dependencies in
`mix.exs`:

```elixir
def deps do
  [
    {:purl, "~> 0.1.0"}
  ]
end
```

## License

Copyright 2023 JOSHMARTIN GmbH

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at:

  > <http://www.apache.org/licenses/LICENSE-2.0>

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.

[docs]: https://hexdocs.pm/purl
