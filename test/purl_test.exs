defmodule PurlTest do
  use ExUnit.Case, async: true

  doctest Purl

  spec_tests =
    "test/spec/test-suite-data.json"
    |> File.read!()
    |> Jason.decode!()

  describe inspect(&Purl.new/1) do
    test "should return a purl for a string" do
      assert {:ok,
              %Purl{
                type: "hex",
                namespace: ["name", "space"],
                name: "purl",
                version: "1.0.0",
                qualifiers: %{"key" => "value"},
                subpath: ["path", "to", "directory"]
              }} = Purl.new("pkg:hex/name/space/purl@1.0.0?key=value#/path/to/directory/")
    end
  end

  describe inspect(&Purl.new!/1) do
    test "should return a purl for a string" do
      assert %Purl{type: "hex", name: "purl"} = Purl.new!("pkg:hex/purl")
    end
  end

  describe "specification verification" do
    for %{"description" => description, "is_invalid" => is_invalid?, "purl" => purl} =
          verification <- spec_tests do
      if is_invalid? do
        test description do
          assert {:error, _reason} = Purl.new(unquote(purl))
        end
      else
        test description do
          %{
            "type" => type,
            "namespace" => namespace,
            "name" => name,
            "version" => version,
            "qualifiers" => qualifiers,
            "subpath" => subpath,
            "canonical_purl" => canonical
          } = unquote(Macro.escape(verification))

          namespace = String.split(namespace || "", "/", trim: true)

          qualifiers =
            case qualifiers do
              nil -> %{}
              qualifiers -> qualifiers
            end

          subpath = String.split(subpath || "", "/", trim: true)

          assert {:ok,
                  %Purl{
                    type: ^type,
                    namespace: ^namespace,
                    name: ^name,
                    version: ^version,
                    qualifiers: ^qualifiers,
                    subpath: ^subpath
                  } = parsed} = Purl.new(unquote(purl))

          assert canonical == Purl.to_string(parsed)
        end
      end
    end
  end
end
