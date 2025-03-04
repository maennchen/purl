defmodule PurlTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

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

  describe inspect(&Purl.from_resource_uri/1) do
    test "GitHub git url" do
      assert {:ok, %Purl{type: "github", namespace: ["jshmrtn"], name: "purl"}} =
               Purl.from_resource_uri("git@github.com:jshmrtn/purl.git")
    end

    test "GitHub http url" do
      assert {:ok, %Purl{type: "github", namespace: ["jshmrtn"], name: "purl"}} =
               Purl.from_resource_uri("https://github.com/jshmrtn/purl.git")
    end

    test "BitBucket git url" do
      assert {:ok, %Purl{type: "bitbucket", namespace: ["jshmrtn"], name: "purl"}} =
               Purl.from_resource_uri("git@bitbucket.org:jshmrtn/purl.git")
    end

    test "BitBucket http url" do
      assert {:ok, %Purl{type: "bitbucket", namespace: ["jshmrtn"], name: "purl"}} =
               Purl.from_resource_uri("https://irrelevant@bitbucket.org/jshmrtn/purl.git")
    end

    test "Hex.pm http url" do
      assert {:ok, %Purl{type: "hex", namespace: [], name: "expo"}} =
               Purl.from_resource_uri("https://hex.pm/packages/expo")
    end

    test "Random URL" do
      assert :error = Purl.from_resource_uri("https://example.com")
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

          subpath =
            (subpath || "")
            |> String.split("/", trim: true)
            |> Enum.reject(&(&1 in ["", ".", ".."]))

          assert {:ok, parsed} = Purl.new(unquote(purl))

          assert %Purl{
                   type: type,
                   namespace: namespace,
                   name: name,
                   version: version,
                   qualifiers: qualifiers,
                   subpath: subpath
                 } == parsed

          assert canonical == Purl.to_string(parsed)
        end
      end
    end
  end

  describe "fuzzer" do
    property "compose / parse gives same result" do
      check all(purl <- Purl.Generator.purl()) do
        # Uppercase fields should still parse, but the canonical form is lowercase
        comparison = %{
          purl
          | type: String.downcase(purl.type),
            qualifiers: Map.new(purl.qualifiers, fn {key, value} -> {String.downcase(key), value} end),
            version:
              case purl.version do
                %Version{} = version -> Version.to_string(version)
                string when is_binary(string) -> string
              end
        }

        canonical = Purl.to_string(purl)

        assert {:ok, comparison} == Purl.new(canonical)
      end
    end
  end
end
