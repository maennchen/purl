defmodule PurlTest do
  use ExUnit.Case, async: true
  doctest Purl

  test "greets the world" do
    assert Purl.hello() == :world
  end
end
