{ pkgs, ... }:

{
  packages = [ pkgs.git ];

  languages.elm.enable = true;
  languages.javascript.enable = true;
  languages.javascript.corepack.enable = true;

  pre-commit.hooks = {
    elm-format.enable = true;
    elm-review.enable = true;
    elm-test.enable = true;
  };

  processes = {
    serve.exec = "yarn elm-watch hot";
  };
}
