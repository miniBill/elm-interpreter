{ pkgs, ... }:

{
  languages.javascript.enable = true;
  languages.javascript.corepack.enable = true;

  processes = {
    serve.exec = "yarn elm-watch hot";
  };
}
