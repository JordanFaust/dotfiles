{ stdenv, bundlerApp, ruby_3_2, libyaml, my, ... }:
bundlerApp {
  pname = "ruby-lsp";
  gemdir  = ./ruby-lsp;
  ruby = ruby_3_2;
  exes = [ "ruby-lsp" ];
}
