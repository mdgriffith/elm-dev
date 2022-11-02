# This defines a function taking `pkgs` as parameter, and uses
# `nixpkgs` by default if no argument is passed to it.
{ pkgs ? import <nixpkgs> { } }:

# This avoids typing `pkgs.` before each package name.
with pkgs;

# Defines a shell.
mkShell {
  # Sets the build inputs, i.e. what will be available in our
  # local environment.
  buildInputs = [
    elmPackages.elm
    elmPackages.elm-format
    elmPackages.elm-test-rs
    elmPackages.elm-review
    nodejs-18_x
    git
    llvmPackages_13.llvm
  ];
}
