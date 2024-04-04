args: let
  f = import ./package.nix;
  package-bootstrap = (f (args // {self = package-bootstrap;})).export;
  package = package-bootstrap.import-package f args;
in
  package
