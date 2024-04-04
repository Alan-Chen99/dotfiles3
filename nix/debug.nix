{
  self,
  deps,
  lib,
  legacypkgs,
}: rec {
  topretty = lib.generators.toPretty {allowPrettyValues = true;};
  dumb = {
    __pretty = _: topretty "derivation";
    val = null;
  };
  patchdrv = x:
    if lib.attrsets.isDerivation x
    then {
      __pretty = x:
        lib.strings.concatStrings
        [(topretty x) " : " (topretty (x // {type = dumb;}))];
      val = x;
    }
    else x;

  export.pr = x: builtins.trace (topretty (patchdrv x)) null;
  export.trace-pr = x: builtins.trace (topretty (patchdrv x)) x;
  export.dbgn = n: x:
    if lib.attrsets.isDerivation x
    then (lib.debug.traceSeqN (n + 1) (patchdrv x) null)
    else (lib.debug.traceSeqN n x null);
  export.dbg1 = self.dbgn 1;
  export.dbg2 = self.dbgn 2;

  test.dbg = let
    x = self.dbg2 {
      a = "abc";
      type = "derivation";
    };
  in
    x == null;

  export.loc = x:
    if lib.attrsets.isDerivation x
    then x.meta.position
    else if builtins.isFunction x
    then builtins.unsafeGetLambdaPos x
    else null;

  export.prpkgs = {}: self.pr (builtins.attrNames legacypkgs);
}
