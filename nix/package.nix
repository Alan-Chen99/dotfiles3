{
  self,
  dbg ? null,
}: rec {
  nixd-eval = true;

  isFunction = f:
    builtins.isFunction f
    || (f ? __functor && isFunction (f.__functor f));

  functionArgs = f:
    if f ? __functor
    then f.__functionArgs or (functionArgs (f.__functor f))
    else builtins.functionArgs f;

  fix = f: let x = f x; in x;

  make-package-helper = pkg:
    pkg.val
    // {
      __package = pkg;
      __functor = add-extension pkg;
    };

  add-extension = old: _: extension:
    if isFunction extension
    then let
      newf = final: let
        prev = old.__unfix__ final;
      in
        prev // (extension final prev);
    in
      make-package-helper {
        __unfix__ = newf;
        val = fix newf;
        extensions = old.extensions ++ [extension];
      }
    else let
      ext = extension.__extend extension old;
    in
      make-package-helper rec {
        __unfix__ = ext.__unfix__;
        val = fix __unfix__;
        extensions = old.extensions ++ [ext.extension];
      };

  extend-reexport = parent-self: get-export-set: _: old: let
    newf = final: let
      export-set = get-export-set prev;
      ipts = builtins.mapAttrs (name: _: parent-self."${name}") export-set;
      exports-names = builtins.attrNames export-set;
      prev = old.__unfix__ (final // ipts);
      ans-others = builtins.removeAttrs prev exports-names;
    in
      ans-others // {reexport = export-set;};
  in {
    __unfix__ = newf;
    extension.exports-names = builtins.attrNames (builtins.intersectAttrs old.val parent-self);
  };

  export.reexport-with = parent-self: export-set: {
    __extend = extend-reexport parent-self export-set;
  };

  make-package = fn: args: let
    fixpt = final: let
      fn_args = args // {self = builtins.intersectAttrs prev final;};
      fn_out = fn fn_args;
      prev = fn_out.export;
    in
      {
        __unwrapped = fn;
        __args = fn_args;
        __out = fn_out;
        __clean = builtins.removeAttrs final ["__unwrapped" "__args" "__out" "__clean"];
      }
      // prev;
  in
    make-package-helper {
      __unfix__ = fixpt;
      val = fix fixpt;
      extensions = [];
    };

  export.import-package = fn: let
    f =
      if builtins.isFunction fn
      then fn
      else import fn;
  in {
    __functionArgs = builtins.removeAttrs (functionArgs f) ["self"];
    __functor = _: args: make-package f args;
  };

  export.call-package-with = defaultargs: fn: let
    pkg = self.import-package fn;
    auto = builtins.intersectAttrs (functionArgs pkg) defaultargs;
  in
    args: pkg (auto // args);

  test = rec {
    testpkg = {
      a,
      b,
      self,
    }: {
      export.x = a;
      export.y = self.x + 1;
    };

    testcalled =
      self.call-package-with {
        a = 3;
      }
      testpkg {
        b = 2;
      };
    y1 = testcalled.y;
    y2 = (testcalled (final: prev: {x = 10;})).y;
  };

  export.runtest = {}: dbg.pr test;
}
