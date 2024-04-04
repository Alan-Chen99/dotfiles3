{
  self,
  lib,
}: {
  export.nixd-eval = lib.generators.toPretty {} {
    x = {x = "hello";};
    __pretty_as = v: v.x;
  };
  # adds __pretty_as
  export.lib-overlay = lib: prev:
    with prev.generators; let
      libStr = lib.strings;
      libAttr = lib.attrsets;
      isFunction = lib.isFunction;

      toPretty = {
        /*
        If this option is true, attrsets like { __pretty = fn; val = …; }
        will use fn to convert val to a pretty printed representation.
        (This means fn is type Val -> String.)
        */
        allowPrettyValues ? false,
        /*
        If this option is true, the output is indented with newlines for attribute sets and lists
        */
        multiline ? true,
        /*
        Initial indentation level
        */
        indent ? "",
      }: let
        go = indent: v:
          with builtins; let
            isPath = v: typeOf v == "path";
            introSpace =
              if multiline
              then "\n${indent}  "
              else " ";
            outroSpace =
              if multiline
              then "\n${indent}"
              else " ";
          in
            if isAttrs v && v ? __pretty_as
            then go indent (v.__pretty_as v)
            else if isInt v
            then toString v
            # toString loses precision on floats, so we use toJSON instead. This isn't perfect
            # as the resulting string may not parse back as a float (e.g. 42, 1e-06), but for
            # pretty-printing purposes this is acceptable.
            else if isFloat v
            then builtins.toJSON v
            else if isString v
            then let
              lines = filter (v: ! isList v) (builtins.split "\n" v);
              escapeSingleline = libStr.escape ["\\" "\"" "\${"];
              escapeMultiline = libStr.replaceStrings ["\${" "''"] ["''\${" "'''"];
              singlelineResult = "\"" + concatStringsSep "\\n" (map escapeSingleline lines) + "\"";
              multilineResult = let
                escapedLines = map escapeMultiline lines;
                # The last line gets a special treatment: if it's empty, '' is on its own line at the "outer"
                # indentation level. Otherwise, '' is appended to the last line.
                lastLine = lib.last escapedLines;
              in
                "''"
                + introSpace
                + concatStringsSep introSpace (lib.init escapedLines)
                + (
                  if lastLine == ""
                  then outroSpace
                  else introSpace + lastLine
                )
                + "''";
            in
              if multiline && length lines > 1
              then multilineResult
              else singlelineResult
            else if true == v
            then "true"
            else if false == v
            then "false"
            else if null == v
            then "null"
            else if isPath v
            then toString v
            else if isList v
            then
              if v == []
              then "[ ]"
              else
                "["
                + introSpace
                + libStr.concatMapStringsSep introSpace (go (indent + "  ")) v
                + outroSpace
                + "]"
            else if isFunction v
            then let
              fna = lib.functionArgs v;
              showFnas = concatStringsSep ", " (libAttr.mapAttrsToList
                (name: hasDefVal:
                  if hasDefVal
                  then name + "?"
                  else name)
                fna);
            in
              if fna == {}
              then "<function>"
              else "<function, args: {${showFnas}}>"
            else if isAttrs v
            then
              # apply pretty values if allowed
              if allowPrettyValues && v ? __pretty && v ? val
              then v.__pretty v.val
              else if v == {}
              then "{ }"
              else if v ? type && v.type == "derivation"
              then "<derivation ${v.name or "???"}>"
              else
                "{"
                + introSpace
                + libStr.concatStringsSep introSpace (libAttr.mapAttrsToList
                  (name: value: "${libStr.escapeNixIdentifier name} = ${
                    builtins.addErrorContext "while evaluating an attribute `${name}`"
                    (go (indent + "  ") value)
                  };")
                  v)
                + outroSpace
                + "}"
            else abort "generators.toPretty: should never happen (v = ${v})";
      in
        go indent;
    in {
      generators =
        prev.generators // {toPretty = toPretty;};
    };
}
