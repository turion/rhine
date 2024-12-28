{ pkgs, overlay, lib }:
let
  dommyPkg = (pkgs.pkgsCross.ghcjs.extend overlay).haskell.packages.ghc910.rhine-tree;
  dommy = lib.traceVal (dommyPkg + "/bin/dommy");
  dommyContents = lib.traceVal (builtins.readFile dommy);
  botchedHTML = pkgs.writeTextFile {
    name = "index.html";
    text = ''
      <!DOCTYPE html>
      <html>
        <head>
          <script>
            ${dommyContents}
          </script>
        </head>
      </html>
    '';
  };
in
dommyPkg
