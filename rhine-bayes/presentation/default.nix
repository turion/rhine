{ pkgs, nix-mkPandoc }:

let
  inherit (pkgs) lib;
  mkPandoc = import nix-mkPandoc {
    inherit pkgs;
  };
  title = "rhine-bayes";
  opts = {
    src = ./.;
    documentFile = ./. + "/${title}.md";
    version = "0.1.0";
    incremental = true;
  };
  myMkPandoc = specificOpts: mkPandoc (specificOpts // opts);
  beamer = myMkPandoc {
    name = "${title}.pdf";
    texlivePackages = {
      inherit (pkgs.texlive)
        fancyvrb
        beamer
        xcolor;
    };
    to = "beamer";
    variables = {
      theme = "Frankfurt";
      colortheme = "beaver";
    };
  };
  revealjs = myMkPandoc {
    name = "${title}.html";
    to = "revealjs";
    variables = {
      theme = "serif";
      revealjs-url = builtins.fetchTarball {
        url    = "https://registry.npmjs.org/reveal.js/-/reveal.js-4.1.3.tgz";
        sha256 = "0a93vxd49y2g0wsafghgqcpj6gszzjvv9lql5zrwrw26lc02x465";
      };
      standalone  = true;
      embed-resources = true;
    };
  };
in pkgs.linkFarmFromDrvs title [ beamer revealjs ]
