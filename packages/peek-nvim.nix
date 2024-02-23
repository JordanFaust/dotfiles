{ inputs
, stdenv
, stdenvNoCC
, deno
, deno2nix
# , python3
# , python3Packages
, fetchurl
, fetchFromGitHub
, glib-networking
, gtk3
, gobject-introspection
, jq
, makeWrapper
, webkitgtk
# , wrapGAppsHook
}:
let
  gfmCss = fetchurl {
    url = "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/5.1.0/github-markdown.min.css";
    hash = "sha256-ZNZ1RlGwgBHlYCmsbfg7tHxaVw2raeJOKJ9x/Zfrkn0=";
  };
  pname   = "peek-nvim";
  version = "1.0.4p3";
  src = fetchFromGitHub {
    owner = "toppair";
    repo = "peek.nvim";
    rev = "c6ab252ff874e1597e27f865dcfc68b15cae0cff";
    sha256 = "sha256-ovIe6alcpYKDC4Eje3kDQpVB9Q88Svb6Tx88iU2vBqQ=";
  };

  # Here we build the vendor directory as a separate derivation.
  # peekVendor = stdenv.mkDerivation rec {
in stdenv.mkDerivation rec {
  # name = "peek-vendor";
  # name = "peek-nvim";
  inherit version pname;

  src = ./peek-nvim;

  # dontConfigure = true;

  nativeBuildInputs = [ deno ];

  # configurePhase = ''
  #   runHook preConfigurePhase
  #
  #   cat $src/client/src/mermaid.ts
  #   sed -i 's/https:\/\/cdn.skypack.dev\/@types\/mermaid?dts/https:\/\/cdn.esm.sh\/mermaid@9?no-dts/g' $src/client/src/mermaid.ts
  #   cat $src/client/src/mermaid.ts
  #   exit 1
  #
  #   runHook postConfigurePhase
  # '';

  # buildCommand = ''
  #   runHook preBuild
  #
  #   # Deno wants to create cache directories.
  #   # By default $HOME points to /homeless-shelter, which isn't writable.
  #
  #   mkdir $out
  #
  #   HOME="$(mktemp -d)"
  #
  #   # # cp -r $src
  #   # cp $src/client/src/mermaid.ts $HOME/mermaid.ts
  #   # ls -al
  #   # sed -i 's/https:\/\/cdn.skypack.dev\/@types\/mermaid?dts/https:\/\/cdn.esm.sh\/mermaid@9?no-dts/g' client/src/mermaid.ts
  #   #
  #   # cp -r $src/app $out/app
  #   # mkdir -p $out/client/src
  #   # cp -r $src/client/src/script.ts $out/client/script.ts
  #   # cp $HOME/mermaid.ts $out/client/mermaid.ts
  #   # cp -r $src/client/src/util.ts $out/client/util.ts
  #   # cp -r $src/media $out/media
  #   # cp -r $src/public $out/public
  #   # cp -r $src/scripts $out/scripts
  #   #
  #   # cp -r $HOME/mermaid.ts $out/client/src/mermaid.ts
  #
  #   # Build vendor directory
  #   deno vendor --output=$out/vendor \
  #     $src/scripts/build.js \
  #     $src/app/src/main.ts \
  #     $src/app/src/webview.ts \
  #     $src/client/src/script.ts \
  #     $src/scripts/build.js
  #
  #   runHook postBuild
  # '';

  buildCommand = ''
    # runHook preInstall

    mkdir $out

    HOME="$(mktemp -d)"

    ls -al

    cp -r $src/app app
    cp -r $src/client client
    cp -r $src/lua lua
    cp -r $src/media media
    cp -r $src/public public
    cp -r $src/scripts scripts
    cp $src/deno.json deno.json

    # cp -r ${gfmCss} public/github-markdown.min.css

    ls -al

    echo "vendoring dependencies"
    deno vendor  \
      scripts/build.js \
      app/src/main.ts \
      app/src/webview.ts \
      client/src/script.ts
      scripts/build.js

    ls -al
    ls -al vendor/

    echo "building clients"
    deno run \
      --import-map=vendor/import_map.json \
      --allow-read \
      --allow-write \
      --allow-env \
      --no-remote \
      --no-check \
      scripts/build.js

    echo "copying to out"
    cp -r app $out/app
    cp -r client $out/client
    cp -r lua $out/lua
    cp -r media $out/media
    cp -r public $out/public
    cp -r scripts $out/scripts
    cp -r vendor $out/vendor
    cp deno.json $out/deno.json

    ls -al $out/public

    # runHook postInstall
  '';


  propagatedBuildInputs = [
    glib-networking
    gtk3
    gobject-introspection
    webkitgtk
    # pythonPackages.pygobject3
    # pythonPackages.requests
  ];

  # Here we specify the hash, which makes this a fixed-output derivation.
  # When inputs have changed, outputHash should be set to empty, to recalculate the new hash.
  outputHashAlgo = "sha256";
  outputHashMode = "recursive";
  outputHash = "sha256-4Yqc4RJuANA3JMAdQ0MZuIt9q2uGYrFv9v8vIk4ruxE=";

  # meta = {
  #   homepage = "https://github.com/toppair/peek.nvim";
  #   description = "Markdown preview plugin for Neovim";
  #   # license = lib.licenses.unlicense;
  #   platforms = [ "x86_64-linux" ];
  #   maintainers = [];
  # };
}
# in stdenvNoCC.mkDerivation rec {
#   inherit pname version src;
#
#   buildInputs = [ deno jq ];
#
#   # buildPhase = ''
#   #   # export DENO_DIR="/tmp/deno2nix"
#   #   # mkdir -p $DENO_DIR
#   #   # ln -s "{deno2nix.internal.mkDepsLink (src + "/deno.lock")}" $(deno info --json | jq -r .modulesCache)
#   #   # cp -r ${gfmCss} public/github-markdown.min.css
#   #   # # ${deno}/bin/deno task build:fast
#   #   # # ls -al public
#   #   # # exit 1
#   #   # ${deno}/bin/deno run --allow-read --allow-write --allow-env scripts/build-main.out.js
#   #   # ls -al public
#   #   # exit 1
#   #   # # # ${deno}/bin/deno run --allow-read scripts/build-script.out.js
#   #   # # # ${deno}/bin/deno run --allow-read scripts/build-webview.out.js
#   #
#   #
#   #   cp -r ${gfmCss} public/github-markdown.min.css
#   #
#   #   ls -al ${peekVendor}/import_map.json
#   #
#   #   HOME="$(mktemp -d)"
#   #
#   #   cp -r ${peekVendor} $HOME/
#   #
#   #   ${deno}/bin/deno run \
#   #     --import-map=${peekVendor}/import_map.json \
#   #     --allow-read \
#   #     --allow-write \
#   #     --allow-env \
#   #     --no-remote \
#   #     --no-check \
#   #     ${src}/scripts/build.js
#   #   ls -al public/
#   #   exit 1
#   # '';
#   #
#   # installPhase = ''
#   #   runHook preInstall
#   #
#   #   mkdir $out
#   #
#   #   cp -r app $out/app
#   #   cp -r client $out/client
#   #   cp -r lua $out/lua
#   #   cp -r media $out/media
#   #   cp -r public $out/public
#   #   cp deno.json $out/deno.json
#   #
#   #   ls -al $out/public
#   #
#   #   runHook postInstall
#   # '';
#
#   installPhase = ''
#     runHook preInstall
#
#     mkdir $out
#
#     cp -r ${peekVendor}/ $out/
#
#     exit 1
#
#     runHook postInstall
#   '';
#
#   postInstall = ''
#     # runHook preInstall
#
#     # ls -al $out
#
#     # ls -al $out/public
#     # # cp -r ${gfmCss} public/github-markdown.min.css
#
#     # runHook postInstall
#   '';
#   #
#   # wrapGAppsHook is required to make sure GTK is properly loaded and
#   # detected
#   # nativeBuildInputs = [
#   #   # wrapGAppsHook
#   #   glib-networking
#   #   gobject-introspection
#   #   gtk3
#   #   deno
#   # ];
#   propagatedBuildInputs = [
#     glib-networking
#     gtk3
#     gobject-introspection
#     webkitgtk
#     # pythonPackages.pygobject3
#     # pythonPackages.requests
#   ];
#
#   meta = {
#     homepage = "https://github.com/toppair/peek.nvim";
#     description = "Markdown preview plugin for Neovim";
#     # license = lib.licenses.unlicense;
#     platforms = [ "x86_64-linux" ];
#     maintainers = [];
#   };
# }
