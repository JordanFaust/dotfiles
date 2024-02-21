{ inputs
, stdenv
, deno
# , python3
# , python3Packages
, fetchurl
, fetchFromGitHub
, glib-networking
, gtk3
, gobject-introspection
, makeWrapper
, webkitgtk
# , wrapGAppsHook
}:
let
  gfmCss = fetchurl "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/5.1.0/github-markdown.min.css";
in
stdenv.mkDerivation rec {
  pname   = "peek.nvim";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "toppair";
    repo = "peek.nvim";
    rev = "5c5f1ee2305e39102ab1eb65204424dece2d4208";
    sha256 = "g10S8C32mnOymCmGNdM8gmGpYn5/ObMJK3g6amKtQmI=";
  };

  phases = "unpackPhase pathPhase installPhase";

  buildInputs = [ deno ];
  pathPhase = ''
  '';
  prePatch = ''
    # mkdir -p ./subprojects/gvc
  '';

  postPatch = ''
    # chmod +x post_install.sh
    # patchShebangs post_install.sh
  '';
  buildPhase = ''
    echo "nothing"
  '';
  # phases = "installPhase";
  #
  # buildPhase = ''
  #   # ${deno}/bin/deno task \
  #   #   --quite build:fast
  # '';
  #
  # postPatch = ''
  #   mkdir $out
  #   cp -r app/src $out
  #   cp -r client/src $out
  #   cp -r lua/peek $out
  #   cp -r media $out
  #   cp -r public $out
  #   cp -r scripts $out
  #   cp -r deno.json $out
  #   cp ${gfmCss} public/github-markdown.min.css
  # '';
  #
  # wrapGAppsHook is required to make sure GTK is properly loaded and
  # detected
  # nativeBuildInputs = [
  #   # wrapGAppsHook
  #   glib-networking
  #   gobject-introspection
  #   gtk3
  #   deno
  # ];
  propagatedBuildInputs = [
    glib-networking
    gtk3
    gobject-introspection
    webkitgtk
    # pythonPackages.pygobject3
    # pythonPackages.requests
  ];

  # buildPhase = ''
  #   cp ${gfmCss} public/github-markdown.min.css
  # '';
  #
  # installPhase = ''
  #   cp ${gfmCss} public/github-markdown.min.css
  # '';

  meta = {
    homepage = "https://github.com/toppair/peek.nvim";
    description = "Markdown preview plugin for Neovim";
    # license = lib.licenses.unlicense;
    platforms = [ "x86_64-linux" ];
    maintainers = [];
  };
}
