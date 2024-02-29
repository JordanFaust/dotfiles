{
  lib,
  inputs,
  python3,
  python3Packages,
  fetchFromGitHub,
  glib-networking,
  gtk3,
  gobject-introspection,
  makeWrapper,
  webkitgtk,
  wrapGAppsHook,
}: let
  pythonPackages = python3Packages;
in
  python3Packages.buildPythonApplication rec {
    pname = "gp-saml-gui";
    version = "1.0.10";

    src = fetchFromGitHub {
      owner = "dlenski";
      repo = "gp-saml-gui";
      # rev = "f1fafba32e06a86308aab03abaaa1b076e1e1096";
      rev = "258f47cdc4a8ed57a1eef16667f6cad0d1cb49b1";
      sha256 = "g10S8C32mnOymCmGNdM8gmGpYn5/ObMJK3g6amKtQmI=";
    };

    # wrapGAppsHook is required to make sure GTK is properly loaded and
    # detected
    nativeBuildInputs = [
      wrapGAppsHook
      glib-networking
      gobject-introspection
      gtk3
    ];
    propagatedBuildInputs = [
      glib-networking
      gtk3
      gobject-introspection
      webkitgtk
      pythonPackages.pygobject3
      pythonPackages.requests
    ];

    # This is required because of an issue with webkitgtk:
    # https://github.com/NixOS/nixpkgs/issues/16327
    postInstall = ''
      wrapProgram "$out/bin/gp-saml-gui" --set WEBKIT_DISABLE_COMPOSITING_MODE 1
    '';

    meta = {
      homepage = "https://github.com/dlenski/gp-saml-gui";
      description = "GUI for GlobalProtect VPN with SAML for OpenConnect";
      # license = lib.licenses.unlicense;
      platforms = ["x86_64-linux"];
      maintainers = [];
    };
  }
