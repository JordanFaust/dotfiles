{ lib
, python3
, python3Packages
, fetchFromGitHub
, glib-networking
, gtk3
, gobject-introspection
, makeWrapper
, webkitgtk
, wrapGAppsHook
}:

let
  pythonPackages = python3Packages;
in python3Packages.buildPythonApplication rec {
  pname   = "gp-saml-gui";
  version = "1.0.8";

  src = fetchFromGitHub {
    owner = "dlenski";
    repo = "gp-saml-gui";
    rev = "e73026c85b42cbfed0a7a074e4f4b5c4d471aeb8";
    sha256 = "jrbDU0FJz6r33MP3vZFS6iFrf9xIe7jLwJkEUkNy374=";
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
    platforms = [ "x86_64-linux" ];
    maintainers = [];
  };
}