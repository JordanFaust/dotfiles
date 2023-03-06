{ lib
, deno
, pkg-config
, webkitgtk
, stdenv,
...
}:

let name = "webview_deno";
    version = "0.7.4-2022-09-3";
in stdenv.mkDerivation {
  inherit name version;
    # use same webview_deno as in main.ts
    # src = ../../src/webview_deno;
    src = fetchFromGitHub {
      # https://github.com/webview/webview_deno
      owner = "webview";
      repo = pname;
      rev = "9c3bff67c0b6ba2b5dc6066c15b3117da6ad7c68";
      sha256 = "sha256-4iWOoZ5ToH5hMfDP7mDgN52F/RR9WoiLxbyfPRzu+Y0=";
      fetchSubmodules = true;
    };
    # patches = [
    #   # https://github.com/webview/webview_deno/pull/144
    #   # print command before running
    #   ./pull-144.diff
    # ];

    lockfileHash = "sha256-6cwa8WXp3X5zEiys79TbVr506HRbhDwsirRcbSTa4WA=";

    buildInputs = [ deno pkg-config webkitgtk ];

    # TODO list
    mainScript = "script/build.ts";

    lockfile = stdenv.mkDerivation {
      # https://deno.land/manual@v1.26.1/linking_to_external_code/integrity_checking
      # TODO deno vendor
      # https://github.com/denoland/deno/issues/13532
      inherit (webview_deno) version buildInputs src patches;
      # fixed output drv
      outputHash = lockfileHash;
      outputHashMode = "flat";
      outputHashAlgo = "sha256";
      pname = "${pname}-lockfile";
      buildPhase = ''
        ${setDenoDir}
        deno cache --lock=lock.json --lock-write ${mainScript}
      '';
      installPhase = ''
        cp lock.json $out
      '';
    };

    # not working. "deno task" does not take --lock
    #deno task --lock=${lockfile} build

    buildPhase = ''
      ${setDenoDir}
      echo "deno version:"
      deno --version
      mkdir -p $DENO_DIR
      ln -s ${deno2nix.internal.mkDepsLink lockfile} $DENO_DIR/deps
      deno run -A --unstable --lock=${lockfile} --cached-only ${mainScript}
    '';

    # produce $out/libwebview.so
    installPhase = ''
      cp -r build/ $out
    '';
  };

#   deno2nix-src = fetchFromGitHub {
#     # https://github.com/SnO2WMaN/deno2nix
#     owner = "SnO2WMaN";
#     repo = "deno2nix";
#     rev = "a2092563c48cb71adc397f496f05049d4f20647a";
#     sha256 = "sha256-P82KyZVr/hnBUcNOTNVeWV73FnXjMTjo9emF5rjiG2Y=";
#   };
#
#   # TODO simpler
#   inherit (import (deno2nix-src + "/nix/overlay.nix") pkgs {}) deno2nix;
#
#   setDenoDir = ''
#     export DENO_DIR=/tmp/deno
#   '';
#
# in
#
# webview_deno
