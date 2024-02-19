{ lib
, inputs
, system
, stdenv
, writeShellScriptBin
, writeScriptBin
, swww
, bun
, dart-sass
, fd
, brightnessctl
, accountsservice
}:

let
  ags = inputs.ags.packages.${system}.default.override {
    extraPackages = [accountsservice];
  };

  pname = "catppuccin";
  config = stdenv.mkDerivation {
    inherit pname;
    version = "1.7.6";
    src = ./ags;

    buildPhase = ''
      ${bun}/bin/bun build ./main.ts \
        --outfile main.js \
        --external "resource://*" \
        --external "gi://*"

      ${bun}/bin/bun build ./greeter/greeter.ts \
        --outfile greeter.js \
        --external "resource://*" \
        --external "gi://*"
    '';

    installPhase = ''
      mkdir $out
      cp -r assets $out
      cp -r style $out
      cp -r greeter $out
      cp -r widget $out
      cp -f main.js $out/config.js
      cp -f greeter.js $out/greeter.js
    '';
  };
in {
  desktop = {
    inherit config;
    script = writeScriptBin pname ''
      export PATH=$PATH:${dart-sass}/bin
      export PATH=$PATH:${fd}/bin
      export PATH=$PATH:${brightnessctl}/bin
      export PATH=$PATH:${swww}/bin
      ${ags}/bin/ags -b ${pname} -c ${config}/config.js $@
    '';
  };
  greeter = {
    inherit config;
    # script = writeShellScriptBin "greeter" ''
    script = writeScriptBin "greeter" ''
      export PATH=$PATH:${dart-sass}/bin
      export PATH=$PATH:${fd}/bin
      ${ags}/bin/ags -b ${pname} -c ${config}/greeter.js $@
    '';
  };
  config = config;
}

