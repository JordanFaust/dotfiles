{ lib
, inputs
, system
, stdenv
, cage
, writeShellScriptBin
, writeScriptBin
, swww
, bun
, dart-sass
, fd
, brightnessctl
, accountsservice
, slurp
, wf-recorder
, wl-clipboard
, wayshot
, swappy
, hyprpicker
, pavucontrol
, networkmanager
}:

let
  ags = inputs.ags.packages.${system}.default.override {
    extraPackages = [accountsservice];
  };

  pname = "desktop";
  config = stdenv.mkDerivation {
    inherit pname;
    version = "1.7.7";
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

  addBins = list: builtins.concatStringsSep ":" (builtins.map (p: "${p}/bin") list);
in {
  inherit config;
  desktop = writeScriptBin pname ''
    export PATH=$PATH:${addBins [
      dart-sass
      fd
      brightnessctl
      swww
      slurp
      wf-recorder
      wl-clipboard
      wayshot
      swappy
      hyprpicker
      pavucontrol
      networkmanager
    ]}
    ${ags}/bin/ags -b ${pname} -c ${config}/config.js $@
  '';
  greeter = { cursor }: writeScriptBin "greeter" ''
    export XCURSOR_THEME=${cursor}
    export PATH=$PATH:${dart-sass}/bin
    export PATH=$PATH:${fd}/bin
    ${cage}/bin/cage -ds -m last ${ags}/bin/ags -- -c ${config}/greeter.js
  '';
}

