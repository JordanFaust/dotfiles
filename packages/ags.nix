{
  lib,
  inputs,
  system,
  stdenv,
  cage,
  swww,
  bun,
  dart-sass,
  fzf,
  gtk3,
  fd,
  brightnessctl,
  accountsservice,
  slurp,
  wf-recorder,
  wl-clipboard,
  writeShellScript,
  wayshot,
  which,
  swappy,
  hyprpicker,
  pavucontrol,
  networkmanager,
}: let
  name = "desktop";
  version = "1.8.0";

  ags = inputs.ags.packages.${system}.default.override {
    extraPackages = [accountsservice];
  };

  dependencies = [
    which
    dart-sass
    fd
    fzf
    brightnessctl
    swww
    inputs.matugen.packages.${system}.default
    inputs.hyprland.packages.${system}.default
    slurp
    wf-recorder
    wl-clipboard
    wayshot
    swappy
    hyprpicker
    pavucontrol
    networkmanager
    gtk3
  ];

  addBins = list: builtins.concatStringsSep ":" (builtins.map (p: "${p}/bin") list);

  greeter = writeShellScript "greeter" ''
    export PATH=$PATH:${addBins dependencies}
    echo "starting greeter"
    ${lib.getExe cage} -d -s -m last ${ags}/bin/ags -- -c ${config}/greeter.js
  '';

  desktop = writeShellScript name ''
    export PATH=$PATH:${addBins dependencies}
    ${ags}/bin/ags -b ${name} -c ${config}/config.js $@
  '';

  config = stdenv.mkDerivation {
    inherit name version;
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
in
  stdenv.mkDerivation {
    inherit name version;

    src = config;

    installPhase = ''
      mkdir -p $out/bin

      cp -r . $out

      cp ${desktop} $out/bin/${name}
      cp ${greeter} $out/bin/greeter
    '';

    meta = {
      homepage = "https://github.com/JordanFaust/dotfiles";
      description = "GTK desktop widgets via AGS";
      # license = lib.licenses.unlicense;
      platforms = ["x86_64-linux"];
      maintainers = [];
      mainProgram = "${name}";
    };
  }
