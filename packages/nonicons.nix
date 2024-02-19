{ lib, inputs, stdenvNoCC, fetchFromGitHub, my,  ... }:

let pname = "nonicons";
    version = "0.0.18";
in stdenvNoCC.mkDerivation {
  inherit pname version;

  src =  fetchFromGitHub {
    owner = "yamatsum";
    repo = "nonicons";
    rev = "8454b3b6c3ceeee18b386b7882c5a071dcf0f3af";
    hash = "sha256-c2UUef5/l5ugKwWV8R3gijD6aorw9H4ca+mGjy+VyYE=";
  };

  # phases = "installPhase";
  installPhase = ''
    runHook preInstall

    echo 'listing directory content'
    ls -al ../
    echo 'listing dist content'
    ls -al dist/nonicons.ttf
    install -m444 -Dt $out/share/fonts/truetype dist/nonicons.ttf

    runHook postInstall
  '';

  meta = {
    homepage = "https://github.com/yamatsum/nonicons";
    description = "Progamming Language SVG Icons";
    license = lib.licenses.mit;
    platforms = [ "x86_64-linux" ];
    maintainers = [];
  };
}
