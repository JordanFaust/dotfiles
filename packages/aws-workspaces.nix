# see https://github.com/NixOS/nixpkgs/blob/88a55dffa4d44d294c74c298daf75824dc0aafb5/pkgs/by-name/aw/aws-workspaces/package.nix#L98
{
  stdenv,
  lib,
  inputs,
  makeWrapper,
  dpkg,
  fetchurl,
  autoPatchelfHook,
  curl,
  libkrb5,
  lttng-ust,
  libpulseaudio,
  gtk3,
  # openssl_1_1,
  openssl_legacy,
  icu70,
  webkitgtk_4_0,
  librsvg,
  gdk-pixbuf,
  libsoup_2_4,
  glib-networking,
  graphicsmagick_q16,
  libva,
  libusb1,
  hiredis,
  xcbutil,
}:
stdenv.mkDerivation (finalAttrs: {
  pname = "aws-workspaces";
  version = "4.7.0.4312";

  src = fetchurl {
    # Check new version at https://d3nt0h4h6pmmc4.cloudfront.net/ubuntu/dists/focal/main/binary-amd64/Packages
    urls = [
      "https://d3nt0h4h6pmmc4.cloudfront.net/ubuntu/dists/focal/main/binary-amd64/workspacesclient_${finalAttrs.version}_amd64.deb"
      "https://archive.org/download/workspacesclient_${finalAttrs.version}_amd64/workspacesclient_${finalAttrs.version}_amd64.deb"
    ];
    hash = "sha256-G0o5uFnEkiUWmkTMUHlVcidw+2x8e/KmMfVBE7oLXV8=";
  };

  nativeBuildInputs = [
    autoPatchelfHook
    makeWrapper
  ];

  # Crashes at startup when stripping:
  # "Failed to create CoreCLR, HRESULT: 0x80004005"
  dontStrip = true;

  buildInputs = [
    (lib.getLib stdenv.cc.cc)
    libkrb5
    curl
    lttng-ust
    libpulseaudio
    gtk3
    openssl_legacy.out
    icu70
    webkitgtk_4_0
    librsvg
    gdk-pixbuf
    libsoup_2_4
    glib-networking
    graphicsmagick_q16
    hiredis
    libusb1
    libva
    xcbutil
  ];

  unpackPhase = ''
    ${dpkg}/bin/dpkg -x $src $out
  '';

  preFixup = ''
    patchelf --replace-needed liblttng-ust.so.0 liblttng-ust.so $out/lib/libcoreclrtraceptprovider.so
    patchelf --replace-needed libGraphicsMagick++-Q16.so.12 libGraphicsMagick++.so.12 $out/usr/lib/x86_64-linux-gnu/pcoip-client/vchan_plugins/libvchan-plugin-clipboard.so
    patchelf --replace-needed libhiredis.so.0.14 libhiredis.so $out/lib/libpcoip_core.so
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin $out/lib
    mv $out/opt/workspacesclient/* $out/lib
    rm -rf $out/opt

    wrapProgram $out/lib/workspacesclient \
      --prefix LD_LIBRARY_PATH : "${lib.makeLibraryPath finalAttrs.buildInputs}" \
      --set GDK_PIXBUF_MODULE_FILE "${librsvg.out}/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache" \
      --set GIO_EXTRA_MODULES "${glib-networking.out}/lib/gio/modules"

    mv $out/lib/workspacesclient $out/bin

    runHook postInstall
  '';

  meta = with lib; {
    description = "Client for Amazon WorkSpaces, a managed, secure Desktop-as-a-Service (DaaS) solution";
    homepage = "https://clients.amazonworkspaces.com";
    license = licenses.unfree;
    mainProgram = "workspacesclient";
    maintainers = with maintainers; [];
    platforms = [ "x86_64-linux" ]; # TODO Mac support
    sourceProvenance = with sourceTypes; [ binaryNativeCode ];
  };
})
