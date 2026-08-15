{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
  bun,
  ...
}:
stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "tmux-palette";
  version = "0.2.1";

  src = fetchFromGitHub {
    owner = "eduwass";
    repo = "tmux-palette";
    rev = "v${finalAttrs.version}";
    hash = "sha256-bVdd8YAFYwob6y5nATcdLC8nzBKdaCDzqZHqyc+04/k=";
  };

  dontBuild = true;
  dontConfigure = true;

  installPhase = ''
    runHook preInstall

    # Install source tree to the Nix store (read-only).
    # The wrapper copies it to a writable cache dir on first run so that
    # bun install can populate node_modules there.
    mkdir -p $out/share/tmux-palette $out/bin
    cp -r . $out/share/tmux-palette/

    # Write the launcher wrapper.
    # Shell variables inside this block use ''$ to escape Nix interpolation.
    cat > $out/bin/tmux-palette.sh << 'WRAPPER_EOF'
#!/usr/bin/env bash
set -euo pipefail

SHARE="@out@/share/tmux-palette"
VERSION="@version@"
BUN="@bun@/bin/bun"
# Key the cache dir on the store output's hash rather than just the semver
# version. The output hash changes whenever any build input changes -
# including the bash used to patch shebangs in $SHARE - so a `nix-collect-
# garbage` that removes an old bash derivation can never leave us pointing
# at a stale, now-missing interpreter path.
STORE_HASH="$(basename "@out@")"
CACHE="''${XDG_CACHE_HOME:-$HOME/.cache}/tmux-palette/$STORE_HASH"
SENTINEL="$CACHE/.installed"

# On first invocation (or after the store output changes, e.g. a version
# bump or a rebuild of an input like bash), copy source to the writable
# cache directory and install node_modules with bun.
if [[ ! -f "$SENTINEL" ]]; then
  echo "tmux-palette: first-run setup v$VERSION..." >&2
  rm -rf "$CACHE"
  mkdir -p "$CACHE"
  cp -r "$SHARE/." "$CACHE/"
  # The Nix store is read-only; make the cache copy writable so bun can
  # create node_modules and write lock metadata.
  chmod -R u+w "$CACHE"
  (cd "$CACHE" && "$BUN" install --frozen-lockfile --silent) >&2
  touch "$SENTINEL"
fi

# Let sub-palette chaining (palette: "..." actions) find this wrapper.
export TMUX_PALETTE_BIN="$0"

exec "$CACHE/bin/tmux-palette.sh" "$@"
WRAPPER_EOF

    substituteInPlace $out/bin/tmux-palette.sh \
      --replace '@out@'     "$out" \
      --replace '@version@' "${finalAttrs.version}" \
      --replace '@bun@'     "${bun}"

    chmod +x $out/bin/tmux-palette.sh

    runHook postInstall
  '';

  meta = {
    description = "Raycast-style command palette for tmux — fast, scriptable, easy to extend";
    homepage = "https://github.com/eduwass/tmux-palette";
    license = lib.licenses.mit;
    mainProgram = "tmux-palette.sh";
    platforms = lib.platforms.unix;
  };
})
