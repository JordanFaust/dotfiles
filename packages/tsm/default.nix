{
  lib,
  buildGoModule,
  ...
}:
buildGoModule {
  pname = "tsm";
  version = "0.1.0";

  src = ./.;

  vendorHash = null;

  meta = {
    description = "tmux session manager — git repo discovery and FZF session picker";
    mainProgram = "tsm";
    platforms = lib.platforms.unix;
  };
}
