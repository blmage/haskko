options:
with (builtins.fromJSON (builtins.readFile ./nixpkgs.json));
let
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };
  config.allowUnfree = true;
  config.allowBroken = false;
  overlays = [ (import ./wasm)
               (import ./overlay.nix options)
             ] ++ options.overlays;
in
  import nixpkgs
    { inherit overlays config;
    }
