{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }: let
    pkgs = nixpkgs.legacyPackages.x86_64-linux;
    # what am I doing
    joinLDPaths = 
      l: pkgs.lib.strings.concatStringsSep ":" (map (d: "${d}/lib") l);
  in {

    packages.x86_64-linux.hello = nixpkgs.legacyPackages.x86_64-linux.hello;

    packages.x86_64-linux.default = self.packages.x86_64-linux.hello;

    devShells.x86_64-linux.default = pkgs.mkShell {
      name = "minesweeper-hs";
      # this just doesn't work lol idk will figure it out later
      buildInputs = with pkgs.haskellPackages; [
        cabal-install
        ghc
      ];
      LD_LIBRARY_PATH = joinLDPaths [
        pkgs.zlib
        pkgs.zstd.out
        pkgs.SDL2
        pkgs.xz.out
        pkgs.bzip2.out
      ];
    };
  };
}
