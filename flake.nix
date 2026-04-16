{
  description = "streamly-statistics";

  inputs = {
    basepkgs.url = "git+ssh://git@github.com/composewell/streamly-packages?rev=187bde6bd362eced707ed96198ddc193af66ff42";
    nixpkgs.follows = "basepkgs/nixpkgs";
    nixpkgs-darwin.follows = "basepkgs/nixpkgs-darwin";
  };

  outputs = { self, nixpkgs, nixpkgs-darwin, basepkgs }:
    basepkgs.nixpack.mkOutputs {
      inherit nixpkgs nixpkgs-darwin basepkgs;
      name = "streamly-statistics";
      sources = basepkgs.nixpack.lib.localSource "streamly-statistics" ./.;
      packages = basepkgs.nixpack.lib.devPackage "streamly-statistics";
      #sources = import ./sources.nix;
      #packages = import ./packages.nix;
    };
}
