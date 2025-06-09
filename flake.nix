{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }: {

    packages.x86_64-linux = {
      puffin = nixpkgs.legacyPackages.x86_64-linux.buildGoModule {
        pname = "puffin";
        version = "git";
        vendorHash = "sha256-XfsGXh9nz+QJEAqtw/BfzHhSVje6q0KLsVg1ZQ6fN4U=";
        src = ./.;
      };
      default = self.packages.x86_64-linux.puffin;
    };


  };
}
