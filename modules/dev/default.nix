{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev;
in {
  options.modules.dev = {
    xdg.enable = mkBoolOpt true;
  };

  config = mkMerge [
    # These are some common dev tools that are required
    ({
      user.packages = with pkgs; [
        # Add AWS V2 CLI
        awscli2
        aws-vault

        # Kubernetes
        kubectx
        kubectl
        kind
        krew

        # Local Dev
        kubernetes-helm
        skaffold

        # Terraform
        terraform
        terraform-docs

        # Istio
        istioctl
      ];

      environment.shellAliases = {
        k      = "kubectl";
      };
    })

    (mkIf cfg.xdg.enable {
    })
  ];
}
