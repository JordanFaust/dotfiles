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
        ssm-agent
        ssm-session-manager-plugin

        # Kubernetes
        kubectx
        kubectl
        kind
        krew
        argocd

        # Local Dev
        kubernetes-helm
        kustomize
        skaffold

        # Terraform
        terraform
        terraform-docs

        # Istio
        istioctl

        # Linkerd 
        linkerd
      ];

      environment.shellAliases = {
        k      = "kubectl";
      };
    })

    (mkIf cfg.xdg.enable {
    })
  ];
}
