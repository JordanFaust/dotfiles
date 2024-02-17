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
        argo
        argocd

        # Local Dev
        unstable.kubernetes-helm
        kustomize
        unstable.skaffold

        # Terraform
        terraform
        terraform-docs

        # Istio
        istioctl

        # Linkerd
        linkerd

        # Consul
        consul

        # Devbox
        unstable.devbox

        # Load Testing Tooling
        k6

        # Security
        unstable.jfrog-cli
      ];

      environment.shellAliases = {
        k      = "kubectl";
      };
    })

    (mkIf cfg.xdg.enable {
    })
  ];
}
