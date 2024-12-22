{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  minimal = config.modules.minimal;
in {
  config = lib.mkIf (!minimal) {
    # These are some common dev tools that are required
    home = {
      packages = with pkgs; [
        # Add AWS V2 CLI
        stable.awscli2
        stable.aws-vault
        stable.ssm-agent
        stable.ssm-session-manager-plugin

        # Kubernetes
        kubectx
        kubectl
        kind
        krew
        argo
        argocd

        # Local Dev
        unstable.kubernetes-helm
        helm-ls
        kustomize
        unstable.skaffold
        tilt
        k3d
        lens
        postman
        httpie
        insomnia

        # Terraform
        terraform
        terraform-docs

        # Networking API Tools
        istioctl
        linkerd
        consul
        deck

        # Devbox
        unstable.devbox

        # Load Testing Tooling
        k6

        # Security
        unstable.jfrog-cli

        # Steam Run as a last ditch effort
        steam-run

        # Nix Development
        nil

        # Markdown
        marksman
      ];

      shellAliases = {
        k = "kubectl";
      };
    };
  };
}
