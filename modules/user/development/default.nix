{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  inherit (config.modules) minimal;
in {
  config = lib.mkIf (!minimal) {
    # These are some common dev tools that are required
    home = {
      packages = with pkgs; [
        # Add AWS V2 CLI
        awscli2
        aws-vault
        amazon-ssm-agent
        ssm-session-manager-plugin

        # Performance Monitoring
        bmon
        btop
        htop

        # Kubernetes
        kubectx
        kubectl
        kind
        krew
        kubectl-view-allocations
        k9s
        argo-workflows
        argocd

        # Local Dev
        kubernetes-helm
        helm-ls
        tilt
        k3d
        lens
        postman
        httpie
        insomnia
        regclient
        circleci-cli
        yajsv
        cloc
        statix
        graphviz

        # Terraform
        terraform
        terraform-docs

        # Networking API Tools
        cfssl
        istioctl
        egctl
        linkerd
        consul
        deck
        socat

        # Devbox
        devbox

        # Load Testing Tooling
        k6

        # Security
        jfrog-cli

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
