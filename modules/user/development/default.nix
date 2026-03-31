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
      packages = with pkgs;
        [
          # AWS
          awscli2
          aws-vault
          ssm-session-manager-plugin

          # Performance Monitoring
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
          jira-cli-go
          yajsv
          cloc
          statix
          graphviz
          mise

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

          # Load Testing
          k6

          # Security
          jfrog-cli

          # Nix Development
          nil
        ]
        ++ lib.optionals pkgs.stdenv.isLinux [
          # Linux-only: SSM agent daemon, Steam FHS wrapper, Linux network monitor
          amazon-ssm-agent
          steam-run
          bmon
        ];

      shellAliases = {
        k = "kubectl";
      };
    };
  };
}
