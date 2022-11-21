systems: let
  ciInputName = "GitHub event";
  repository = "input-output-hk/cardano-automation";
in rec {
  tasks.ci = {config, lib, ...}: {
    preset = {
      nix.enable = true;

      github.ci = {
        enable = config.actionRun.facts != {};
        inherit repository;
        revision = config.preset.github.lib.readRevision ciInputName null;
      };
    };

    command.text = config.preset.github.status.lib.reportBulk {
      bulk.text = "echo ${lib.escapeShellArg (builtins.toJSON systems)} | nix-systems -i";
      each.text = ''
        nix build -L \
          .#hydraJobs.packages.bench-data-publish:exe:bench-data-publish."$1" \
          .#hydraJobs.devShells.default."$1" \
          .#hydraJobs.{plan-nix,roots}."$1"
      '';
      skippedDescription = lib.escapeShellArg "No nix builder available for this system";
    };

    memory = 1024 * 10;
    nomad.resources.cpu = 10000;
  };

  actions."cardano-automation/ci" = {
    task = "ci";
    io = ''
      let github = {
        #input: "${ciInputName}"
        #repo: "${repository}"
      }
      
      #lib.merge
      #ios: [
        #lib.io.github_push & github,
        #lib.io.github_pr   & github,
      ]
    '';
  };
}
