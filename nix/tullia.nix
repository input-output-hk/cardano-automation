system: let
  ciInputName = "GitHub event";
in rec {
  tasks.ci = {config, lib, ...}: {
    preset = {
      nix.enable = true;

      github-ci = {
        enable = config.actionRun.facts != {};
        repo = "input-output-hk/cardano-automation";
        sha = config.preset.github-ci.lib.readRevision ciInputName null;
      };
    };

    command.text = ''
      nix build -L \
        .#hydraJobs.packages.bench-data-publish:exe:bench-data-publish.${system} \
        .#hydraJobs.devShells.default.${system} \
        .#hydraJobs.{plan-nix,roots}.${system}
    '';

    memory = 1024 * 8;
    nomad.resources.cpu = 10000;
  };

  actions."cardano-automation/ci" = {
    task = "ci";
    io = ''
      let github = {
        #input: "${ciInputName}"
        #repo: "input-output-hk/cardano-automation"
      }
      
      #lib.merge
      #ios: [
        #lib.io.github_push & github,
        #lib.io.github_pr   & github,
      ]
    '';
  };
}
