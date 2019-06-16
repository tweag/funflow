{
  extras = hackage:
    {
      packages = {
        "aws" = (((hackage.aws)."0.20").revisions).default;
        "hedis" = (((hackage.hedis)."0.12.5").revisions)."5e3f47b935a48e57c44197f715a26d88a33c46c045a8c19d75b0d7ebc4ae3cbe";
        } // {
        funflow = ./funflow.nix;
        funflow-aws = ./funflow-aws.nix;
        funflow-checkpoints = ./funflow-checkpoints.nix;
        funflow-jobs = ./funflow-jobs.nix;
        funflow-examples = ./funflow-examples.nix;
        funflow-cwl = ./funflow-cwl.nix;
        };
      };
  resolver = "lts-13.25";
  }