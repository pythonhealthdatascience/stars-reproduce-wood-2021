# Reproduction README

## Model summary

> Wood RM, Pratt AC, Kenward C, McWilliams CJ, Booton RD, Thomas MJ, Bourdeaux CP, Vasilakis C. **The Value of Triage during Periods of Intense COVID-19 Demand: Simulation Modeling Study**. *Medical Decision Making* 41(4):393-407 (2021). <https://doi.org/10.1177/0272989X21994035>.

This study uses discrete-event simulation to explore the deaths and life years lost under different triage strategies for an intensive care unit, relative to a baseline strategy. The unit is modelled with 20 beds (varied from 10 to 200 in sensitivity analyses). Three different triage strategies are explored, under three different projected demand trajectories.

## Scope of the reproduction

In this assessment, we attempted to reproduce 5 items: 4 figures and 1 table.

## Reproducing these results

### Repository overview

```
├── docker
│   └──  ...
├── inputs
│   └──  ...
├── outputs
│   └──  ...
├── renv
│   └──  ...
├── scripts
│   └──  ...
├── tests
│   └──  ...
├── .Rprofile
├── DESCRIPTION
├── README.md
├── wood2021.Rproj
└── renv.lock
```

* `docker/` - Instructions for creation of docker container.
* `inputs/` - Some of the input parameters for the model, stored in `.csv`
* `outputs/` - Outputs files from the scripts (e.g. `.csv`, `.txt`, `.png`)
* `renv/` - Instructions for creation of R environment
* `scripts/` - Code for the model and for reproducing items from the scope
* `tests/` - Test to check that a simple run of the model produces consistent results with our reproduction
* `.Rprofile` - Activates R environment
* `DESCRIPTION` - Lists packages that we installed into environment (their dependencies will have also been installed)
* `README.md` - This file!
* `wood2021.Rproj` - Project settings
* `renv.lock` - Lists R version and all packages in the R environment

### Step 1. Set up environment

Before you can run the model, you will need to create an R environment with the correct version of R and the specified packages.

#### Option A. Renv

An `renv` environment has been provided. To create this environment locally on your machine, you should open the R project with the R environment loaded, and then run:

```
renv::restore()
```

In `renv.lock`, you will see the version of R listed. However, `renv` will not install this for you, so you will need to switch to this yourself if you wish to also use the same version of R. This reproduction has been run in R 4.4.1, and it is possible (although not definite) that later versions of R may not be compatible, or that you may encounter difficulties installing the specified package versions in later versions of R.

#### Option B. Build local docker image

A Dockerfile is provided, which you can use to build the Docker image. The docker image will include the correct version of R, the required packages and their versions, and an installation of RStudio which you can run from your browser. It will also include the scripts and outputs from this directory. For this option and option C, you'll need to ensure that `docker` is installed on your machine.

To create the docker image and then open up RStudio:

1. In the terminal, navigate to the parent directory of your `reproduction/` folder
2. Build the image:

```
docker build --tag wood2021 . -f ./reproduction/docker/Dockerfile
```

3. Create container and open RStudio:

```
(sleep 2 && xdg-open http://localhost:8888) & sudo docker run -it -p 8888:8787 -e DISABLE_AUTH=true --name wood2021_docker wood2021
```

#### Option C. Pull pre-built docker image

A pre-built image is available on the GitHub container registry. To use it:

1. Create a Personal Access Token (Classic) for your GitHub account with `write:packages` and `delete:packages` access
2. On terminal, run the following command and then enter your sudo password (if prompted), followed by the token just generated (which acts as your GitHub password)

```
sudo docker login ghcr.io -u githubusername
```

3. Download the image:

```
sudo docker pull ghcr.io/pythonhealthdatascience/wood2021
```

4. Create container and open RStudio:

```
(sleep 2 && xdg-open http://localhost:8888) & sudo docker run -it -p 8888:8787 -e DISABLE_AUTH=true --name wood2021_docker ghcr.io/pythonhealthdatascience/wood2021:latest
```

### Step 2. Running the model

#### Option A: Execute the R scripts

To run the model, simply run `main.R`.

To produce the tables and figures in the paper, run `fig4_fig5_fig6_tbl4.R` and `fig7.R`.

### Option B: Testthat

A small version of one the model scenarios is provided as a test within `tests/testthat`. You can run this scenario by running the following command from your R console whilst in the `reproduction/` directory:

```
testthat::test_dir("tests/testthat")
```

The test should only take about **40 seconds** to run, depending on your machine specs.

## Reproduction specs and runtime

Within this reproduction, due to long run times, the model was run on a remote machine. This was an Intel Core i9-13900K with 81GB RAM running Pop!_OS 22.04 Linux. The total run time was **48 hours 25 minutes**. It ran in a single loop, so required the computer to remain on for that time. This time includes all scenarios - but, for just the base scenario, the run time was **2 hours 3 minutes**.

## Citation

To cite the original study, please refer to the reference above. To cite this reproduction, please refer to the CITATION.cff file in the parent folder.

## License

This repository is licensed under the GNU General Public License v3.0.