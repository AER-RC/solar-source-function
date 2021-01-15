# Introduction

Solar Source Function (SSF)/Solar Irradiance calculations.

...

If any build or run issues occur, please [create an issue](https://github.com/AER-RC/solar-source-function/issues) or contact the [AER-RC Group](https://github.com/AER-RC).

# Cloning the Latest Release

Assuming the output directory should be `solar-source-function`:

`git clone git@github.com:AER-RC/solar-source-function.git`


Currently, the latest release is SSF v1.5, and it is recommended that this be the version that users clone and checkout (rather than the `master` branch). To do this, one needs to simply checkout the `v1.5` tag:

```
git checkout tags/v1.5
```

Instead of cloning, users can also download an SSF [tarball](https://github.com/AER-RC/solar-source_function/releases/tag/v2.7.1) and unpack it:

```
tar xvf solar-source-function_v1.5.tar.gz
mv solar-source-function_v1.5 extract_solar
```

Though not necessary, the move to `extract_solar` is for consistency with previous release packages and the associated documentation.

# Building

There are two makefiles: `Makefile.pgi` (uses `pgf90`) and `Makefile.gnu` (uses `gfortran`). There are three database options (see `extract_solar_instructions` for details):

To compile:

```
make -f Makefile.xxx
```

where

```
xxx: pgi or gnu
```

# Dependencies

Before running the download script, the user will need to install the Zenodo Python API, which can be done with:

```
pip install -r requirements.txt
```

Note that we install `v1.3.0` of the package, which was released in February 2020. The code in this repository is currently incompatible with `v1.3.2`.

# Downloading and Staging the SSF Binary Inputs

extract_solar requires a solar source function; there are four different versions available, which must be downloaded from Zenodo.
Assuming the user has `cd`'d into `solar-source-function`, they can download and untar the necessary binary inputs with:

```
./get_solar_binary_data.py
```

Currently, there are 4 files downloaded:

```
Title: Extract Solar Model Inputs
Keywords:
Publication date: 2020-12-17
DOI: 10.5281/zenodo.4336064
Total size: 299.1 MB

Link: https://zenodo.org/api/files/71e7b1cf-196b-4db9-87be-f82f8e411d0d/build_comb_solar_rad_avg.bin   size: 66.0 MB
100% [........................................................................] 69215420 / 69215420
Checksum is correct. (76dd81bb1ebe605580e77b7d80d6141d)

Link: https://zenodo.org/api/files/71e7b1cf-196b-4db9-87be-f82f8e411d0d/build_comb_solar_rad_multi_comp.bin   size: 197.9 MB
100% [......................................................................] 207489980 / 207489980
Checksum is correct. (1c3f1ea707437c874df7d6342d058d56)

Link: https://zenodo.org/api/files/71e7b1cf-196b-4db9-87be-f82f8e411d0d/solar.kurucz.rad.mono.0.4R.bin   size: 8.8 MB
100% [..........................................................................] 9227840 / 9227840
Checksum is correct. (9b452c10afcd8ed42975c4578be95bea)

Link: https://zenodo.org/api/files/71e7b1cf-196b-4db9-87be-f82f8e411d0d/solar.kurucz.rad.mono.full_disk.bin.little_Endian   size: 26.4 MB
100% [........................................................................] 27669480 / 27669480
Checksum is correct. (cd6451e629f11409a34a5d5c2c42c02b)
All files have been downloaded.
```

They are all moved into the `data` directory of this repository.

# Run Example
