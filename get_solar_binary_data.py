#!/usr/bin/env python

import os

class solar():
  def __init__(self, inArgs):
    """
    `get_solar_binary_data.py -h`
    """

    self.topDir = inArgs['top_dir']
    self.zRecID = inArgs['record_id']
  # end constructor

  def getSolar(self):
    """
    Retrieve `extract_solar` file dataset from Zenodo, extract
    archive, then stage files as expected by the model
    """

    from zenodo_get.__main__ import zenodo_get as zget

    # what can be downloaded? should be 4 binaries
    # save to list first, then download
    arcList = 'zenodo_archive_list.txt'
    zget([str(self.zRecID), '-w', arcList])
    zget([str(self.zRecID)])

    # now move downloaded files to data directory
    files = open(arcList).read().splitlines()
    for file in files:
      base = os.path.basename(file)
      os.rename(base, '{}/data/{}'.format(self.topDir, base))
    # end file loop
  # end getSolar()
# end solar class

if __name__ == '__main__':
  import argparse

  parser = argparse.ArgumentParser(\
    formatter_class=argparse.ArgumentDefaultsHelpFormatter, \
    description='Script that utilizes the Zenodo API to ' + \
    'download the binary files that are necessary for the AER ' + \
    '`extract_solar` executble to run. Data are saved to top_dir/data.')
  parser.add_argument('-d', '--top_dir', default='.', \
    help='Top-level path of AER `solar_source_function` repository.')
  parser.add_argument('-record', '--record_id', type=int, \
    default=4336064, help='Zenodo record ID for the ' + \
    'solar irradiance files.')
  args = parser.parse_args()

  sObj = solar(vars(args))
  sObj.getSolar()
# end main()
