SELECT *
  FROM environ.fxn_sea_ice_extent_by_date(0.15, --concentration,
                                          -1701089, --xmin
                                          -4280076, --ymin
                                          1341794 , --xmax
                                          -1741504, --ymax
                                          3571, --epsg of the bounding box
                                          '2005-04-01', --start_date,
                                          '2005-04-15' -- end_date
  )