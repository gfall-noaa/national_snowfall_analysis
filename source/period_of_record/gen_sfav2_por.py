#!/usr/bin/python3

import datetime as dt
import calendar
import os
import errno
import gzip
import numpy as np
import sys
from osgeo import gdal,osr,gdalconst
import cartopy.crs as ccrs
import matplotlib as mpl
import matplotlib.colors as mplcol
import matplotlib.pyplot as mplplt
import math
import cartopy.feature
from cartopy.feature import NaturalEarthFeature as cfNEF
from cartopy.feature import LAND, COASTLINE
import argparse
from mpl_toolkits.basemap import Basemap


def zvalue_from_index(arr, ind):
    """
    Helper function from
    https://krstn.eu/np.nanpercentile()-there-has-to-be-a-faster-way
    See also
    https://stackoverflow.com/questions/2374640
    arr has to be a 3D array (num_z, num_rows, num_cols)
    ind has to be a 2D array (num_rows, num_cols)
    """
    # Get number of rows and columns.
    _,num_rows,num_cols = arr.shape

    # Get linear indices.
    idx = num_rows * num_cols * ind + \
        np.arange(num_rows*num_cols).reshape((num_rows,num_cols))

    # Extract elements with np.take().
    return np.take(arr, idx)


def ma_quantile(arr, quantile, ndv):
    """
    A faster version of numpy.nanquantile from
    https://krstn.eu/np.nanpercentile()-there-has-to-be-a-faster-way
    modified to work with masked arrays.
    arr has to be a 3D array (num_z, num_rows, num_cols)
    """
    # Count valid (non-masked) values along the first axis.
    num_valid = np.sum(np.invert(arr.mask), axis=0)

    # Identify locations where there are no valid data.
    no_valid = num_valid == 0

    # Replace masked values with the maximum of the flattened array.
    arr_copy = np.copy(np.ma.getdata(arr))
    arr_copy[arr.mask] = np.amax(arr)

    # Sort values along the z axis. Formerly masked values will be at the
    # end.
    arr_copy = np.sort(arr_copy, axis=0)

    # Loop over requested quantiles.
    if type(quantile) is list:
        quantiles = []
        quantiles.extend(quantile)
    else:
        quantiles = [quantile]

    # if len(quantiles) < 2:
    #     quant_arr = np.zeros(shape=(arr.shape[1], arr.shape[2]))
    # else:
    #     quant_arr = np.zeros(shape=(len(quantiles),
    #                                 arr.shape[1], arr.shape[2]))
    # quant_arr = np.ma.masked_where(quant_arr == 0.0, quant_arr)

    result = []
    # print('>>')
    for i in range(len(quantiles)):

        quant = quantiles[i]

        # Desired (floating point) position for each row/column as well
        # as floor and ceiling of it.
        k_arr = (num_valid - 1) * quant
        f_arr = np.floor(k_arr).astype(np.int32)
        c_arr = np.ceil(k_arr).astype(np.int32)

        # Identify locations where the desired quantile hit exactly.
        fc_equal_k_mask = f_arr == c_arr

        # Interpolate.
        floor_val = zvalue_from_index(arr=arr_copy, ind=f_arr) * (c_arr - k_arr)
        ceil_val = zvalue_from_index(arr=arr_copy, ind=c_arr) * (k_arr - f_arr)

        quant_arr = floor_val + ceil_val
        quant_arr[fc_equal_k_mask] = \
            zvalue_from_index(arr=arr_copy,
                              ind=k_arr.astype(np.int32))[fc_equal_k_mask]

        # Re-mask locations where there are no valid data.
        quant_arr[no_valid] = ndv
        quant_arr = np.ma.masked_where(quant_arr == ndv, quant_arr)

        result.append(quant_arr)
    #     print(quant_arr[0,0])
    #     print(result[i][0,0])
    #     print(np.ma.getdata(result[i])[0,0])

    # print('<<')
    return result


def nan_quantile(arr, quantile):
    """
    A faster version of numpy.nanquantile from
    https://krstn.eu/np.nanpercentile()-there-has-to-be-a-faster-way
    arr has to be a 3D array (num_z, num_rows, num_cols)
    """

    # Count valid (non-NaN) values along the first axis.
    num_valid = np.sum(np.isfinite(arr), axis=0)

    # Identify locations where there are no non-nan data.
    no_valid = num_valid == 0

    # Replace np.nan with the maximum of the flattened array.
    arr[np.isnan(arr)] = np.nanmax(arr)

    # Sort values. Former np.nan values will be at the end.
    arr = np.sort(arr, axis=0)

    # Loop over requested quantiles.
    if type(quantile) is list:
        quantiles = []
        quantiles.extend(quantile)
    else:
        quantiles = [quantile]

    # if len(quantiles) < 2:
    #     quant_arr = np.zeros(shape=(arr.shape[1], arr.shape[2]))
    # else:
    #     quant_arr = np.zeros(shape=(len(quantiles),
    #                                 arr.shape[1], arr.shape[2]))

    result = []
    for i in range(len(quantiles)):

        quant = quantiles[i]

        # Desired (floating point) position for each row/column as well
        # as floor and ceiling of it.
        k_arr = (num_valid - 1) * quant
        f_arr = np.floor(k_arr).astype(np.int32)
        c_arr = np.ceil(k_arr).astype(np.int32)

        # Identify locations where the desired quantile hit exactly.
        fc_equal_k_mask = f_arr == c_arr

        # Interpolate.
        floor_val = zvalue_from_index(arr=arr, ind=f_arr) * (c_arr - k_arr)
        ceil_val = zvalue_from_index(arr=arr, ind=c_arr) * (k_arr - f_arr)

        quant_arr = floor_val + ceil_val
        quant_arr[fc_equal_k_mask] = \
            zvalue_from_index(arr=arr,
                              ind=k_arr.astype(np.int32))[fc_equal_k_mask]
        quant_arr[no_valid] = np.nan

        result.append(quant_arr)

    return result


# class GeoRasterDS:
#     """
#     Geographic (lon/lat) raster dataset structure.
#     """
#     def __init__(self,
#                  gdal_ds,  # GDAL dataset
#                  ccrs,     # cartopy CRS
#                  x_ll_ctr, # x min (center) of raster in CRS (i.e., native)
#                  nx,       # x grid size (number of columns)
#                  dx,       # x grid spacing
#                  y_ll_ctr, # y min (center) of raster in CRS (i.e., native)
#                  ny,       # y grid size (number of rows)
#                  dy):      # y grid spacing
#         self.gdal_ds = gdal_ds
#         self.ccrs = ccrs
#         self.x_ll_ctr = x_ll_ctr
#         self.nx = nx
#         self.dx = dx
#         self.y_ll_ctr = y_ll_ctr
#         self.ny = ny
#         self.dy = dy


def GF_rcParams():
    """
    Set rcParams for matplotlib.
    """

    # Maybe this should be more of a stylesheet kind of thing. Will look
    # into it later.

    # Make colorbar tick labels smaller.
    #mpl.rcParams['xtick.labelsize']='small'
    mpl.rcParams['ytick.labelsize']='small'

    # Set the typeface for plots.
    # My favorite monospace
    #mpl.rcParams['font.family'] = 'monospace'
    #mpl.rcParams['font.monospace'] = 'Inconsolata'
    # My favorite sans
    # Use this to get a list of TrueType fonts on the system.
    #>>> import matplotlib.font_manager
    #>>> matplotlib.font_manager.findSystemFonts(fontpaths=None, fontext='ttf')
    mpl.rcParams['font.family'] = 'sans-serif'
    # mpl.rcParams['font.sans-serif'] = 'Calibri'

    # Eliminate the toolbar in plot windows.
    #mpl.rcParams['toolbar'] = 'None'

    # Some colors.
    mpl.rcParams['figure.facecolor'] = '#e8f2ffff'
    mpl.rcParams['text.color'] = 'k'
    mpl.rcParams['ytick.color'] = 'k'

    # Default margins for subplots.
    mpl.rcParams['figure.subplot.left'] = 0.025
    mpl.rcParams['figure.subplot.bottom'] = 0.025
    mpl.rcParams['figure.subplot.right'] = 0.975
    mpl.rcParams['figure.subplot.top'] = 0.900


def daily_snowfall_colors():
    """
    Define a color ramp to display daily snowfall amounts in inches.
    """

    red = np.array([228, 189, 107, 49,
                    8, 8,
                    255, 255, 255, 219, 158, 105, 54, 204,
                    159, 124, 86], dtype=np.float64) / 255.0
    grn = np.array([238, 215, 174, 130,
                    81, 38,
                    255, 196, 135, 20, 0, 0, 0, 204,
                    140, 82, 28], dtype=np.float64) / 255.0
    blu = np.array([245, 231, 214, 189,
                    156, 148,
                    150, 0, 0, 0, 0, 0, 0, 255,
                    216, 165, 114], dtype=np.float64) / 255.0

    rgb = np.transpose(np.array([red,grn,blu]))

    colormap = mplcol.ListedColormap(rgb, name='Snowfall')

    colormap.set_under(color='#ffffffff')
    colormap.set_over(color='#2e0033ff') # 46, 0, 51

    # Set colors for a discrete colormap shoing inches of snowfall.
    col_levels = [0.0, 0.1, 1.0, 2.0, 3.0,
                  4.0, 6.0,
                  8.0, 12.0, 18.0, 24.0, 30.0, 36.0, 48.0, 60.0,
                  72.0, 96.0, 120.0]
    tick_levels = col_levels
    tick_labels = ['0', '0.1', '1', '2', '3', '4', '6', '8',
                   '12', '18', '24', '30', '36', '48', '60',
                   '72', '96', '120']
    norm = mplcol.BoundaryNorm(col_levels, len(col_levels) - 1)

    snowfall_color_ramp = {'colormap': colormap,
                           'col_levels': col_levels,
                           'tick_levels': tick_levels,
                           'tick_labels': tick_labels,
                           'norm': norm,
                           'extend': 'both'}

    return snowfall_color_ramp












def zero_to_ten_colors():
    """
    Define a color ramp to display numbers from 0 to 10.
    """

    # colorbrewer2.org 11-color qualitative.
    red = np.array([255, 106, 202, 255, 253, 227, 251, 51, 178, 31, 166],
                   dtype=np.float64) / 255.0
    grn = np.array([255, 61, 178, 127, 191, 26, 154, 160, 223, 120, 206],
                   dtype=np.float64) / 255.0
    blu = np.array([153, 154, 214, 0, 111, 28, 153, 44, 138, 180, 227],
                   dtype=np.float64) / 255.0

    rgb = np.transpose(np.array([red,grn,blu]))

    colormap = mplcol.ListedColormap(rgb, name='zero_to_ten')

    col_levels = [-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5,
                  8.5, 9.5, 10.5]
    tick_levels = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    tick_labels = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10']
    norm = None

    zero_to_ten_color_ramp = {'colormap': colormap,
                              'col_levels': col_levels,
                              'tick_levels': tick_levels,
                              'tick_labels': tick_labels,
                              'norm': norm,
                              'extend': 'neither'}

    return zero_to_ten_color_ramp












def zero_to_eleven_colors():
    """
    Define a color ramp to display numbers from 0 to 11.
    """

    # colorbrewer2.org 11-color qualitative.
    red = np.array([177, 255, 106, 202, 255, 253, 227, 251, 51, 178, 31, 166],
                   dtype=np.float64) / 255.0
    grn = np.array([89, 255, 61, 178, 127, 191, 26, 154, 160, 223, 120, 206],
                   dtype=np.float64) / 255.0
    blu = np.array([40, 153, 154, 214, 0, 111, 28, 153, 44, 138, 180, 227],
                   dtype=np.float64) / 255.0

    rgb = np.transpose(np.array([red,grn,blu]))

    colormap = mplcol.ListedColormap(rgb, name='zero_to_eleven')

    col_levels = [-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5,
                  8.5, 9.5, 10.5, 11.5]
    tick_levels = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
    tick_labels = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10',
                  '11']
    norm = None

    zero_to_eleven_color_ramp = {'colormap': colormap,
                                 'col_levels': col_levels,
                                 'tick_levels': tick_levels,
                                 'tick_labels': tick_labels,
                                 'norm': norm,
                                 'extend': 'neither'}

    return zero_to_eleven_color_ramp

































def geo_grid_map(crs, figure_x_size, figure_y_size, num, bbox,
                 x_ctr_data, y_ctr_data, dataset, raster_band_index, ndv,
                 title, color_ramp, color_ramp_label, cbar_geom=None,
                 cbar_labelsize='medium', cbar_fontsize='large',
                 use_basemap=False):
    """
    Generate a geographic map of single band from a GDAL raster dataset.

    crs: The cartopy.crs object describing the coordinate system.
    figure_x_size: The figure x size in inches.
    figure_y_size: The figure y size in inches.
    num: The num argument that will be provided to matplotlib.pyplot.subplots
         (and thereby provided to matplotlib.pyplot.figure) identifying the
         figure.
    bbox: The bounding box of the raster, in the coordinate system of the
          data.
    x_ctr_data: The list of x axis locations for the data (i.e. the center
                coordinates of each column).
    y_ctr_data: The list of y axis locations for the data (i.e. the center
                coordinates of each row). This should apply to the data in a
                north-up orientation, even though the grids in the GDAL
                dataset are generally oriented north-down.
    dataset: The GDAL raster dataset.
    raster_band_index: The raster band to show, indexed from 1.
    ndv: The no-data value.
    title: A string for the plot title.
    color_ramp: a dictionary describing the color ramp to use for the data:
                {'colormap': matplotlib.colors colormap object
                 'col_levels': colorbar axis levels bounding each color
                 'tick_levels': colorbar tick levels
                 'tick_labels': colorbar tick labels
                 'norm': colormap index from a call to
                         matplotlib.colors.BoundaryNorm
                 'extend': 'min', 'max', 'both', or 'neither'}
    color_ramp_label: Typically the units of the values on the colorbar
                      axis.
    """

    # fig, ax = mplplt.subplots(subplot_kw=dict(projection=crs),
    #                           figsize=(figure_x_size, figure_y_size),
    #                           num=num,
    #                           clear=True,
    #                           constrained_layout=False)

    fig = mplplt.figure(figsize=(figure_x_size, figure_y_size),
                        clear=True)
    # By setting the projection keyword for the Axes, an instance of the
    # GeoAxes class is created. This is a subclass of
    # matplotlib.axes.Axes and behaves a little differently from it.
    # https://scitools.org.uk/cartopy/docs/v0.13/matplotlib/geoaxes.html
    ax = mplplt.axes(projection=crs)
    ax.set_extent(bbox, crs=crs)

    # print(bbox)
    # print('####')
    # print(ax.dataLim)
    # print(ax.viewLim)
    # print('####')

    # For GeoAxes you cannot use the usual set_facecolor method as you
    # would in a regular axis for missing data.
    # https://github.com/SciTools/cartopy/issues/880
    # ax.background_patch.set_facecolor('#e4e4e4ff')
    ax.set_facecolor('#e4e4e4ff')

    if use_basemap:
        # Temporarily use basemap draw state, national, and continental
        # boundaries. This requires defining the coordinate system and
        # bounding box as a basemap entity that coexists with the GeoAxes
        # defined above. This option is necessary on CentOS 7 systems, where
        # cartopy 0.13 is unable to draw continent, state, and national
        # boundaries. 
        # srs = osr.SpatialReference()
        # srs.ImportFromWkt(dataset.GetProjection())
        # print(srs)
        # print(type(srs))
        # proj4 = srs.ExportToProj4()
        # print(proj4)
        # print(bbox)
        # print(type(crs))
        # print(type(crs.proj4_params))

        # for key in crs.proj4_params:
        #      print(key, crs.proj4_params[key])

        #     print('--')
        #pprint(vars(crs))
        # print('--')
        #print(dir(crs))
        # print('--')
        crs_proj_name = crs.proj4_params['proj']
        if crs_proj_name == 'lcc':
            x_0 = 0.5 * (bbox[0] + bbox[1])
            y_0 = 0.5 * (bbox[2] + bbox[3])
            crs_geo = ccrs.Geodetic(globe=crs.globe)
            llcrnrlon, llcrnrlat = \
                crs_geo.transform_point(bbox[0], bbox[2], crs)
            urcrnrlon, urcrnrlat = \
                crs_geo.transform_point(bbox[1], bbox[3], crs)
            lon_0, lat_0 = crs_geo.transform_point(x_0, y_0, crs)
            print(lon_0, lat_0)
            # print(bbox[1] - bbox[0])
            # print(bbox[3] - bbox[2])
            # width=bbox[1] - bbox[0],
            # height=bbox[3] - bbox[2],
            print(llcrnrlon,llcrnrlat)
            print(urcrnrlon,urcrnrlat)
            map = Basemap(ax=ax._axes,
                          projection='lcc',
                          lon_0=crs.proj4_params['lon_0'],
                          lat_0=crs.proj4_params['lat_0'],
                          lat_1=crs.proj4_params['lat_1'],
                          lat_2=crs.proj4_params['lat_2'],
                          llcrnrlon=llcrnrlon,
                          llcrnrlat=llcrnrlat,
                          urcrnrlon=urcrnrlon,
                          urcrnrlat=urcrnrlat,
                          # area_thresh=100.0,
                          resolution='i',
                          anchor='SW')
            # Polygons:
            # 0: North America, West Coast
            # 1: 
            polygons_to_show = [0, # N. America, West Coast
                                1,
                            ]
        elif crs_proj_name == 'eqc':
            lon_0 = 0.5 * (bbox[0] + bbox[1])
            lat_0 = 0.5 * (bbox[2] + bbox[3])
            # print(lon_0, lat_0)
            map = Basemap(ax=ax._axes,
                          projection='cyl',
                          llcrnrlon=bbox[0],
                          llcrnrlat=bbox[2],
                          urcrnrlon=bbox[1],
                          urcrnrlat=bbox[3],
                          # area_thresh=100.0,
                          resolution='i')
            # Polygons:
            polygons_to_show = [0, # N. America, West Coast
                                1,
                                2,
                                3,
                                4, # Hudson Bay
                                5, # St. Lawrence River
                                6, # Gulf of St. Lawrence
                                7,
                                8, # Nova Scotia
                                9, # Nova Scotia
                                10, # N. America, East Coast
                                11, # Central America, East Coast
                                12, # Cuba
                                13,
                                14,
                                15, # Vancouver Island
                                16, # Andros Island, Bahamas
                                17, # Long Island, New York
                                18, # Akimiski Island, James Bay, Canada
                                19, # Isla de la Juventud, Cuba
                                20, # Hispaniola
                                21, # South Andros Island, Bahamas
                                22, # Isla Tiburon, Mexico
                                23, # Great Abaco Island, Bahamas
                                24, # Little Abaco Island, Bahamas
                                25, # Isla Angel de la Guarda, Mexico
                                26, # Cayo Coco, Cuba
                                27, # Long Island, Bahamas
                                28, # Acklins Island, Bahamas
                                29, # Eleuthera Island, Bahamas
                                30, # Whidbey Island, Washington (I think)
                                31,
                                32, # Isla Cedros, Mexico
                                33, # Cat Island, Bahamas
                                34, # Cayo Santa Maria, Cuba
                                35,
                                36,
                                37,
                                38,
                                39,
                                40, # Mayaguana, Bahamas
                                41,
                                42,
                                43,
                                44,
                                45,
                                46,
                                47,
                                48,
                                49,
                                50,
                                51,
                                52,
                                53,
                                54,
                                55,
                                56,
                                57,
                                58,
                                59,
                                60,
                                61,
                                62,
                                63,
                                64,
                                65,
                                66,
                                67,
                                68,
                                69,
                                70,
                                71,
                                72,
                                73,
                                74,
                                75,
                                76,
                                77,
                                78,
                                79,
                                80,
                                81,
                                82,
                                83,
                                84,
                                85,
                                86,
                                87,
                                88,
                                89,
                                90,
                                91,
                                92,
                                93,
                                94,
                                95, # Lake Erie
                                96, # Lake Winnipeg
                                97, # Lake Ontario
                                98, # Michikamau Lake, Canada
                                99,
                                100,
                                101, # Lake Winnipegosis
                                102,] # Lake Manitoba
            #                     103, # Lake Nipigon
            #                     104, # Lake of the Woods
            # ]
        else:
            print('ERROR: no support for projection "{}".'.
                  format(crs_proj_name),
                  file=sys.stderr)
            return None, None

    # print('####')
    # print(ax.dataLim)
    # print(ax.viewLim)
    # print('####')

    # Extract the requested raster from the dataset.
    grid = dataset.GetRasterBand(raster_band_index).ReadAsArray()
    # print('corner values (unmasked): {}, {}, {}, {}'.
    #       format(grid[0,0],
    #              grid[0,-1],
    #              grid[-1,0],
    #              grid[-1,-1]))
    # Convert the grid to a masked array.
    grid_masked = np.ma.masked_equal(grid, ndv)
    # print(grid_masked.data[0,0])
    # grid_masked.data[grid_masked.mask == True] = np.nan
    # print(grid_masked.data[0,0])
    # print('corner values (masked): {}, {}, {}, {}'.
    #       format(grid_masked.data[0,0],
    #              grid_masked.data[0,-1],
    #              grid_masked.data[-1,0],
    #              grid_masked.data[-1,-1]))
    # print(grid_masked[0,0])
    # print(grid_masked[0,-1])
    # print(grid_masked[-1,0])
    # print(grid_masked[-1,-1])
    # if len(grid_masked.mask.shape) > 0:
    #     print('corner mask values: {}, {}, {}, {}'.
    #           format(grid_masked.mask[0,0],
    #                  grid_masked.mask[0,-1],
    #                  grid_masked.mask[-1,0],
    #                  grid_masked.mask[-1,-1]))
    #ax.set_facecolor('yellow')
    #fig.set_facecolor('red')

    # print(grid.shape)
    # print(len(x_ctr_data))
    # print(len(y_ctr_data))
    # print(figure_x_size)
    # print(figure_y_size)
    
    # Draw the grid.
    #ax.set_facecolor('black')
    #ax.set_facecolor('black')

    # cf = ax.contourf(x_ctr_data, y_ctr_data,
    #                  np.flipud(grid_masked),
    #                  color_ramp['col_levels'],
    #                  cmap=color_ramp['colormap'],
    #                  norm=color_ramp['norm'],
    #                  extend=color_ramp['extend'],
    #                  transform=crs)
    # print(bbox[0],x_ctr_data[0],bbox[1],x_ctr_data[-1])
    # print(bbox[2],y_ctr_data[0],bbox[3],y_ctr_data[-1])
    cf = mplplt.contourf(x_ctr_data, y_ctr_data,
                         np.flipud(grid_masked),
                         color_ramp['col_levels'],
                         cmap=color_ramp['colormap'],
                         norm=color_ramp['norm'],
                         extend=color_ramp['extend'],
                         transform=crs)

    ax.set_title(title)

    if cbar_geom is not None:
        cbar_axes = fig.add_axes(cbar_geom) # left, bottom, width, height
        cbar = fig.colorbar(cf,
                            cax=cbar_axes,
                            orientation='vertical',
                            pad=0.05,
                            shrink=0.7)
    else:
        cbar = fig.colorbar(cf,
                            orientation='vertical',
                            fraction=0.04,
                            pad=0.05,
                            shrink=0.7)

    cbar.set_ticks(color_ramp['tick_levels'])
    cbar.set_ticklabels(color_ramp['tick_labels'])
    cbar.ax.yaxis.set_tick_params(length=4)
    cbar.ax.yaxis.set_tick_params(labelsize=cbar_labelsize)
    cbar.set_label(color_ramp_label,
                   fontsize=cbar_fontsize,
                   color=mpl.rcParams['ytick.color'])

    # print('####')
    # print(ax.dataLim)
    # print(ax.viewLim)
    # print('####')

    # Draw U.S. states and national boundaries.
    if not use_basemap:
        ax.add_feature(cfNEF(category='cultural',
                             name='admin_1_states_provinces_lakes',
                             scale='50m',
                             edgecolor='gray',
                             facecolor='none',
                             linewidth=0.4))
        ax.add_feature(cfNEF(category='cultural',
                             name='admin_0_countries_lakes',
                             scale='50m',
                             edgecolor='black',
                             facecolor='none',
                             linewidth=0.4))
    else:
        # xbound = ax.get_xbound()
        # ybound = ax.get_ybound()
        
        coasts = map.drawcoastlines(linewidth=0)
        # print(type(coasts))
        # print(dir(coasts))
        # print(coasts.get_label())
        coasts_paths = coasts.get_paths()

        # for ipoly in range(len(coasts_paths)):        
        for ind, ipoly in enumerate(polygons_to_show):
            # if ipoly < poly_start: continue
            # if ipoly > poly_stop: break
            r = coasts_paths[ipoly]
            polygon_vertices = [(vertex[0],vertex[1])
                                for (vertex,code)
                                in r.iter_segments(simplify=False)]
            px = [polygon_vertices[i][0] for i in range(len(polygon_vertices))]
            py = [polygon_vertices[i][1] for i in range(len(polygon_vertices))]
            map.plot(px,py,'k-', linewidth=0.4)
            # if ind < len(polygons_to_show) - 1:
            # else:
            #     map.plot(px,py,'k-', linewidth=0.4, color='g')
            # Uncomment below to draw start/stop polygons with fat colored
            # lines to make them easy to spot when setting hard-coded
            # poly_start and poly_stop values.
            # if ipoly == poly_start:
            #     map.plot(px,py,'k-', linewidth=1.0, color='g')
            # if ipoly == poly_stop:
            #     map.plot(px,py,'k-', linewidth=1.0, color='r')
                
        map.drawcountries(linewidth=0.4)
        map.drawstates(linewidth=0.4)

        # print(ax.viewLim)
        #ax.set_xbound(xbound)
        #ax.set_ybound(ybound)
        # print(ax.viewLim)
        #ax.set_extent(bbox, crs=crs)

    # print('####')
    # print(ax.dataLim)
    # print(ax.viewLim)
    # print(ax.get_xbound())
    # print('####')

    #pprint(vars(ax))
    #print(bbox)


    return(None, ax)





def read_sfav2_daily(snowfall_dir,
                     scratch_dir,
                     date_yyyymmddhh):
    """
    Read daily National Snowfall Analysis results.
    """

    # Verify input directory exists.
    if not os.path.isdir(snowfall_dir):
        raise FileNotFoundError(errno.ENOENT,
                                os.strerror(errno.ENOENT),
                                archive_dir)
        return None

    # Verify scratch directory exists.
    if not os.path.isdir(scratch_dir):
        raise FileNotFoundError(errno.ENOENT,
                                os.strerror(errno.ENOENT),
                                scratch_dir)
        return None

    file_dir = os.path.join(snowfall_dir,
                            'sfav2_{}'.format(date_yyyymmddhh[0:8]))

    # Verify file directory exists.
    if not os.path.isdir(file_dir):
        raise FileNotFoundError(errno.ENOENT,
                                os.strerror(errno.ENOENT),
                                file_dir)
        return None

    tiff_file = 'sfav2_CONUS_24h_{}.tif'.format(date_yyyymmddhh)
    tiff_path = os.path.join(file_dir,tiff_file)
    if not os.path.exists(tiff_path):
        print('No data found for {}'.format(date_yyyymmddhh))
        return None

    ds = gdal.Open(tiff_path)

    return ds


def read_nsidc_arch_snow(archive_dir,
                         scratch_dir,
                         date_yyyymmdd,
                         product_group=1034,
                         unmasked=False):
    """
    Read snow depth (in mm) from a local copy of the NSIDC SNODAS
    archives.
    """

    # Verify input directory exists.
    if not os.path.isdir(archive_dir):
        raise FileNotFoundError(errno.ENOENT,
                                os.strerror(errno.ENOENT),
                                archive_dir)
        return None, None

    # Verify scratch directory exists.
    if not os.path.isdir(scratch_dir):
        raise FileNotFoundError(errno.ENOENT,
                                os.strerror(errno.ENOENT),
                                scratch_dir)
        return None, None

    if not unmasked:
        domain = 'masked'
        domain_file = 'us'
    else:
        domain = 'unmasked'
        domain_file = 'zz'

    #DEPTH
    #product_group = 1036 # snow depth

    file_dir = os.path.join(archive_dir,
                            domain,
                            '{}'.format(product_group),
                            date_yyyymmdd[0:4],
                            date_yyyymmdd[4:6])

    # Verify file directory exists.
    if not os.path.isdir(file_dir):
        raise FileNotFoundError(errno.ENOENT,
                                os.strerror(errno.ENOENT),
                                file_dir)
        return None, None

    if int(date_yyyymmdd) < 20161001:
        hdr_ext = 'Hdr'
    else:
        hdr_ext = 'txt'

    hdr_file = '{}_ssmv1{}tS__T0001TTNATS{}05HP001.{}.gz'. \
               format(domain_file,
                      product_group,
                      date_yyyymmdd,
                      hdr_ext)

    if not os.path.exists(os.path.join(file_dir,hdr_file)):
        print('No header found for {}'.format(date_yyyymmdd))
        return None, None

    # Read the GISRS raster header. ALL fields are returned as strings.
    gisrs_hdr = {}
    with gzip.open(os.path.join(file_dir,hdr_file), 
                   mode='rt') as nsidc_hdr:
        for line in nsidc_hdr:
            gisrs_hdr[line.split(':')[0]] = \
                line.split(':')[1].rstrip('\n').strip()
        #     print('"{}" = "{}"'.
        #           format(line.split(':')[0],
        #                  gisrs_hdr[line.split(':')[0]]))
        # data_units = gisrs_hdr['Data units']
        # data_slope = gisrs_hdr['Data slope']
        # data_intercept = gisrs_hdr['Data intercept']

    # Data units are always in mm.
    if not gisrs_hdr['Data units'].startswith('Meters / 1000.000'):
        print('Unsupported units "{}" in {}.'.
              format(gisrs_hdr['Data units'],
                     os.path.join(file_dir,hdr_file)),
              file=sys.stderr)
        return None, None

    if gisrs_hdr['Data type'] != 'integer':
        print('Unsupported data type "{}" in {}.'.
              format(gisrs_hdr['Data type'],
                     os.path.join(file_dir,hdr_file)),
              file=sys.stderr)
        return None, None

    if gisrs_hdr['Data bytes per pixel'] != '2':
        print('Unsupported bytes per pixel of "{}" in {}.'.
              format(gisrs_hdr['Data bytes per pixel'],
                     os.path.join(file_dir,hdr_file)),
              file=sys.stderr)
        return None, None

    dat_file = '{}_ssmv1{}tS__T0001TTNATS{}05HP001.dat.gz'. \
               format(domain_file,
                      product_group,
                      date_yyyymmdd)

    if not os.path.exists(os.path.join(file_dir,dat_file)):
        print('No data file found for {}'.format(date_yyyymmdd))
        return None, None

    # To read the binary grid we need to use the NumPy frombuffer
    # method, and must remember to flip the bytes on little endian
    # systems (such as Linux) because data are stored as big endian in
    # the SNODAS archives.
    with gzip.open(os.path.join(file_dir,dat_file),
                  mode='rb') as dat_file:
        dt = np.dtype('int16')
        # Data are stored as big endian in SNODAS archives. Linux is
        # little endian, so generally those bytes need to get swapped.
        if sys.byteorder == 'little':
            dt = dt.newbyteorder('>')
        grid = np.frombuffer(dat_file.read(), dtype=dt)
        grid = grid.reshape(int(gisrs_hdr['Number of rows']),
                            int(gisrs_hdr['Number of columns']))

    return gisrs_hdr, grid


def parse_args():

    """
    Parse command line arguments.
    """

    help_message = 'Generate a climatology of the National Snowfall Analysis.'

    parser = argparse.ArgumentParser(description=help_message)

    parser.add_argument('-s', '--start_year',
                        type=int,
                        metavar='start water year',
                        nargs='?')
    parser.add_argument('-f', '--finish_year',
                        type=int,
                        metavar='finish water year',
                        nargs='?')
    parser.add_argument('-p', '--plot_results',
                        action='store_true',
                        help='Display plot of climatology for each day.')
    args = parser.parse_args()

    if not args.start_year:
        args.start_year = 2009
        print('No start year given. Using default of {}.'.
              format(args.start_year))
    if not args.finish_year:
        args.finish_year = 2019
        print('No finish year given. Using default of {}.'.
              format(args.finish_year))
    return args


def main():
    """
    Using daily 12Z results from the National Snowfall Analysis, Version 2,
    generate gridded "period of record" (POR) data, including different
    quantiles and IQR.
    """

    opt = parse_args()

    if opt.plot_results:
        # Prepare for plotting.
        mplplt.close('all')
        GF_rcParams()

    snowfall_dir = '/operations/misc/snowfall_v2'
    scratch_dir = '/net/scratch/{}'.format(os.getlogin())

    # opt.start_year and opt.finish_year are the END of the water
    # years. For example, if  opt.start_year = 2005, then the first year
    # of the climatology covers October 2004 - September 2005.
    clim_num_years = opt.finish_year - opt.start_year + 1

    repair_ds = None
    ndv_out = None

    # Generate xxSNODASxx climatology for a hypothetical leap year.
    for day_of_water_year in range(1, 367):
        # Generate climatology for current day_of_water_year.
        print('Day of water year {}.'.format(day_of_water_year))
        # Skip ahead to December 18.
        # if day_of_water_year < 79:
        # Skip ahead to June 11.
        # if day_of_water_year < 255:
        #   continue
        layers = []
        layers_nonzero = []

        # if day_of_water_year < 100:
        #     continue

        # Looping backward means that later years will establish
        # coordinates for the climatology. We do not want the outputs to
        # be anchored to the pre-shift (which occurred on 2016-10-01?)
        # coordinates.
        date_mmdd = None
        lon_lat_ds = None
        for year in range(opt.finish_year, opt.start_year - 1, -1):
            start_of_water_year = '{}1001'.format(year-1)
            start_of_water_year_datetime = \
              dt.datetime.strptime(start_of_water_year, '%Y%m%d')

            # Calculate the datetime associated with the current
            # day_of_water_year.
            if day_of_water_year < 152:
                # For dates up to and including Feburary 28, calculating
                # date_mmdd is simple.
                dowy_datetime = start_of_water_year_datetime + \
                                dt.timedelta(days=day_of_water_year-1)
                if date_mmdd is None:
                    date_mmdd = dowy_datetime.strftime('%m%d')
            else:
                if calendar.isleap(year):
                    # Calculating date_mmdd is unchanged for leap years.
                    dowy_datetime = start_of_water_year_datetime + \
                                    dt.timedelta(days=day_of_water_year-1)
                    if date_mmdd is None:
                        date_mmdd = dowy_datetime.strftime('%m%d')
                else:
                    # Subtract an extra day for non-leap years, which
                    # means that February 28 stands in for leap day when
                    # day_of_water_year = 152 (out of 366) and year is
                    # not a leap year.
                    dowy_datetime = start_of_water_year_datetime + \
                                    dt.timedelta(days=day_of_water_year-2)

            print('Reading data for {}.'.
                  format(dowy_datetime.strftime('%Y%m%d')))

            if (day_of_water_year == 152) and (not calendar.isleap(year)):
                dowy_datetime = start_of_water_year_datetime + \
                                dt.timedelta(days=day_of_water_year-1)
                print('  leap day in non-leap year: read data for {}.'.
                      format(dowy_datetime.strftime('%Y%m%d')))

            ds = read_sfav2_daily(snowfall_dir,
                                  scratch_dir,
                                  dowy_datetime.strftime('%Y%m%d' + '12'))
            if ds is None:
                continue
            band = ds.GetRasterBand(1)
            grid = band.ReadAsArray()
            ndv = band.GetNoDataValue()
            if ndv_out is None:
                ndv_out = ndv

            this_num_rows = grid.shape[0]
            this_num_cols = grid.shape[1]
            this_min_lon = ds.GetGeoTransform()[0]
            this_lon_res = ds.GetGeoTransform()[1]
            this_max_lon = this_min_lon + this_num_cols * this_lon_res
            this_max_lat = ds.GetGeoTransform()[3]
            this_lat_res = ds.GetGeoTransform()[5]
            this_min_lat = this_max_lat + this_num_rows * this_lat_res

            # Convert the grid to a masked array.
            grid = np.ma.masked_equal(grid, ndv)

            # If this is the first grid read for this date, define the grid
            # geometry, both "out" (output) and "ref" (reference).
            if len(layers) == 0:
                num_rows_out = this_num_rows
                num_cols_out = this_num_cols
                min_lon_out = this_min_lon
                max_lon_out = this_max_lon
                min_lat_out = this_min_lat
                max_lat_out = this_max_lat
                lon_res_out = this_lon_res
                lat_res_out = this_lat_res
                num_rows_ref = this_num_rows
                num_cols_ref = this_num_cols
                min_lon_ref = this_min_lon
                max_lon_ref = this_max_lon
                min_lat_ref = this_min_lat
                max_lat_ref = this_max_lat
                lon_res_ref = this_lon_res
                lat_res_ref = this_lat_res

            # Verify grid shape matches grid geometry definition from the
            # raster header. Assume that if grid.shape is correct, then
            # we can use the other geometry parameters without a problem.
            if this_num_rows != grid.shape[0]:
                print('ERROR: grid # rows mismatch in snowfall grid for ' +
                      dowy_datetime.strftime('%Y-%m-%d') + '.',
                      file=sys.stderr)
                sys.exit(1)
            if this_num_cols != grid.shape[1]:
                print('ERROR: grid # columns mismatch in snowfall grid for ' +
                      dowy_datetime.strftime('%Y-%m-%d') + '.',
                      file=sys.stderr)
                sys.exit(1)

            # Verify grid shape against output geometry.
            if grid.shape[0] != num_rows_out:
                print('ERROR: grid # rows inconsistency in ' +
                      'snowfall grid for ' +
                      dowy_datetime.strftime('%Y-%m-%d') + '.',
                      file=sys.stderr)
                sys.exit(1)
            if grid.shape[1] != num_cols_out:
                print('ERROR: grid # columns inconsistency in ' +
                      'snowfall grid for ' +
                      dowy_datetime.strftime('%Y-%m-%d') + '.',
                      file=sys.stderr)
                sys.exit(1)

            # Make sure grid geometry does not differ significantly from
            # output geometry. We will tolerate differences of up to
            # 0.001 degrees, which is 3.6 arc sec--around 100
            # meters. This exercise is purely academic since no such
            # shift has ever happened, but it pays to be careful.
            shift = max(abs(this_min_lon - min_lon_out),
                        abs(this_max_lon - max_lon_out),
                        abs(this_min_lat - min_lat_out),
                        abs(this_max_lat - max_lat_out))
            if shift > 0.001:
                print('ERROR: unacceptably large coordinate shift at {}.'.
                      format(dowy_datetime.strftime('%Y%m%d')),
                      file=sys.stderr)
                sys.exit(1)

            # Give a notice if there is any significant change in
            # geometry. Since we are converting strings that were
            # generated from floats back into floats--and also because we
            # performed an intentional shift of the SNODAS grid in
            # 2012--this is expected, and not a problem, but is worth
            # noting when it occurs. The threshold for this check is
            # 1.0e-5 degrees--about 1 meter.
            shift = max(abs(this_min_lon - min_lon_ref),
                        abs(this_max_lon - max_lon_ref),
                        abs(this_min_lat - min_lat_ref),
                        abs(this_max_lat - max_lat_ref))
            if shift > 1.0e-5:
                print('NOTICE: minor coordinate shift at {}.'.
                      format(dowy_datetime.strftime('%Y%m%d')))
                num_rows_ref = this_num_rows
                num_cols_ref = this_num_cols
                min_lon_ref = this_min_lon
                max_lon_ref = this_max_lon
                min_lat_ref = this_min_lat
                max_lat_ref = this_max_lat
                lon_res_ref = this_lon_res
                lat_res_ref = this_lat_res

            if lon_lat_ds is None:

                # Generate a general purpose GDAL dataset for generating
                # graphics (including output GeoTIFF file).

                mem_driver = gdal.GetDriverByName('MEM')
                np_dtype_name = grid.dtype.name
                gdal_data_type_num = gdal.GetDataTypeByName(np_dtype_name)
                gdal_data_type_name = gdal.GetDataTypeName(gdal_data_type_num)

                # Create a generic dataset that any grid for this
                # climatology can be dropped into.
                lon_lat_ds = mem_driver.Create('sfav2 period of record',
                                               xsize=grid.shape[1],
                                               ysize=grid.shape[0],
                                               bands=1,
                                               eType=eval('gdal.GDT_' +
                                                          gdal_data_type_name))
                # Define the "projection".
                srs = osr.SpatialReference()
                srs.ImportFromEPSG(4326)
                lon_lat_ds.SetProjection(srs.ExportToWkt())

                print('lat_res_out {}'.format(lat_res_out))
                # Define the GeoTransform.
                geo_transform = (min_lon_out, lon_res_out, 0.0,
                                 max_lat_out, 0.0, lat_res_out)
                print('geo_transform :{}'.format(geo_transform))
                lon_lat_ds.SetGeoTransform(geo_transform)

                # Write grid to GDAL dataset.
                grid_display = np.ma.copy(grid)

                lon_lat_ds.GetRasterBand(1).WriteArray(grid_display)

                # Define variables needed for plotting.
                lon_lat_crs = ccrs.PlateCarree()

                bbox = [min_lon_out, max_lon_out, min_lat_out, max_lat_out]
                aspect = (max_lon_out - min_lon_out) / \
                         (max_lat_out - min_lat_out)
                min_lon_ctr_out = min_lon_out + 0.5 * lon_res_out
                max_lon_ctr_out = max_lon_out - 0.5 * lon_res_out
                min_lat_ctr_out = min_lat_out + 0.5 * lat_res_out
                max_lat_ctr_out = max_lat_out - 0.5 * lat_res_out
                xsize = 12.0
                ysize = math.ceil(2.0 * xsize / aspect) / 2.0
                lon_ctr_out = np.linspace(min_lon_out + 0.5 * lon_res_out,
                                          max_lon_out - 0.5 * lon_res_out,
                                          num_cols_out)
                lat_ctr_out = np.linspace(min_lat_out + 0.5 * lat_res_out,
                                          max_lat_out - 0.5 * lat_res_out,
                                          num_rows_out)


            # Add the current grid to the layers list, for stacking to
            # come afterward.
            layers.append(grid)
            layers_nonzero.append(grid > 0)

        if date_mmdd is None:
            print('ERROR: data did not include a leap year; ' +
                  'check programming.',
                  file=sys.stderr)
            exit(1)
        if lon_lat_ds is None:
            print('ERROR: no dataset created; check programming',
                  file=sys.stderr)
            exit(1)

        # Convert layers list into a stack.
        print('Stacking grids.')
        t1 = dt.datetime.utcnow()
        layers = np.ma.stack(layers, axis=0)
        layers_nonzero = np.ma.stack(layers_nonzero, axis=0)
        t2 = dt.datetime.utcnow()
        elapsed_time = t2 - t1
        print('elapsed: {} seconds'.format(elapsed_time.total_seconds()))

        # Calculate the number of years of good data and the number of years
        # of nonzero data (these are not the same thing) for each grid cell.
        num_years_non_ndv = layers.count(axis=0)
        num_years_nonzero = layers_nonzero.sum(axis=0).astype('int16')

        sample_row = 400
        sample_col = 700
        print(layers[:,sample_row,sample_col])
        print(num_years_non_ndv[sample_row, sample_col])
        print(layers_nonzero[:,sample_row,sample_col])
        print(num_years_nonzero[sample_row, sample_col])
        print(num_years_nonzero.dtype)

        print('Computing maximum.')
        t1 = dt.datetime.utcnow()
        sfav2_max = np.ma.max(layers, axis=0, fill_value=ndv)
        # The fill_value above does not work as expected (setting masked
        # values to ndv) so manually set values of masked cells to ndv.
        sfav2_max.data[sfav2_max.mask == True] = ndv
        t2 = dt.datetime.utcnow()
        elapsed_time = t2 - t1
        print('elapsed: {} seconds'.format(elapsed_time.total_seconds()))

        print('Computing mean.')
        t1 = dt.datetime.utcnow()
        print('layers: {}'.format(layers.shape))
        sfav2_mean = np.ma.mean(layers, axis=0)
        ind = np.where(num_years_non_ndv == 0)
        sfav2_mean[ind] = ndv
        sfav2_mean.mask[ind] = True

        num_years_nonzero[ind] = -1
        num_years_nonzero.mask[ind] = True

        # Identify cells that are imperfect but have enough data to
        # calculate a result.
        min_years_for_clim = math.ceil(clim_num_years / 2)
        ind = np.where((num_years_non_ndv >= min_years_for_clim) &
                       (num_years_non_ndv < clim_num_years))
        print('There are {} "imperfect" pixels, '.format(len(ind[0])) +
              'with {}-{} '.format(min_years_for_clim, clim_num_years - 1) +
              'years of data.')

        # Mask cells where we have data for less than half the years of
        # the climatology. For odd years use the ceiling.
        ind = np.where((num_years_non_ndv > 0) & 
                       (num_years_non_ndv < min_years_for_clim))

        sfav2_max[ind] = ndv
        sfav2_max.mask[ind] = True
        sfav2_mean[ind] = ndv
        sfav2_mean.mask[ind] = True

        layers = None
        layers_nonzero = None

        # Create a string for the year range.
        if day_of_water_year < 93:
            year_range = '{}-{}'.format(opt.start_year-1, opt.finish_year-1)
            year_range_file = '{}_to_{}'. \
                format(opt.start_year-1, opt.finish_year-1)
        else:
            year_range = '{}-{}'.format(opt.start_year, opt.finish_year)
            year_range_file = '{}_to_{}'. \
                format(opt.start_year, opt.finish_year)

        # --------------------------------------------------------------- 
        # Write the mean grid to the generic GDAL dataset lon_lat_ds.
        # --------------------------------------------------------------- 
        # Write the mean to a GeoTIFF. See
        # https://gdal.org/drivers/raster/gtiff.html
        lon_lat_ds.GetRasterBand(1).WriteArray(sfav2_mean)
        # Even though ndv is a 32-bit float, it is a numpy type, and for
        # an unknown reason it has to be cast to a regular Python float
        # for SetNoDataValue to accept it without errors.
        lon_lat_ds.GetRasterBand(1).SetNoDataValue(float(ndv))
        desc = 'Mean snowfall ' + \
               '(inches) for {} '. \
               format(calendar.month_name[int(date_mmdd[0:2])]) + \
               '{}, '.format(int(date_mmdd[2:])) + \
               year_range
        lon_lat_ds.GetRasterBand(1).SetDescription(desc)
        tiff_name = 'sfav2_por_' + \
                    year_range_file + \
                    'mean_{}.tif'.format(date_mmdd)
        print('Creating GeoTIFF "{}".'.format(tiff_name))
        tiff_driver = gdal.GetDriverByName('GTiff')
        tiff_driver.CreateCopy(tiff_name,
                               lon_lat_ds,
                               False,
                               options=["COMPRESS=LZW"])

        if opt.plot_results:

            x_size = 16.0 * 0.75
            y_size = 9.0 * 0.75
            cbar_geom = [0.85, 0.1, 0.02, 0.4]

            # # Pixel sizes of bottom margin, map, and top margin.
            # vertical_px = [50, 600, 100]
            # ratio_v = np.float(num_rows_out) / vertical_px[1]
            # lower_bound_norm = vertical_px[0] / np.sum(vertical_px)
            # upper_bound_norm = np.sum(vertical_px[:2]) / \
            #                    np.sum(vertical_px)
            # y_size_px = np.sum(vertical_px) * ratio_v
            # mpl.rcParams['figure.subplot.bottom'] = lower_bound_norm
            # mpl.rcParams['figure.subplot.top'] = upper_bound_norm
            # dpi = y_size_px / y_size

            # Define the color ramp.
            snowfall_color_ramp = daily_snowfall_colors()

            # Generate the figure.
            title = 'Mean Snowfall ({}) '.format(year_range) + \
                    'for ' + \
                    dowy_datetime.strftime('%m-%d')

            fig, ax = geo_grid_map(lon_lat_crs,
                                   xsize,
                                   ysize,
                                   1,
                                   bbox,
                                   lon_ctr_out,
                                   lat_ctr_out,
                                   lon_lat_ds,
                                   1,
                                   ndv,
                                   title,
                                   snowfall_color_ramp,
                                   'inches',
                                   cbar_geom,
                                   use_basemap=False)
            mplplt.show()

        # --------------------------------------------------------------- 
        # Write the maximum to a GeoTIFF.
        lon_lat_ds.GetRasterBand(1).WriteArray(sfav2_max)
        lon_lat_ds.GetRasterBand(1).SetNoDataValue(float(ndv))
        desc = 'Maximum snowfall ' + \
               '(inches) for {} '. \
               format(calendar.month_name[int(date_mmdd[0:2])]) + \
               '{}, '.format(int(date_mmdd[2:])) + \
               year_range
        lon_lat_ds.GetRasterBand(1).SetDescription(desc)

        # Write the maximum to a GeoTIFF. See
        # https://gdal.org/drivers/raster/gtiff.html
        tiff_name = 'sfav2_por_' + \
                    year_range_file + \
                    'max_{}.tif'.format(date_mmdd)
        print('Creating GeoTIFF "{}".'.format(tiff_name))
        tiff_driver.CreateCopy(tiff_name,
                               lon_lat_ds,
                               False,
                               options=["COMPRESS=LZW"])

        if opt.plot_results:

            # Define the color ramp.
            snowfall_color_ramp = daily_snowfall_colors()

            # Generate the figure.
            title = 'Maximum Snowfall ({}) '.format(year_range) + \
                    'for ' + \
                    dowy_datetime.strftime('%m-%d')
            fig, ax = geo_grid_map(lon_lat_crs,
                                   xsize,
                                   ysize,
                                   1,
                                   bbox,
                                   lon_ctr_out,
                                   lat_ctr_out,
                                   lon_lat_ds,
                                   1,
                                   ndv,
                                   title,
                                   snowfall_color_ramp,
                                   'inches',
                                   cbar_geom,
                                   use_basemap=False)
            mplplt.show()



        # --------------------------------------------------------------- 
        # Write num_years_nonzero to a GeoTIFF.
        lon_lat_ds.GetRasterBand(1).WriteArray(num_years_nonzero)
        lon_lat_ds.GetRasterBand(1).SetNoDataValue(-1)
        desc = 'Number of years with nonzero snowfall ' + \
               'for {} '. \
               format(calendar.month_name[int(date_mmdd[0:2])]) + \
               '{}, '.format(int(date_mmdd[2:])) + \
               year_range
        print(desc)
        lon_lat_ds.GetRasterBand(1).SetDescription(desc)
        tiff_name = 'sfav2_por_' + \
                    year_range_file + \
                    'num_nonzero_{}.tif'.format(date_mmdd)
        print('Creating GeoTIFF "{}".'.format(tiff_name))
        tiff_driver = gdal.GetDriverByName('GTiff')
        tiff_driver.CreateCopy(tiff_name,
                               lon_lat_ds,
                               False,
                               options=["COMPRESS=LZW"])

        if opt.plot_results:

            # Define the color ramp.
            num_years_color_ramp = zero_to_eleven_colors()

            # Generate the figure.
            title = '# Years with Snowfall > 0 ({}) '.format(year_range) + \
                    'for ' + \
                    dowy_datetime.strftime('%m-%d')
            fig, ax = geo_grid_map(lon_lat_crs,
                                   xsize,
                                   ysize,
                                   1,
                                   bbox,
                                   lon_ctr_out,
                                   lat_ctr_out,
                                   lon_lat_ds,
                                   1,
                                   -1,
                                   title,
                                   num_years_color_ramp,
                                   'Years',
                                   cbar_geom,
                                   use_basemap=False)

            mplplt.show()


if __name__ == '__main__':
    main()
