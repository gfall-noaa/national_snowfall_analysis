; Convert GeoTIFF images from the National Snowfall Analysis from
; PackBits to LZW compression.
;
; You can also do this with
;
;   gdal_translate -co "COMPRESS=lzw" \
;                  <input TIFF file> <output TIFF file>
;
; Will perform 

  sfav2_dir = '/operations/misc/snowfall_v2'
  output_dir = '/net/tmp/tiff_test'
  total_size_packbits = 0LL
  total_size_lzw = 0LL
  sample_size_packbits = 0LL
  sample_size_lzw = 0LL
  sample_size_gdal = 0LL
  daily_dir = FILE_SEARCH(sfav2_dir + $
                          '/sfav2_[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]', $
                          /TEST_DIRECTORY)
  for dc = 0, N_ELEMENTS(daily_dir) - 1 do begin
      tiff_list = FILE_SEARCH(daily_dir[dc] + '/*.tif')
      for fc = 0, N_ELEMENTS(tiff_list) - 1 do begin
          if_info = FILE_INFO(tiff_list[fc])
          total_size_packbits = total_size_packbits + if_info.size
          file_name = STRMID(tiff_list[fc], $
                             STRPOS(tiff_list[fc], '/', /REVERSE_SEARCH) + 1)
          if NOT(QUERY_TIFF(tiff_list[fc], ii_info)) then STOP
          if (ii_info.description ne 'IDL TIFF file') then STOP
          image = READ_TIFF(tiff_list[fc], $
                            GEOTIFF = geo_data)
          image_size = SIZE(image)
          if (image_size[3] ne 4) then STOP ; HAS TO BE FLOAT
          WRITE_TIFF, output_dir + '/' + file_name, $
                      image, $
                      GEOTIFF = geo_data, $
                      COMPRESSION = 1, $
                      /FLOAT
          of_info = FILE_INFO(output_dir + '/' + file_name)
          total_size_lzw = total_size_lzw + of_info.size
          if (RANDOMU(seed) gt 0.95) then begin
;             For a sample of the data, more fully verify GeoTIFF file
;             contents between original, IDL-converted LZW version,
;             and GDAL-converted LZW version, and compare total sizes
;             of those samples.
              sample_size_packbits = sample_size_packbits + if_info.size
              sample_size_lzw = sample_size_lzw + of_info.size
              if NOT(QUERY_TIFF(output_dir + '/' + file_name, oi_info)) $
                  then STOP
              gdal_file = output_dir + '/' + $
                          STRMID(file_name, 0, STRPOS(file_name, '.')) + $
                          '_gdal.tif'
              cmd = 'gdal_translate -q -co "COMPRESS=lzw" ' + $
                    tiff_list[fc] + ' ' + $
                    gdal_file
              SPAWN, cmd, EXIT_STATUS = status
              if (status ne 0) then STOP
              gf_info = FILE_INFO(gdal_file)
              sample_size_gdal = sample_size_gdal + gf_info.size
                            if NOT(QUERY_TIFF(gdal_file, gi_info, $
                                GEOTIFF = gdal_geo_data)) then STOP
              if (oi_info.channels ne ii_info.channels) then STOP
              if (oi_info.dimensions[0] ne ii_info.dimensions[0]) then STOP
              if (oi_info.dimensions[1] ne ii_info.dimensions[1]) then STOP
              if (oi_info.has_palette ne ii_info.has_palette) then STOP
              if (oi_info.image_index ne ii_info.image_index) then STOP
              if (oi_info.num_images ne ii_info.num_images)then STOP
              if (oi_info.pixel_type ne ii_info.pixel_type) then STOP
              if (oi_info.type ne ii_info.type) then STOP
              if (oi_info.bits_per_sample ne ii_info.bits_per_sample) then STOP
              if (oi_info.orientation ne ii_info.orientation) then STOP
              if (oi_info.planar_config ne ii_info.planar_config) then STOP
              if (oi_info.photometric ne ii_info.photometric) then STOP
              if (oi_info.position[0] ne ii_info.position[0]) then STOP
              if (oi_info.position[1] ne ii_info.position[1]) then STOP
              if (oi_info.resolution[0] ne ii_info.resolution[0]) then STOP
              if (oi_info.resolution[1] ne ii_info.resolution[1]) then STOP
              if (oi_info.units ne ii_info.units) then STOP
              if (oi_info.tile_size[0] ne ii_info.tile_size[0]) then STOP
              if (oi_info.tile_size[1] ne ii_info.tile_size[1]) then STOP
              if (oi_info.description ne ii_info.description) then STOP
              ; Do not compare document_name and date_time tags.

              if (gi_info.channels ne ii_info.channels) then STOP
              if (gi_info.dimensions[0] ne ii_info.dimensions[0]) then STOP
              if (gi_info.dimensions[1] ne ii_info.dimensions[1]) then STOP
              if (gi_info.has_palette ne ii_info.has_palette) then STOP
              if (gi_info.image_index ne ii_info.image_index) then STOP
              if (gi_info.num_images ne ii_info.num_images)then STOP
              if (gi_info.pixel_type ne ii_info.pixel_type) then STOP
              if (gi_info.type ne ii_info.type) then STOP
              if (gi_info.bits_per_sample ne ii_info.bits_per_sample) then STOP
              if (gi_info.orientation ne ii_info.orientation) then STOP
              if (gi_info.planar_config ne ii_info.planar_config) then STOP
              if (gi_info.photometric ne ii_info.photometric) then STOP
              if (gi_info.position[0] ne ii_info.position[0]) then STOP
              if (gi_info.position[1] ne ii_info.position[1]) then STOP
              if (gi_info.resolution[0] ne ii_info.resolution[0]) then STOP
              if (gi_info.resolution[1] ne ii_info.resolution[1]) then STOP
              if (gi_info.units ne ii_info.units) then STOP
              if (gi_info.tile_size[0] ne ii_info.tile_size[0]) then STOP
              if (gi_info.tile_size[1] ne ii_info.tile_size[1]) then STOP
              if (gi_info.description ne ii_info.description) then STOP
              ; Do not compare document_name and date_time tags.

              if (gdal_geo_data.modelPixelScaleTag[0] ne $
                  geo_data.modelPixelScaleTag[0]) then STOP
              if (gdal_geo_data.modelPixelScaleTag[1] ne $
                  geo_data.modelPixelScaleTag[1]) then STOP
              if (gdal_geo_data.modelPixelScaleTag[2] ne $
                  geo_data.modelPixelScaleTag[2]) then STOP
              if (gdal_geo_data.modelTiePointTag[0,0] ne $
                  geo_data.modelTiePointTag[0,0]) then STOP
              if (gdal_geo_data.modelTiePointTag[1,0] ne $
                  geo_data.modelTiePointTag[1,0]) then STOP
              if (gdal_geo_data.modelTiePointTag[2,0] ne $
                  geo_data.modelTiePointTag[2,0]) then STOP
              if (gdal_geo_data.modelTiePointTag[3,0] ne $
                  geo_data.modelTiePointTag[3,0]) then STOP
              if (gdal_geo_data.modelTiePointTag[4,0] ne $
                  geo_data.modelTiePointTag[4,0]) then STOP
              if (gdal_geo_data.modelTiePointTag[5,0] ne $
                  geo_data.modelTiePointTag[5,0]) then STOP
              if (gdal_geo_data.GTModelTypeGeoKey ne $
                  geo_data.GTModelTypeGeoKey) then STOP
              if (gdal_geo_data.GTRasterTypeGeoKey ne $
                  geo_data.GTRasterTypeGeoKey) then STOP
              if (gdal_geo_data.geographicTypeGeoKey ne $
                  geo_data.geographicTypeGeoKey) then STOP
              ;Original files do not have
              ;  GeoCitationGeoKey,
              ;  GeogSemiMajorAxisGeoKey, or
              ;  GeogInvFlatteningGeoKey
              ;if (gdal_geo_data.geogCitationGeoKey ne $
              ;    geo_data.geogCitationGeoKey) then STOP
              if (gdal_geo_data.geogAngularUnitsGeoKey ne $
                  geo_data.geogAngularUnitsGeoKey) then STOP
              ;if (gdal_geo_data.geogSemiMajorAxisGeoKey ne $
              ;    geo_data.geogSemiMajorAxisGeoKey) then STOP
              ;if (gdal_geo_data.geogInvFlatteningGeoKey ne $
              ;    geo_data.geogInvFlatteningGeoKey) then STOP

              g_image = READ_TIFF(gdal_file)
              g_image_size = SIZE(g_image)
              if (g_image_size[3] ne 4) then STOP
              if (MIN(g_image - image) ne 0.0) then STOP
              if (MAX(g_image - image) ne 0.0) then STOP

              PRINT, 'Sample size (original PackBits): ' + $
                     STRCRA(sample_size_packbits / 1024. / 1024.) + $
                     ' MB'
              PRINT, '          Sample size (IDL LZW): ' + $
                     STRCRA(sample_size_lzw / 1024. / 1024.) + $
                     ' MB'
              PRINT, '         Sample size (GDAL LZW): ' + $
                     STRCRA(sample_size_gdal / 1024. / 1024.) + $
                     ' MB'

          endif
          ;if fc eq 0 then print, if_info.size, of_info.size
          ; verify images are same
          ;print, fc, n_elements(tiff_list) - 1
      endfor
      PRINT, 'Total size (original PackBits): ' + $
             STRCRA(total_size_packbits / 1024. / 1024. / 1024.) + ' GB'
      PRINT, '          Total size (IDL LZW): ' + $
             STRCRA(total_size_lzw / 1024. / 1024. / 1024.) + ' GB'
  endfor
end
