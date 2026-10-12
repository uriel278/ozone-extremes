import xarray as xr
import pandas as pd
import geopandas as gpd
import numpy as np
from shapely.geometry import box
import os
def export_grid_to_shapefile(dataset, output_path, crs='EPSG:4326'):
    """
    Export the grid of an xarray Dataset to a polygon Shapefile.
    
    Parameters:
    -----------
    dataset : xarray.Dataset
        Your loaded satellite data
    output_path : str
        Path for the output shapefile (e.g., 'grid_cells.shp')
    crs : str or dict
        Coordinate Reference System (default: WGS84)
    """
    
    # Ensure we have the spatial coordinates
    if 'lon' in dataset.dims and 'lat' in dataset.dims:
        lon_coord = 'lon'
        lat_coord = 'lat'
    elif 'longitude' in dataset.dims and 'latitude' in dataset.dims:
        lon_coord = 'longitude'
        lat_coord = 'latitude'
    elif 'x' in dataset.dims and 'y' in dataset.dims:
        lon_coord = 'x'
        lat_coord = 'y'
    else:
        raise ValueError("Could not identify longitude/latitude coordinates")
    
    # Get coordinate values
    lons = dataset[lon_coord].values
    lats = dataset[lat_coord].values
    
    # Calculate cell boundaries (assuming regular grid)
    # For center coordinates, calculate edges
    if len(lons) > 1:
        lon_edges = np.zeros(len(lons) + 1)
        lon_edges[1:-1] = (lons[:-1] + lons[1:]) / 2
        lon_edges[0] = lons[0] - (lons[1] - lons[0]) / 2
        lon_edges[-1] = lons[-1] + (lons[-1] - lons[-2]) / 2
    else:
        # If only one point, create a small buffer
        lon_edges = [lons[0] - 0.5, lons[0] + 0.5]
    
    if len(lats) > 1:
        lat_edges = np.zeros(len(lats) + 1)
        lat_edges[1:-1] = (lats[:-1] + lats[1:]) / 2
        lat_edges[0] = lats[0] - (lats[1] - lats[0]) / 2
        lat_edges[-1] = lats[-1] + (lats[-1] - lats[-2]) / 2
    else:
        lat_edges = [lats[0] - 0.5, lats[0] + 0.5]
    
    # Create polygons for each grid cell
    polygons = []
    cell_attributes = []
    
    for i in range(len(lons)):
        for j in range(len(lats)):
            # Create bounding box for the cell
            lon_min = lon_edges[i]
            lon_max = lon_edges[i+1]
            lat_min = lat_edges[j]
            lat_max = lat_edges[j+1]
            
            # Create polygon
            polygon = box(lon_min, lat_min, lon_max, lat_max)
            polygons.append(polygon)
            
            # Store cell information
            cell_attributes.append({
                'cell_id': f"{i}_{j}",
                'lon_center': lons[i],
                'lat_center': lats[j],
                'lon_index': i,
                'lat_index': j
            })
    
    # Create GeoDataFrame
    gdf = gpd.GeoDataFrame(cell_attributes, geometry=polygons, crs=crs)
    
    # Export to Shapefile
    gdf.to_file(output_path)
    print(f"Grid exported to {output_path} with {len(gdf)} cells")
    
    return gdf

# Usage example
# ds = xr.open_dataset('your_satellite_data.nc')
# grid_gdf = export_grid_to_shapefile(ds, 'satellite_grid.shp


ds = xr.open_dataset(os.path.join('data', 'ozone', '2013', 'cams.eaq.vra.ENSa.o3.l0.2013-01.nc'), engine='netcdf4')
d1 = ds.isel(time = 0)

os.makedirs(os.path.join(os.getcwd(), 'data', 'raw', 'satellite-grid-reanalysis-py'))

export_grid_to_shapefile(d1, 
                         os.path.join(os.getcwd(), 'data', 'raw', 'satellite-grid-reanalysis-py','satellite-grid-reanalysis-py.shp'))
