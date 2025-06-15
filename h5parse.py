#!/usr/bin/env python3
"""Parser for hdf5-viewer.  Returns JSON objects for parsing in elisp."""

# Copyright (C) 2024-2025 Paul Minner, Peter Mao, Caltech

import json
import sys
import argparse
import numpy as np
import h5py
import warnings

def meta_dict(obj) -> dict:
    """Common function to collect metadata from HDF5 object."""
    if isinstance(obj, h5py.Group):
        meta = { 'type': 'group',
                 'name': obj.name,
                 # 'children': []
                }
    elif isinstance(obj, h5py.Dataset):
        shape = "scalar"
        datarange = ""
        dtype = "object" # h5py call non-numeric types object
        data  = obj[()]
        if hasattr(data, "dtype"):
            dtype = str(data.dtype)
        if (hasattr(data, "shape") and
            data.shape is not None and
            dtype != "object"):
            datavec = data.reshape(-1) # data as a vector
            if len(data.shape) > 0:
                shape = str(data.shape)
            if len(datavec) > 0: # Protect against empty datasets
                try: # calculate the data range
                    with warnings.catch_warnings():
                        warnings.simplefilter("ignore")
                        datamin = np.nanmin(datavec)
                        datamax = np.nanmax(datavec)
                    if np.isnan(datamin):
                        datarange = 'nan'
                    elif datamin == datamax:
                        datarange = f'{datamin:.4g}'
                    else:
                        datarange = f'{datamin:.3g}:{datamax:.3g}'
                except: # take the 1st value if it's something weird
                    datarange = str(datavec[0])
        meta =  {'type': 'dataset',
                 'name': obj.name,
                 'shape': shape,
                 'range': datarange,
                 'dtype': dtype}
    else:
        raise Exception(f"'{obj.name}' is not a dataset or group")
    return meta


class H5Instance:
    """Main class for parsing the HDF5 file."""
    def __init__(self, filename: str):
        self.instance =  h5py.File(filename)

    def get_fields(self, root: str) -> dict:
        """Get Groups and Datasets of the Group ROOT"""
        if not self.is_group(root)["return"]:
            raise Exception(f"'{root}' is not a group")
        obj = self.instance[root]
        fields = {}
        for cname, cobj in obj.items():
            if cobj is not None:
                fields[cname] = meta_dict(cobj)
            else:
                fields[cname] = {'type': 'other',
                                 'name': cname}
        return fields

    def preview_field(self, field: str) -> dict:
        """If FIELD is a Group, return its sub-groups and sub-datasets.
        If FIELD is a Dataset, return the data."""
        obj = self.instance[field]
        meta = meta_dict(obj)
        if isinstance(obj, h5py.Group):
            # Return fields in group
            meta['data'] = str(list(obj.keys()))
        else:
            # Return data in field
            meta['data'] =  str(obj[()])
        return meta

    def read_dataset(self, field: str) -> dict:
        """Return metadata and data of FIELD, only for Datasets."""
        obj = self.instance[field]
        if not isinstance(obj, h5py.Dataset):
            raise Exception("Argument to --read-dataset must be a Dataset.")
        meta = meta_dict(obj)
        np.set_printoptions(threshold=sys.maxsize, linewidth=sys.maxsize)
        meta['data'] = str(obj[()])
        return meta

    def is_group(self, field: str) -> dict:
        """True for Groups.  False otherwise."""
        true_or_false = False
        if self.is_field(field)["return"]:
            obj = self.instance[field]
            if isinstance(obj, h5py.Group):
                return {"return": True}
        return {"return": true_or_false}

    def is_field(self, field: str) -> dict:
        """True for Groups and Datasets. False otherwise, in particular for Attributes."""
        true_or_false = field in self.instance
        return {"return": true_or_false}

    def get_attrs(self, root: str) -> dict:
        """Return attributes of Group or Dataset"""
        if not self.is_field(root)["return"]:
            raise Exception("Argument to --get-attrs must be a Group or Dataset.")
        obj = self.instance[root]
        np.set_printoptions(linewidth=45)
        return {x[0]:str(x[1]) for x in obj.attrs.items()}

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('filepath'       , type=str, help='File to parse')
    parser.add_argument('--get-fields'   , type=str, help='Print fields within group')
    parser.add_argument('--get-attrs'    , type=str, help='Print attributes of parent to root')
    parser.add_argument('--preview-field', type=str, help='Print preview of requested field')
    parser.add_argument('--read-dataset' , type=str, help='Print dataset data')
    parser.add_argument('--is-group'     , type=str, help='Print true if field is group')
    parser.add_argument('--is-field'     , type=str, help='Print true if field exists in file')
    args = parser.parse_args()

    inst = H5Instance(args.filepath)

    if args.get_fields:
        print(json.dumps(inst.get_fields(args.get_fields), indent=4))
    elif args.get_attrs:
        print(json.dumps(inst.get_attrs(args.get_attrs)))
    elif args.preview_field:
        print(json.dumps(inst.preview_field(args.preview_field)))
    elif args.read_dataset:
        print(json.dumps(inst.read_dataset(args.read_dataset)))
    elif args.is_group:
        print(json.dumps(inst.is_group(args.is_group)))
    elif args.is_field:
        print(json.dumps(inst.is_field(args.is_field)))
    sys.exit(0)
