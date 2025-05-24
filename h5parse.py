#!/usr/bin/env python3

import h5py
import numpy
import json
import sys
import argparse

def meta_dict(obj) -> dict:
    if isinstance(obj, h5py.Group):
        return {
            'type': 'group',
            'name': obj.name,
            'children': []
        }
    elif isinstance(obj, h5py.Dataset):
        shape = "scalar"
        # if len(obj.shape) > 0:
        #     shape = [int(x) for x in obj.shape]
        return {
            'type': 'dataset',
            'name': obj.name,
            'shape': str(obj.shape),
            'dtype': str(obj.dtype)
        }
    else:
        raise Exception(f"'{obj.name}' is not a dataset or group")

class H5Instance:
    def __init__(self, filename: str):
        self.instance =  h5py.File(filename)

    def get_fields(self, root: str) -> dict:
        """Get Groups and Datasets of the Group ROOT"""
        if not self.is_group(root)["return"]:
            raise Exception(f"'{root}' is not a group")
        obj = self.instance[root]
        fields = {}
        for cname, cobj in obj.items():
            fields[cname] = meta_dict(cobj)
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
        obj = self.instance[root]
        if not self.is_field(root)["return"]:
            raise Exception("Argument to --get-attrs must be a Group or Dataset.")
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
    exit(0)
