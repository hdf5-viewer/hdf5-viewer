#!/usr/bin/env python3

import h5py
import numpy
import json
import sys
import argparse

def meta_dict(name: str, obj) -> dict:
    if isinstance(obj, h5py.Group):
        return {
            'type': 'group',
            'name': name,
            'children': []
        }
    elif isinstance(obj, h5py.Dataset):
        shape = "scalar"
        if len(obj.shape) > 0:
            shape = str(obj.shape)
        return {
            'type': 'dataset',
            'name': name,
            'shape': shape,
            'dtype': str(obj.dtype)
        }
    else:
        raise Exception(f"'{name}' is not a dataset or group")

class H5Instance:
    def __init__(self, filename: str):
        self.instance =  h5py.File(filename)

    def get_fields(self, root: str) -> dict:
        obj = self.instance[root]
        if not isinstance(obj, h5py.Group):
            raise Exception(f"'{root}' is not a group")
        fields = {}
        for cname, cobj in obj.items():
            fields[cname] = meta_dict(cname, cobj)
        return fields

    def preview_field(self, field: str) -> dict:
        obj = self.instance[field]
        meta = meta_dict(field, obj)
        numpy.set_printoptions(threshold=10, linewidth=sys.maxsize)
        if isinstance(obj, h5py.Group):
            # Return fields in group
            meta['data'] = str(list(obj.keys()))
        else:
            # Return data in field
            meta['data'] = str(obj[()])
        return meta

    def read_field(self, field: str) -> dict:
        obj = self.instance[field]
        meta = meta_dict(field, obj)
        linewidth = sys.maxsize
        if len(obj.shape) == 1:
            # Print 1d arrays vertically
            linewidth = 1
        numpy.set_printoptions(threshold=sys.maxsize, linewidth=linewidth)
        meta['data'] = str(obj[()])
        return meta

    def is_group(self, field: str) -> dict:
        try:
            obj = self.instance[field]
            if isinstance(obj, h5py.Group):
                return {"return": True}
            return {"return": False}
        except:
            return {"return": False}

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('filepath', type=str,
                        help='File to parse')
    parser.add_argument('--get-fields', type=str,
                        help='Print fields within group')
    parser.add_argument('--preview-field', type=str,
                        help='Print preview of requested field')
    parser.add_argument('--read-field', type=str,
                        help='Print field data')
    parser.add_argument('--is-group', type=str,
                        help='Print true if field is group')
    args = parser.parse_args()

    inst = H5Instance(args.filepath)

    if args.get_fields:
        print(json.dumps(inst.get_fields(args.get_fields), indent=4))
        exit(0)
    if args.preview_field:
        print(json.dumps(inst.preview_field(args.preview_field)))
        exit(0)
    if args.read_field:
        print(json.dumps(inst.read_field(args.read_field)))
        exit(0)
    if args.is_group:
        print(json.dumps(inst.is_group(args.is_group)))
        exit(0)
