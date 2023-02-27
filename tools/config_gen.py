# Copyright 2022, seL4 Project a Series of LF Projects, LLC
#
# SPDX-License-Identifier: BSD-2-Clause
#

import argparse
import json
import sys
import yaml


def main():
    parser = argparse.ArgumentParser(
        description="Translate a YAML configuration file (e.g. gen_config.yaml) into other formats.",
    )
    parser.add_argument(
        "in_file_yaml",
        metavar="IN_FILE",
        nargs="?",
        type=argparse.FileType("r"),
        default=sys.stdin,
        help="Input YAML file.",
    )
    parser.add_argument(
        "--write-c",
        metavar="OUT_FILE",
        dest="out_file_c",
        type=argparse.FileType("w"),
        default=sys.stdout,
        help="Output C header file.",
    )
    parser.add_argument(
        "--write-json",
        metavar="OUT_FILE",
        dest="out_file_json",
        type=argparse.FileType("w"),
        default=sys.stdout,
        help="Output JSON file.",
    )
    args = parser.parse_args()
    generate(args.in_file_yaml, out_file_c=args.out_file_c,
             out_file_json=args.out_file_json)


def generate(in_file_yaml, out_file_c=None, out_file_json=None):
    config = yaml.safe_load(in_file_yaml)
    if out_file_c is not None:
        generate_c(config, out_file_c)
    if out_file_json is not None:
        generate_json(config, out_file_json)


def generate_c(config, out_file):
    header_contents = "#pragma once\n\n"

    for key, value in config.items():
        macro = f"CONFIG_{key}"

        if isinstance(value, bool):
            if value:
                entry = f"#define {macro}  1"
            else:
                entry = f"/* disabled: {macro} */"
        elif isinstance(value, str):
            if value:
                entry = f"#define {macro}  {value}"
            else:
                entry = f"#define {macro}  /* empty */"
        else:
            raise Exception(
                f"Unexpected type for configuration key {key}:", type(value))

        header_contents += f"{entry}\n"

    out_file.write(header_contents)


def generate_json(config, out_file):
    json.dump(config, out_file, indent=4)


if __name__ == "__main__":
    main()
