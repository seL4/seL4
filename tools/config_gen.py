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
        "--skip-unchanged",
        dest="skip_unchanged",
        action="store_true",
        help="Only write output file if new content is different"
    )
    parser.add_argument(
        "--write-c",
        metavar="OUT_FILE",
        dest="out_file_c",
        nargs="?",
        # Use a+ mode to open in read/write mode without truncating existing contents
        type=argparse.FileType("a+"),
        const=sys.stdout,
        default=None,
        help="Output C header file.",
    )
    parser.add_argument(
        "--write-json",
        metavar="OUT_FILE",
        dest="out_file_json",
        nargs="?",
        type=argparse.FileType("a+"),
        const=sys.stdout,
        default=None,
        help="Output JSON file.",
    )
    args = parser.parse_args()
    generate(args.in_file_yaml, out_file_c=args.out_file_c,
             out_file_json=args.out_file_json, skip_unchanged=args.skip_unchanged)


def generate(in_file_yaml, out_file_c=None, out_file_json=None, skip_unchanged=False):
    config = yaml.safe_load(in_file_yaml)
    if out_file_c is not None:
        generate_c(config, out_file_c, skip_unchanged)
    if out_file_json is not None:
        generate_json(config, out_file_json, skip_unchanged)


def generate_c(config, out_file, skip_unchanged):
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

    write_file_lazy(out_file, header_contents, skip_unchanged)


def generate_json(config, out_file, skip_unchanged):
    json_contents = json.dumps(config, indent=4)
    write_file_lazy(out_file, json_contents, skip_unchanged)


def write_file_lazy(out_file, content, skip_unchanged):
    if out_file == sys.stdout:
        out_file.write(content)
        return

    out_file.seek(0)
    if not skip_unchanged or out_file.read() != content:
        out_file.seek(0)
        out_file.truncate()
        out_file.write(content)


if __name__ == "__main__":
    main()
