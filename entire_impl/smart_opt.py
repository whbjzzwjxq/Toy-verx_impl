#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import argparse
from collections import defaultdict
import logging
import sys
import pydot

from os.path import abspath, dirname, join, basename

# Prepend .. to $PATH so the project modules can be imported below
src_path = join(dirname(abspath(__file__)), "..")
sys.path.insert(0, src_path)

# Local project imports
import src.vandal.exporter as exporter
import src.vandal.dataflow as dataflow
import src.vandal.tac_cfg as tac_cfg
import src.vandal.settings as settings
import src.vandal.opcodes as opcodes
import src.analysis.core_analysis as analysis


# Version string to display with -v
VERSION = """\
+------------------------------+
| Vandal EVM Decompiler v0.0.3 |
| (c) The University of Sydney |
+------------------------------+\
"""


# Define a version() function in case we want dynamic version strings later
def version():
    return VERSION


def select_color(block: tac_cfg.TACBasicBlock):
    last_opcode = block.last_op.opcode
    has_create, has_call = False, False
    for op in block.tac_ops:
        if op.opcode == opcodes.CREATE:
            has_create = True
        if op.opcode == opcodes.CALL:
            has_call = True
    if last_opcode == opcodes.RETURN:
        return 'green'
    elif last_opcode == opcodes.STOP:
        return 'blue'
    elif last_opcode.is_exception():
        return 'red'
    elif last_opcode == opcodes.SELFDESTRUCT:
        return 'purple'
    elif has_create:
        return 'brown'
    elif has_call:
        return 'orange'
    else:
        return 'black'


def gen_content(block: tac_cfg.TACBasicBlock):
    strs = f'{block.ident()}\n'
    for op in block.tac_ops:
        strs = f'{strs}{str(op)}\n'
    return strs
    

def draw_dot(cfg: tac_cfg.TACGraph, name: str):
    graph = pydot.Dot(name, graph_type='digraph', bgcolor='white')
    created_blocks = defaultdict(bool)

    def create_block(_block):
        _idx = _block.ident()
        if not created_blocks[_idx]:
            node = pydot.Node(_block.ident(), label=gen_content(_block), color=select_color(_block), shape='box')
            graph.add_node(node)
            created_blocks[_idx] = True
        return _idx

    for block in cfg.blocks:
        idx = create_block(block)
        for succ in block.succs:
            succ_idx = create_block(succ)
            edge = pydot.Edge(idx, succ_idx)
            graph.add_edge(edge)
    return graph.to_string()

def gen_args():
    parser = argparse.ArgumentParser(
        description="An EVM bytecode disassembly decompiler that generates "
                    "three-address code for program analysis. Use config.ini "
                    "to set further configuration options.")

    parser.add_argument("-a",
                        "--disassembly",
                        action="store_true",
                        default=False,
                        help="decompile dissassembled input. Default: decompile bytecode input")

    parser.add_argument("-g",
                        "--graph",
                        action="store_true",
                        default=False,
                        help="Output graph as graphviz .dot format")

    parser.add_argument("-t",
                        "--tsv",
                        nargs="?",
                        const="",
                        metavar="DIR",
                        default=None,
                        help="generate tab-separated .facts files for Souffle "
                            "and write files to the specified directory, which "
                            "will be recursively created if it does not exist "
                            "(current working directory by default).")

    parser.add_argument("-d",
                        "--dominators",
                        action="store_true",
                        default=False,
                        help="If producing tsv output, also include graph "
                            "dominator relations.")

    parser.add_argument("-o",
                        "--opcodes",
                        nargs="*",
                        default=[],
                        metavar="OPCODE",
                        help="If producing tsv output, also include relations "
                            "encoding all occurrences of the specified "
                            "list of opcodes. Opcode X will be stored in "
                            "op_X.facts.")

    parser.add_argument("-c",
                        "--config",
                        metavar="CFG_STRING",
                        help="override settings from the configuration files "
                            "in the format \"key1=value1, key2=value2...\" "
                            "(with the quotation marks).")

    parser.add_argument("-C",
                        "--config_file",
                        default=settings._CONFIG_LOC_,
                        metavar="FILE",
                        help="read the settings from the given file; "
                            "any given settings will override the defaults. "
                            "Read from config.ini if not set.")

    parser.add_argument("-v",
                        "--verbose",
                        action="store_true",
                        help="emit verbose debug output to stderr.")

    parser.add_argument("-vv",
                        "--prolix",
                        action="store_true",
                        help="higher verbosity level, including extra debug "
                            "messages from dataflow analysis and elsewhere.")

    parser.add_argument("-n",
                        "--no_out",
                        action="store_true",
                        help="do not output decompiled graph.")

    parser.add_argument("-V",
                        "--version",
                        action="store_true",
                        help="show program's version number and exit.")

    parser.add_argument("infile",
                        nargs="?",
                        type=argparse.FileType("r"),
                        default=sys.stdin,
                        help="file from which decompiler input should be read "
                            "(stdin by default).")

    parser.add_argument("outfile",
                        nargs="?",
                        type=argparse.FileType("w"),
                        default=sys.stdout,
                        help="file to which decompiler output should be written "
                            "(stdout by default).")

    # Parse the arguments.
    args = parser.parse_args()
    return args

def main():
    args = gen_args()

    # Set up logger, with appropriate log level depending on verbosity.
    log_level = logging.WARNING
    if args.prolix:
        log_level = logging.DEBUG
    elif args.verbose:
        log_level = logging.INFO
    logging.basicConfig(format='%(levelname)s: %(message)s', level=log_level)

    # Handle --version
    if args.version:
        print(version())
        sys.exit(1)

    # Always show version for log_level >= LOW
    logging.info("\n" + version())

    # Initialise data flow settings.
    settings.import_config(args.config_file)

    # Override config file with any provided settings.
    if args.config is not None:
        pairs = [pair.split("=") for pair in args.config.replace(" ", "").split(",")]
        for k, v in pairs:
            settings.set_from_string(k, v)

    # Build TAC CFG from input file
    try:
        logging.info("Reading from '%s'.", args.infile.name)
        if args.disassembly:
            cfg = tac_cfg.TACGraph.from_dasm(args.infile)
        else:
            cfg = tac_cfg.TACGraph.from_bytecode(args.infile)
        logging.info("Initial CFG generation completed.")

    # Catch a Control-C and exit with UNIX failure status 1
    except KeyboardInterrupt:
        logging.critical("\nInterrupted by user")
        sys.exit(1)

    # Initialise data flow settings.
    settings.import_config(args.config_file)

    # Override config file with any provided settings.
    if args.config is not None:
        pairs = [pair.split("=") for pair in args.config.replace(" ", "").split(",")]
        for k, v in pairs:
            settings.set_from_string(k, v)

    # Run data flow analysis
    dataflow.analyse_graph(cfg)
    _analysis = analysis.CostAnalysis(cfg)
    if args.graph:
        cfg = _analysis.source
        output = draw_dot(cfg, name=basename(args.infile.name))
    else:
        output = _analysis.analyze()
    print(output)


if __name__ == '__main__':
    main()
