# SPDX-License-Identifier: GPL-2.0-only
"""Regression tests for the hardware generator's memory region alignment."""
import pathlib
import sys
import unittest

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parents[1]))
from hardware.memory import Region


class TestRegionAlignment(unittest.TestCase):
    def test_align_size_covers_original_region(self):
        for base, size, bits in (
            (0x1000, 0x1000, 12),  # already aligned
            (0x1003, 0x20, 12),    # unaligned, within one page
            (0x1003, 0x1000, 12),  # unaligned, crosses into next page
            (0x1ff0, 0x30, 12),    # crosses a page boundary
            (0x200003, 0x200000, 21),  # RISC-V device page alignment
        ):
            with self.subTest(base=base, size=size, bits=bits):
                region = Region(base, size)
                aligned = region.align_size(bits)
                self.assertEqual(aligned.base % (1 << bits), 0)
                self.assertEqual(aligned.size % (1 << bits), 0)
                self.assertLessEqual(aligned.base, region.base)
                self.assertGreaterEqual(aligned.base + aligned.size, region.base + region.size)

    def test_empty_region_remains_empty(self):
        aligned = Region(0x1003, 0).align_size(12)
        self.assertEqual(aligned.size, 0)


if __name__ == "__main__":
    unittest.main()
