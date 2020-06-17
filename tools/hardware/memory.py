#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

import functools

import hardware.utils as utils


@functools.total_ordering
class Region:
    ''' Represents a region of memory. '''

    def __init__(self, base: int, size: int, owner: 'WrappedNode'):
        self.base = base
        self.size = size
        self.owner = owner

    @staticmethod
    def clone(other):
        ret = Region(other.base, other.size)
        return ret

    def __repr__(self):
        return 'Region(base=0x{:x},size=0x{:x})'.format(self.base, self.size)

    def __eq__(self, other):
        return self.base == other.base and self.size == other.size

    def __ne__(self, other):
        # Needed only for py2.
        return not self.__eq__(other)

    def __gt__(self, other):
        return self.base > other.base

    def __hash__(self):
        return hash((self.base, self.size))

    @staticmethod
    def from_range(start, end, owner):
        ''' create a region from a start/end rather than start/size '''
        ret = Region(start, end - start, owner)
        return ret

    def overlaps(self, other):
        ''' returns True if this region overlaps the given region '''
        # either our base is first, and to overlap our end must be > other.base
        if self.base <= other.base and (self.base + self.size) > other.base:
            return True
        # or other.base is first, and to overlap other's end must be > self.base
        elif other.base <= self.base and (other.base + other.size) > self.base:
            return True
        return False

    def reserve(self, excluded):
        ''' returns an array of regions that represent this region
        minus the excluded range '''
        if not self.overlaps(excluded):
            return [Region(self.base, self.size, self.owner)]

        ret = []
        if self.base < excluded.base:
            # the first region is from our base to excluded.base
            ret.append(Region.from_range(self.base, excluded.base, self.owner))
            # skip the region from excluded.base - excluded.base + excluded.size
            # if there's anything left, add it.
            if (excluded.base + excluded.size) < (self.base + self.size):
                ret.append(Region.from_range(excluded.base + excluded.size,
                                             self.base + self.size, self.owner))
        else:  # self.base >= excluded.base
            # we skip the first chunk
            # we add what's left after the current chunk.
            if (self.base + self.size) > (excluded.base + excluded.size):
                ret.append(Region.from_range(excluded.base + excluded.size,
                                             self.base + self.size, self.owner))
        return ret

    def align_base(self, align_bits):
        ''' align this region up to a given number of bits '''
        new_base = utils.align_up(self.base, align_bits)
        diff = new_base - self.base
        new_size = self.size - diff
        new = Region(new_base, new_size, self.owner)
        return new

    def align_size(self, align_bits):
        ''' align this region's size to a given number of bits.
         will move the base address down and the region's size
         up '''
        new_base = utils.align_down(self.base, align_bits)
        new_size = utils.align_up(self.size, align_bits)
        new = Region(new_base, new_size, self.owner)
        return new

    def make_chunks(self, chunksz):
        base = self.base
        size = self.size
        ret = []
        while size > 0:
            ret.append(Region(base, min(size, chunksz), self.owner))
            base += chunksz
            size -= chunksz
        return ret
