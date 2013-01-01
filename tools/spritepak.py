#!/usr/bin/env python

from PIL import Image
import glob
import sys
import os
import struct

packing = 'Piiffff12s0l'

def find_visible_bounds(image, pad=0):
    data = list(image.getdata())
    w, h = image.size

    def getpx(x, y):
        v = data[x + (y * w)]
        if len(v) == 4: return v
        return (v[0], v[1], v[2], 255)

    def first_hit(gen):
        first_value = None
        for v in gen:
            if not first_value: first_value = v
            x, y = v
            r, g, b, a = getpx(x, y)
            if a > 0:
                return v
        return first_value

    def top_gen():
        for y in xrange(h):
            for x in xrange(w):
                yield (x, y)

    def bottom_gen():
        for ty in xrange(h):
            for x in xrange(w):
                yield (x, (h - 1 - ty))

    def left_gen():
        for x in xrange(w):
            for y in xrange(h):
                yield (x, y)

    def right_gen():
        for tx in xrange(w):
            for y in xrange(h):
                yield ((w - 1 - tx), y)

    left, junk = first_hit(left_gen())
    right, junk = first_hit(right_gen())
    junk, top = first_hit(top_gen())
    junk, bottom = first_hit(bottom_gen())

    if left >= pad: left -= pad
    if right <= (w - 1 - pad): right += pad
    if top >= pad: top -= pad
    if bottom <= (h - 1 - pad): bottom += pad

    return (left, top, right, bottom)


def mk_sheet(filenames, outbase, tgt_dims, trim):
    outimagename = outbase + '.png'
    outdatname = outbase + '.dat'
    outdat = open(outdatname, 'w')

    tgt_w, tgt_h = tgt_dims
    tgt = Image.new("RGBA", tgt_dims)
    metadata = []

    current_x = 0
    current_y = 0
    pad = 1

    max_row_h = 0

    for fname in filenames:
        junk, basename = os.path.split(fname)
        img = Image.open(fname)

        if basename in trim:
            img = img.crop(find_visible_bounds(img))

        img_w, img_h = img.size

        def state_str():
            return '"%s" (%d, %d)' % (fname, img_w, img_h)

        if current_x + img_w > tgt_w:
            if max_row_h == 0:
                raise Exception('%s: nothing fits' % state_str())

            current_y += max_row_h + pad
            current_x = 0

            max_row_h = 0

        if current_y + img_h > tgt_h:
            raise Exception('%s: out of vertical space' % state_str())

        max_row_h = max(max_row_h, img_h)

        # finally, insert the image
        tgt.paste(img, (current_x, current_y))

        u0 = float(current_x) / tgt_w
        v1 = float(current_y) / tgt_h
        u1 = float(current_x + img_w) / tgt_w
        v0 = float(current_y + img_h) / tgt_h
        pakname, _ = os.path.splitext(basename)
        struct_tuple = (0, img_w, img_h, u0, v0, u1, v1, pakname)
        packed = struct.pack(packing, *struct_tuple)
        outdat.write(packed)

        current_x += img_w + pad

    tgt.save(outimagename)
    outdat.close()

if __name__ == '__main__':
    if len(sys.argv) < 4:
        print 'usage: %s trimfile outprefix [input files]' % sys.argv[0]
        exit(1)

    trimfile = sys.argv[1]
    outfile = sys.argv[2]
    infiles = sys.argv[3:]

    with open(trimfile) as f:
        trim = [ name.rstrip() for name in f ]

    mk_sheet(infiles, outfile, (1024, 1024), trim)
