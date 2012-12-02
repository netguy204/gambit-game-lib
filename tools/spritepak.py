#!/usr/bin/env python

from PIL import Image
import glob
import sys
import os
import struct

packing = 'Piiffff12s0l'

def mk_sheet(filenames, outbase, tgt_dims):
    outimagename = outbase + '.png'
    outdatname = outbase + '.dat'
    outdat = open(outdatname, 'w')

    tgt_w, tgt_h = tgt_dims
    tgt = Image.new("RGBA", tgt_dims)
    metadata = []

    current_x = 0
    current_y = 0

    max_row_h = 0

    for fname in filenames:
        img = Image.open(fname)
        img_w, img_h = img.size

        def state_str():
            return '"%s" (%d, %d)' % (fname, img_w, img_h)

        if current_x + img_w > tgt_w:
            if max_row_h == 0:
                raise Exception('%s: nothing fits' % state_str())

            current_y += max_row_h
            current_x = 0

            max_row_h = 0

        if current_y + img_h > tgt_h:
            raise Exception('%s: out of vertical space' % state_str())

        max_row_h = max(max_row_h, img_h)

        # finally, insert the image
        tgt.paste(img, (current_x, current_y))
        #img.close()

        junk, basename = os.path.split(fname)
        u0 = float(current_x) / tgt_w
        v1 = float(current_y) / tgt_h
        u1 = float(current_x + img_w) / tgt_w
        v0 = float(current_y + img_h) / tgt_h
        struct_tuple = (0, img_w, img_h, u0, v0, u1, v1, basename)
        print struct_tuple
        packed = struct.pack(packing, *struct_tuple)
        print "size: %d" % len(packed)
        outdat.write(packed)

        current_x += img_w

    tgt.save(outimagename)
    outdat.close()

if __name__ == '__main__':
    if len(sys.argv) < 3:
        print 'usage: %s outfile [input files]' % sys.argv[0]
        exit(1)

    outfile = sys.argv[1]
    infiles = sys.argv[2:]

    mk_sheet(infiles, outfile, (1024, 1024))
