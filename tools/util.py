import struct

norm_coord_scale = 2 ** 15
coord_scale = 2 ** 6

short_packing = '>h'
fstring_packing = '12s'

def normfloat2short(fl):
    return fl * norm_coord_scale

def float2short(fl):
    return fl * coord_scale

def write_short(f, sh):
    f.write(struct.pack(short_packing, sh))

def write_norm_float(f, fl):
    write_short(f, normfloat2short(fl))

def write_float(f, fl):
    write_short(f, float2short(fl))

def write_fstring(f, s):
    f.write(struct.pack(fstring_packing, s.encode("utf8")))
