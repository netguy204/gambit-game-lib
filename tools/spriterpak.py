#!/usr/bin/env python

import xml.dom.minidom
import json
import os.path
import util
import math
import sys

def attr(dom, name):
    return dom.getAttribute(name)

def elements(dom, name):
    return dom.getElementsByTagName(name)

def resource_key(folder, fl):
    return '%s,%s' % (folder, fl)

def resources(dom):
    result = dict()
    for folder in elements(dom, 'folder'):
        for f in elements(folder, 'file'):
            key = resource_key(attr(folder, 'id'), attr(f, 'id'))
            result[key] = attr(f, 'name')
    return result

def entities(res, dom):
    result = dict()
    for exml in elements(dom, 'entity'):
        key = attr(exml, 'id')
        result[attr(exml, 'id')] = animations(res, exml)
    return result

def animations(res, dom):
    result = dict()
    for axml in elements(dom, 'animation'):
        result[attr(axml, 'name')] = animation(res, axml)
    return result

def animation(res, dom):
    tls = timelines(res, dom)
    main = elements(dom, 'mainline')[0]
    keyframes = []

    for key in elements(main, 'key'):
        if key.hasAttribute('time'):
            time = attr(key, 'time')
        else:
            time = "0"

        def extract_ref(obj):
            keyid = attr(obj, 'key')
            tlid = attr(obj, 'timeline')
            if obj.hasAttribute('parent'):
                parent = attr(obj, 'parent')
            else:
                parent = "-1"
            return {'timeline': tlid, 'key': keyid,
                    'parent': parent}

        bones = []
        for obj in elements(key, 'bone_ref'):
            bones.append(extract_ref(obj))

        frame = []
        for obj in elements(key, 'object_ref'):
            frame.append(extract_ref(obj))

        keyframes.append({'time': time,
                          'elements': frame,
                          'bones': bones})

    result = {
        'length': attr(dom, 'length'),
        'looping': attr(dom, 'looping'),
        'keyframes': keyframes,
        'timelines': tls
        }
    return result

def extract_dict(node, keys, defaults):
    result = dict()
    for key in keys:
        if node.hasAttribute(key):
            result[key] = attr(node, key)
        else:
            result[key] = defaults[key]
    return result

def timelines(res, dom):
    result = []
    for tlxml in elements(dom, 'timeline'):
        tl = []
        for keyxml in elements(tlxml, 'key'):
            subframe = []
            for objxml in keyxml.childNodes:
                if objxml.nodeType == objxml.TEXT_NODE:
                    continue

                if objxml.hasAttribute('folder'):
                    key = resource_key(attr(objxml, 'folder'),
                                       attr(objxml, 'file'))
                    fname = res[key]
                else:
                    fname = ''

                values = extract_dict(
                    objxml,
                    ['x', 'y', 'pivot_x', 'pivot_y',
                     'angle', 'scale_x', 'scale_y'],
                    {'x': '0', 'y': '0', 'pivot_x': '0', 'pivot_y': '1',
                     'angle': '0', 'scale_x': '1', 'scale_y': '1'})
                if keyxml.hasAttribute('spin'):
                    values['spin'] = attr(keyxml, 'spin')
                else:
                    values['spin'] = "1"

                values['file'], _ = os.path.splitext(fname)
                subframe.append(values)
            if subframe: tl.append(subframe)
        if tl: result.append(tl)
    return result

def write_ent(f, ent):
    # num animations
    util.write_short(f, len(ent))

    for aname, a in ent.items():
        util.write_fstring(f, aname)
        util.write_short(f, int(a['length']))
        util.write_short(f, int(a['looping'] != 'false'))
        util.write_short(f, len(a['keyframes']))

        for frame in a['keyframes']:
            util.write_short(f, int(frame['time']))

            util.write_short(f, len(frame['elements']))
            for el in frame['elements']:
                util.write_short(f, int(el['timeline']))
                util.write_short(f, int(el['key']))
                util.write_short(f, int(el['parent']))

            util.write_short(f, len(frame['bones']))
            for el in frame['bones']:
                util.write_short(f, int(el['timeline']))
                util.write_short(f, int(el['key']))
                util.write_short(f, int(el['parent']))

        util.write_short(f, len(a['timelines']))
        for tl in a['timelines']:
            util.write_short(f, len(tl))
            for els in tl:
                assert(len(els) == 1)
                el = els[0]
                util.write_fstring(f, el['file'])
                util.write_float(f, float(el['angle']) * math.pi / 180.0)
                util.write_float(f, float(el['pivot_x']))
                util.write_float(f, float(el['pivot_y']))
                util.write_float(f, float(el['scale_x']))
                util.write_float(f, float(el['scale_y']))
                util.write_float(f, float(el['x']))
                util.write_float(f, float(el['y']))
                util.write_short(f, int(el['spin']))

if __name__ == '__main__':
    if len(sys.argv) != 3:
        print 'usage: %s input output' % (sys.argv[0])
        exit(1)

    fname = sys.argv[1]
    outname = sys.argv[2]

    with open(fname) as f:
        dom = xml.dom.minidom.parse(f)

    res = resources(dom)
    ents = entities(res, dom)

    # don't know how to serialize more than 1 entity
    assert(len(ents) == 1)

    ent = ents.itervalues().next()

    #print json.dumps(ent, sort_keys=True,
    #                 indent=4, separators=(',', ': '))

    with open(outname, "w") as f:
        write_ent(f, ent)
