#!/usr/bin/env python

import xml.dom.minidom
import json
import os.path
import util
import math

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
        frame = []
        if key.hasAttribute('time'):
            time = attr(key, 'time')
        else:
            time = "0"

        for obj in elements(key, 'object_ref'):
            keyid = attr(obj, 'key')
            tlid = attr(obj, 'timeline')
            frame.append({'timeline': tlid, 'key': keyid})

        keyframes.append({'time': time,
                          'elements': frame})

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
    result = {}
    for tlxml in elements(dom, 'timeline'):
        tl = {}
        for keyxml in elements(tlxml, 'key'):
            subframe = []
            for objxml in elements(keyxml, 'object'):
                key = resource_key(attr(objxml, 'folder'),
                                   attr(objxml, 'file'))
                fname = res[key]
                values = extract_dict(
                    objxml,
                    ['x', 'y', 'pivot_x', 'pivot_y',
                     'angle', 'scale_x', 'scale_y'],
                    {'x': '0', 'y': '0', 'pivot_x': '0', 'pivot_y': '1',
                     'angle': '0', 'scale_x': '1', 'scale_y': '1'})
                values['file'], _ = os.path.splitext(fname)
                subframe.append(values)
            tl[attr(keyxml, 'id')] = subframe
        result[attr(tlxml, 'id')] = tl
    return result

def write_ent(f, ent):
    # num animations
    util.write_short(f, len(ent))

    for aname, a in ent.items():
        util.write_fstring(f, aname)
        util.write_short(f, int(a['length']))
        util.write_short(f, int(a['looping'] == 'true'))
        util.write_short(f, len(a['keyframes']))

        for frame in a['keyframes']:
            util.write_short(f, int(frame['time']))
            util.write_short(f, len(frame['elements']))

            for el in frame['elements']:
                util.write_short(f, int(el['timeline']))
                util.write_short(f, int(el['key']))

        util.write_short(f, len(a['timelines']))
        for tlid, tl in a['timelines'].items():
            util.write_short(f, len(tl))
            for elid, els in tl.items():
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

if __name__ == '__main__':
    fname = '/Users/btaylor/Downloads/lever/lever.scml'
    with open(fname) as f:
        dom = xml.dom.minidom.parse(f)

    res = resources(dom)
    ents = entities(res, dom)

    # don't know how to serialize more than 1 entity
    assert(len(ents) == 1)

    ent = ents.itervalues().next()

    print json.dumps(ent, sort_keys=True,
                     indent=4, separators=(',', ': '))

    with open("out", "w") as f:
        write_ent(f, ent)
