import os
import Image
from itertools import *

class CropRegion(object):
    def __init__(self, ulx, uly,  lrx, lry):
        self.ulx = ulx
        self.uly = uly
        self.lrx = lrx
        self.lry = lry

    def width(self):
        return self.lrx - self.ulx + 1

    def height(self):
        return self.lry - self.uly + 1

    def region_tuple(self):
        return (self.ulx, self.uly, self.lrx, self.lry)
    
max_size_cfg = {
    'dir' : 'large',
    'w' : {'1w' : CropRegion(53, 484, 87, 509),
           '2w' : CropRegion(89, 484, 124, 509),
           '3w' : CropRegion(125, 484, 161, 509),
           '4w' : CropRegion(166, 484, 195, 509),
           '5w' : CropRegion(200, 484, 236, 509),
           '6w' : CropRegion(237, 484, 273, 509),
           '7w' : CropRegion(274, 484, 310, 509),
           '8w' : CropRegion(311, 484, 347, 509),
           '9w' : CropRegion(348, 484, 384, 509),
           'wan_char' : CropRegion(447, 199, 486, 252)
           },
    'b' : {'1b' : CropRegion(239, 295, 294, 350),
           '2b' : CropRegion(299, 285, 332, 357),
           '3b' : CropRegion(338, 208, 388, 277),
           'red' : CropRegion(420, 227, 441, 248),
           'green' : CropRegion(397, 253, 418, 274),
           'blue' : CropRegion(420, 253, 441, 274),},
    't' : {'1t': CropRegion(441, 297, 490, 366),
           'large_red' : CropRegion(404, 200, 418, 237),
           'large_green' : CropRegion(388, 200, 402, 237),
           'small_red' : CropRegion(427, 282, 438, 305),
           'small_green' : CropRegion(414, 282, 425, 305),
           'small_blue' : CropRegion(399, 282, 410, 305),
           '8t' : CropRegion(445, 255, 489, 292),
           },
    'honor' : {
        'f1' : CropRegion(30, 374, 81, 428),
        'f2' : CropRegion(36, 303, 80, 360),
        'f3' : CropRegion(240, 382, 282, 423),
        'f4' : CropRegion(301, 376, 349, 424),
        'j1' : CropRegion(363, 367, 406, 430),
        'j2' : CropRegion(413, 370, 467, 425),
        'j3' : CropRegion(96, 348, 145, 425),
        },
    'tile' :  {
        'front' : CropRegion(157, 301, 229, 435),
        'front_panel' : CropRegion(167, 342, 222, 423),
        'back' : None,
        },
    }

medium_size_cfg = {
    'dir' : 'medium',
    'w' : {'1w' : CropRegion(33, 140, 60, 161),
           '2w' : CropRegion(62, 140, 90, 161),
           '3w' : CropRegion(91, 140, 121, 161),
           '4w' : CropRegion(124, 140, 148, 161),
           '5w' : CropRegion(151, 140, 180, 161),
           '6w' : CropRegion(181, 140, 211, 161),
           '7w' : CropRegion(212, 140, 240, 161),
           '8w' : CropRegion(241, 140, 270, 161),
           '9w' : CropRegion(271, 140, 301, 161),
           'wan_char' : CropRegion(323, 100, 353, 140)
           },
    'b' : {'1b' : CropRegion(318, 51, 361, 93),
           '2b' : CropRegion(364, 42, 390, 99),
           '3b' : CropRegion(392, 43, 431, 97),
           'red' : CropRegion(453, 58, 469, 75),
           'green' : CropRegion(435, 78, 451, 94),
           'blue' : CropRegion(453, 78, 469, 94),},
    't' : {'1t': CropRegion(62, 167, 101, 219),
           'large_red' : CropRegion(118, 171, 128, 199),
           'large_green' : CropRegion(106, 171, 116, 199),
           'small_blue' : CropRegion(144, 173, 153, 192),
           'small_green' : CropRegion(144, 195, 153, 214),
           'small_red' : CropRegion(155, 195, 165, 214),
           '8t' : CropRegion(178, 172, 209, 199),
           },
    'honor' : {
        'f1' : CropRegion(50, 436, 90, 479),
        'f2' : CropRegion(110, 433, 145, 478),
        'f3' : CropRegion(181, 442, 213, 474),
        'f4' : CropRegion(228, 437, 266, 476),
        'j1' : CropRegion(277, 430, 310, 480),
        'j2' : CropRegion(316, 433, 358, 476),
        'j3' : CropRegion(364, 133, 401, 192),
        },
    'tile' :  {
        'front' : CropRegion(145, 3, 207, 110),
        'front_panel' : CropRegion(150, 35, 196, 102),
        'back' : CropRegion(73, 3, 135, 110)
        },
    }

small_size_cfg = {
    'dir' : 'small',
    'w' : {'1w' : CropRegion(82, 223, 102, 239),
           '2w' : CropRegion(103, 223, 125, 239),
           '3w' : CropRegion(126, 223, 148, 239),
           '4w' : CropRegion(150, 223, 168, 239),
           '5w' : CropRegion(170, 223, 192, 239),
           '6w' : CropRegion(194, 223, 216, 239),
           '7w' : CropRegion(219, 223, 240, 239),
           '8w' : CropRegion(242, 223, 264, 239),
           '9w' : CropRegion(265, 223, 287, 239),
           'wan_char' : CropRegion(296, 249, 317, 278)
           },
    'b' : {'1b' : CropRegion(291, 213, 320, 243),
           '2b' : CropRegion(403, 118, 422, 156),
           '3b' : CropRegion(423, 118, 450, 156),
           'red' : CropRegion(466, 128, 478, 140),
           'green' : CropRegion(453, 142, 465, 154),
           'blue' : CropRegion(466, 142, 478, 154),},
    't' : {'1t': CropRegion(61, 245, 89, 282),
           'large_red' : CropRegion(102, 248, 108, 267),
           'large_green' : CropRegion(93, 248, 99, 267),
           'small_red' : CropRegion(135, 249, 140, 263),
           'small_green' : CropRegion(128, 249, 133, 263),
           'small_blue' : CropRegion(120, 249, 125, 263),
           '8t' : CropRegion(143, 248, 165, 268),
           },
    'honor' : {
        'f1' : CropRegion(216, 171, 245, 202),
        'f2' : CropRegion(254, 170, 279, 202),
        'f3' : CropRegion(290, 175, 314, 199),
        'f4' : CropRegion(328, 172, 356, 200),
        'j1' : CropRegion(180, 257, 204, 291),
        'j2' : CropRegion(213, 259, 244, 290),
        'j3' : CropRegion(259, 247, 286, 289),
        },
    'tile' :  {
        'front' : CropRegion(216, 3, 261, 81),
        'front_panel' : CropRegion(223, 10, 250, 55),
        'back' : CropRegion(10, 169, 55, 247),
        },
    }


def create_new(width, height):
    nimg = Image.new('RGBA', (width, height), (255, 255, 255, 0))
    return nimg

def create_new_front_tile(im, cfg):
    front_region = cfg['tile']['front']
    region = fetch_region(im, front_region)
    nimg = Image.new('RGBA', region.size, (255, 255, 255, 0))
    nimg.paste(region)
    return nimg

def calc_front_center_offset(cfg):
    panel_region = cfg['tile']['front_panel']
    center_x = (panel_region.ulx + panel_region.lrx) / 2
    center_y = (panel_region.uly + panel_region.lry) / 2
    front_region = cfg['tile']['front']
    return (center_x - front_region.ulx, center_y - front_region.uly)

def calc_real_front_center(cfg, front_ul_x, front_ul_y):
    offset_x, offset_y = calc_front_center_offset(cfg)
    return (front_ul_x + offset_x, front_ul_y + offset_y)

def paste_onto_front_tile(cfg, pic, tile, tile_transform=None):
    real_center_x, real_center_y = calc_real_front_center(cfg, 0, 0)
    offset = (real_center_x - pic.size[0] // 2, real_center_y - pic.size[1] // 2)
    if tile_transform:
        pic = tile_transform(pic)
    tile.paste(pic, offset, pic)
    if tile_transform:
        return tile_transform(tile)
    else:
        return tile

def compose_wan(enum, wan_char):
    width = max(enum.size[0], wan_char.size[0])
    height = enum.size[1] + wan_char.size[1]
    x_center = width // 2
    enum_offset = (x_center - enum.size[0] // 2, 0)
    wan_char_offset = (x_center - wan_char.size[0] // 2, enum.size[1])
    nimg = create_new(width, height)
    nimg.paste(enum, enum_offset)
    nimg.paste(wan_char, wan_char_offset)
    return nimg

def make_wan_char_img(im, cfg, enum):
    enum = fetch_region(im, wan_enum_crop_region(cfg, enum))
    wan_char = fetch_region(im, cfg['w']['wan_char'])
    nimg = compose_wan(enum, wan_char)
    return nimg

def wan_enum_crop_region(cfg, enum):
    k = '{0}w'.format(enum)
    return cfg['w'][k]

def make_bing_char_img(im, cfg, enum):
    panel_region = cfg['tile']['front_panel']
    panel_width = panel_region.width()
    panel_height =panel_region.height()
    def _make_1():
        r = fetch_region(im, cfg['b']['1b'])
        nimg = create_new(*r.size)
        nimg.paste(r, (0, 0))
        return nimg
    def _make_2():
        r = fetch_region(im, cfg['b']['2b'])
        nimg = create_new(*r.size)
        nimg.paste(r, (0, 0))
        return nimg
    def _make_3():
        r = fetch_region(im, cfg['b']['3b'])
        nimg = create_new(*r.size)
        nimg.paste(r, (0, 0))
        return nimg
    def _make_4():
        blue = fetch_region(im, cfg['b']['blue'])
        green = fetch_region(im, cfg['b']['green'])
        dim = max(max(blue.size), max(green.size))
        # hdistance = int(dim / 0.618)
        hdistance = int(panel_width / 2.0)
        #vdistance = int(dim*2)
        vdistance = int(panel_height / 2.0)
        total_width = hdistance + dim
        total_height = vdistance + dim
        tl_offset = (0, 0)
        tr_offset = (hdistance, 0)
        bl_offset = (0, vdistance)
        br_offset = (hdistance, vdistance)
        nimg = create_new(total_width, total_height)
        cycle_seq = (blue, green, green, blue)
        for args in izip(cycle_seq, (tl_offset, tr_offset, bl_offset, br_offset), cycle_seq):
            nimg.paste(*args)
        return nimg
    def _make_5():
        blue = fetch_region(im, cfg['b']['blue'])
        green = fetch_region(im, cfg['b']['green'])
        red =fetch_region(im, cfg['b']['red'])
        dim = max(max(max(blue.size), max(green.size)), max(red.size))
        #hdistance = int(dim / 0.618)
        hdistance = int(panel_width / 2.0)
        #vdistance = int(dim*2)
        vdistance = int(panel_height / 2.0)
        total_width = hdistance + dim
        total_height = vdistance + dim
        tl_offset = (0, 0)
        tr_offset = (hdistance, 0)
        center_x = total_width / 2
        center_y = total_height / 2
        cc_offset = (center_x - dim / 2, center_y - dim / 2)
        bl_offset = (0, vdistance)
        br_offset = (hdistance, vdistance)
        nimg = create_new(total_width, total_height)
        cycle_seq = (green, blue, red, blue, green)
        for args in izip(cycle_seq, (tl_offset, tr_offset, cc_offset, bl_offset, br_offset), cycle_seq):
            nimg.paste(*args)
        return nimg
    def _make_6():
        green = fetch_region(im, cfg['b']['green'])
        red =fetch_region(im, cfg['b']['red'])
        dim = max(max(green.size), max(red.size))
        hdistance = dim
        vdistance = int(dim / 0.618)
        total_width = hdistance + dim
        total_height = vdistance + dim*2
        tl_offset = (0, 0)
        tr_offset = (hdistance, 0)
        cl_offset = (0, vdistance)
        cr_offset = (hdistance, vdistance)
        bl_offset = (0, vdistance + dim)
        br_offset = (hdistance, vdistance + dim)
        nimg = create_new(total_width, total_height)
        cycle_seq = (green, green, red, red, red, red)
        for args in izip(cycle_seq, (tl_offset, tr_offset, cl_offset, cr_offset, bl_offset, br_offset), cycle_seq):
            nimg.paste(*args)
        return nimg
    def _make_7():
        green = fetch_region(im, cfg['b']['green'])
        red =fetch_region(im, cfg['b']['red'])
        dim = max(max(green.size), max(red.size))
        hdistance = int(dim*0.9)
        vdistance = int(dim / 0.618)
        vstep = int((vdistance - dim) / 2.0)
        total_width = hdistance*2 + dim
        total_height = dim*2 + vdistance
        tl_offset = (0, 0)
        tc_offset = (hdistance, vstep)
        tr_offset = (hdistance*2, 2*vstep)
        center_x = total_width / 2
        cl_offset = (center_x - dim, vdistance)
        cr_offset = (center_x, vdistance)
        bl_offset = (center_x - dim, vdistance + dim)
        br_offset = (center_x, vdistance + dim)
        nimg = create_new(total_width, total_height)
        cycle_seq = (green, green, green, red, red, red, red)
        for args in izip(cycle_seq, (tl_offset, tc_offset, tr_offset, cl_offset, cr_offset, bl_offset, br_offset), cycle_seq):
            nimg.paste(*args)
        return nimg
    def _make_8():
        blue = fetch_region(im, cfg['b']['blue'])
        dim = max(blue.size)
        hdistance = dim
        vdistance = int(dim*0.9)
        total_width = dim*2
        total_height = dim + vdistance*3
        nimg = create_new(total_width, total_height)
        for x, y in product((0, 1), (0, 1, 2, 3)):
            nimg.paste(blue, (x*dim, y*vdistance), blue)
        return nimg
    def _make_9():
        blue = fetch_region(im, cfg['b']['blue'])
        green = fetch_region(im, cfg['b']['green'])
        red =fetch_region(im, cfg['b']['red'])
        dim = max(max(max(blue.size), max(green.size)), max(red.size))
        hdistance = int(dim*0.9)
        vdistance = int(dim*1.2)
        total_width = hdistance*2 + dim
        total_height = dim + vdistance*2
        nimg = create_new(total_width, total_height)
        for x, y in product((0, 1, 2), (0, 1, 2)):
            if y == 0:
                nimg.paste(blue, (x*hdistance, y*vdistance), blue)
            elif y == 1:
                nimg.paste(red, (x*hdistance, y*vdistance), red)
            else:
                nimg.paste(green, (x*hdistance, y*vdistance), green)
        return nimg
    if enum == 1:
        return _make_1()
    elif enum == 2:
        return _make_2()
    elif enum == 3:
        return _make_3()
    elif enum == 4:
        return _make_4()
    elif enum == 5:
        return _make_5()
    elif enum == 6:
        return _make_6()
    elif enum == 7:
        return _make_7()
    elif enum == 8:
        return _make_8()
    else:
        return _make_9()

    
def make_tiao_char_img(im, cfg, enum):
    large_green_width = cfg['t']['large_green'].width()
    large_green_height = cfg['t']['large_green'].height()
    large_red_width = cfg['t']['large_red'].width()
    large_red_height = cfg['t']['large_red'].height()
    small_green_width = cfg['t']['small_green'].width()
    small_green_height = cfg['t']['small_green'].height()
    small_red_width = cfg['t']['small_red'].width()
    small_red_height = cfg['t']['small_red'].height()
    small_blue_width = cfg['t']['small_blue'].width()
    small_blue_height = cfg['t']['small_blue'].height()
    panel_region = cfg['tile']['front_panel']
    panel_width = panel_region.width()
    def _make_1t():
        r = fetch_region(im, cfg['t']['1t'])
        nimg = create_new(*r.size)
        nimg.paste(r, (0, 0))
        return nimg
    def _make_2t():
        r = fetch_region(im, cfg['t']['large_green'])
        nimg = create_new(r.size[0], r.size[1]*2)
        nimg.paste(r, (0, 0))
        nimg.paste(r, (0, r.size[1]))
        return nimg
    def _make_3t():
        #low_distance = int(large_green_width * 1.2)
        low_distance = int(panel_width / 2 * 0.7)
        total_width = large_green_width*2 + low_distance
        total_height = large_green_height * 2
        center_x = total_width / 2
        top_offset = (center_x - large_green_width / 2, 0)
        bl_offset = (0, large_green_height)
        br_offset = (large_green_width + low_distance, large_green_height)
        r = fetch_region(im, cfg['t']['large_green'])
        nimg = create_new(total_width, total_height)
        nimg.paste(r, top_offset)
        nimg.paste(r, bl_offset)
        nimg.paste(r, br_offset)
        return nimg
    def _make_4t():
        #distance = int(large_green_width*0.8)
        distance = int(panel_width / 2 * 0.6)
        total_width = large_green_width*2 + distance
        total_height = large_green_height*2
        center_x = (total_width - 1) / 2
        tl_offset = (0, 0)
        tr_offset = (large_green_width + distance, 0)
        bl_offset = (0, large_green_height)
        br_offset = (large_green_width + distance, large_green_height)
        r = fetch_region(im, cfg['t']['large_green'])
        nimg = create_new(total_width, total_height)
        nimg.paste(r, tl_offset)
        nimg.paste(r, tr_offset)
        nimg.paste(r, bl_offset)
        nimg.paste(r, br_offset)
        return nimg
    def _make_5t():
        # distance = int(large_green_width*1.2)
        distance = int(panel_width / 2 * 0.65)
        total_width = distance * 2 + large_green_width
        total_height = large_green_height*2
        tl_offset = (0, 0)
        tr_offset = (distance*2, 0)
        bl_offset = (0, large_green_height)
        br_offset = (distance*2, large_green_height)
        center_x = total_width / 2
        center_offset = (center_x - large_red_width / 2, large_green_height / 2)
        rgreen = fetch_region(im, cfg['t']['large_green'])
        rred = fetch_region(im, cfg['t']['large_red'])
        nimg = create_new(total_width, total_height)
        nimg.paste(rgreen, tl_offset)
        nimg.paste(rgreen, tr_offset)
        nimg.paste(rgreen, bl_offset)
        nimg.paste(rgreen, br_offset)
        nimg.paste(rred, center_offset)
        return nimg
    def _make_6t():
        #distance = int(large_green_width*1.2)
        distance = int(panel_width / 2 * 0.7)
        total_width = distance * 2 + large_green_width
        total_height = large_green_height*2
        tl_offset = (0, 0)
        tr_offset = (distance*2, 0)
        bl_offset = (0, large_green_height)
        br_offset = (distance*2, large_green_height)
        center_x = total_width / 2
        tc_offset = (center_x - large_green_width / 2, 0)
        bc_offset = (center_x - large_green_width / 2, large_green_height)
        rgreen = fetch_region(im, cfg['t']['large_green'])
        nimg = create_new(total_width, total_height)
        for args in izip(repeat(rgreen), (tl_offset, tr_offset, bl_offset, br_offset, tc_offset, bc_offset),
                         repeat(rgreen)):
            apply(nimg.paste, args)
        return nimg
    def _make_7t():
        # distance = int(small_green_height * 0.618)
        distance = int(panel_width / 2.0 * 0.65)
        total_width = distance*2 + small_green_width
        total_height = max(small_green_height, small_blue_height)*2 + small_red_height
        center_x = total_width / 2
        tc_offset = (distance, 0)
        cl_offset = (0, small_red_height)
        # cc_offset = (center_x - small_blue_width / 2, small_red_height)
        cc_offset = (distance, small_red_height)
        cr_offset = (distance*2, small_red_height)
        bl_offset = (0, max(small_green_height, small_blue_height) + small_red_height)
        bc_offset = (distance, max(small_green_height, small_blue_height) + small_red_height)
        br_offset = (distance*2, max(small_green_height, small_blue_height) + small_red_height)
        rgreen = fetch_region(im, cfg['t']['small_green'])
        rred = fetch_region(im, cfg['t']['small_red'])
        rblue = fetch_region(im, cfg['t']['small_blue'])
        nimg = create_new(total_width, total_height)
        nimg.paste(rred, tc_offset, rred)
        for args in izip(repeat(rgreen), (cl_offset, cr_offset, bl_offset, br_offset), repeat(rgreen)):
            nimg.paste(*args)
        for args in izip(repeat(rblue), (cc_offset, bc_offset), repeat(rblue)):
            nimg.paste(*args)
        return nimg
    def _make_8t():
        total_width = cfg['t']['8t'].width()
        total_height = cfg['t']['8t'].height()*2
        r = fetch_region(im, cfg['t']['8t'])
        nimg = create_new(total_width, total_height)
        nimg.paste(r, (0, 0), r)
        nimg.paste(r.transpose(Image.FLIP_TOP_BOTTOM), (0, cfg['t']['8t'].height()),
                   r.transpose(Image.FLIP_TOP_BOTTOM))
        return nimg
    def _make_9t():
        # distance = int(small_green_height * 0.618)
        distance = int(panel_width / 2.0 * 0.618)
        total_width = distance*2 + small_green_width
        vdistance = max(small_green_height, small_red_height)
        total_height = vdistance*3
        center_x = total_width / 2
        tl_offset = (0, 0)
        tc_offset = (distance, 0)
        tr_offset = (distance*2, 0)
        cl_offset = (0, vdistance)
        cc_offset = (distance, vdistance)
        cr_offset = (distance*2, vdistance)
        bl_offset = (0, vdistance*2)
        bc_offset = (distance, vdistance*2)
        br_offset = (distance*2, vdistance*2)
        rgreen = fetch_region(im, cfg['t']['small_green'])
        rred = fetch_region(im, cfg['t']['small_red'])
        nimg = create_new(total_width, total_height)
        for args in izip(repeat(rgreen), (tl_offset, tr_offset, cl_offset, cr_offset, bl_offset, br_offset), repeat(rgreen)):
            nimg.paste(*args)
        for args in izip(repeat(rred), (tc_offset, cc_offset, bc_offset), repeat(rred)):
            nimg.paste(*args)
        return nimg
    if enum == 1:
        return _make_1t()
    elif enum == 2:
        return _make_2t()
    elif enum == 3:
        return _make_3t()
    elif enum == 4:
        return _make_4t()
    elif enum == 5:
        return _make_5t()
    elif enum == 6:
        return _make_6t()
    elif enum == 7:
        return _make_7t()
    elif enum == 8:
        return _make_8t()
    else:
        return _make_9t()

def make_honor_char_img(im, honor_region):
    r = fetch_region(im, honor_region)
    return r

def fetch_region(im, region):
    r = im.crop(region.region_tuple())
    return r

def laydown_transform(trans=None):
    def _t(img):
        if trans:
            return trans(img).transpose(Image.FLIP_TOP_BOTTOM)
        else:
            return img.transpose(Image.FLIP_TOP_BOTTOM)
    return _t

def make_all_wan(im, cfg, tile_transform=None):
    for i in xrange(1, 10):
        front_tile = create_new_front_tile(im, cfg)
        wan_img = make_wan_char_img(im, cfg, i)
        out_tile = paste_onto_front_tile(cfg, wan_img, front_tile, tile_transform)
        out_tile.save('{1}/w{0}.png'.format(i, cfg['dir']), 'PNG')
        front_tile = create_new_front_tile(im, cfg)
        out_tile = paste_onto_front_tile(cfg, wan_img, front_tile, laydown_transform(tile_transform))
        out_tile.save('{1}/w{0}ld.png'.format(i, cfg['dir']), 'PNG')

def make_all_tiao(im, cfg, tile_transform=None):
    for i in xrange(1, 10):
        front_tile = create_new_front_tile(im, cfg)
        tiao_img = make_tiao_char_img(im, cfg, i)
        out_tile = paste_onto_front_tile(cfg, tiao_img, front_tile, tile_transform)
        out_tile.save('{1}/t{0}.png'.format(i, cfg['dir']), 'PNG')
        front_tile = create_new_front_tile(im, cfg)
        out_tile = paste_onto_front_tile(cfg, tiao_img, front_tile, laydown_transform(tile_transform))
        out_tile.save('{1}/t{0}ld.png'.format(i, cfg['dir']), 'PNG')
        
def make_all_bing(im, cfg, tile_transform=None):
    for i in xrange(1, 10):
        front_tile = create_new_front_tile(im, cfg)
        bing_img = make_bing_char_img(im, cfg, i)
        out_tile = paste_onto_front_tile(cfg, bing_img, front_tile, tile_transform)
        out_tile.save('{1}/b{0}.png'.format(i, cfg['dir']), 'PNG')
        front_tile = create_new_front_tile(im, cfg)
        out_tile = paste_onto_front_tile(cfg, bing_img, front_tile, laydown_transform(tile_transform))
        out_tile.save('{1}/b{0}ld.png'.format(i, cfg['dir']), 'PNG')

def make_all_honor(im, cfg, tile_transform=None):
    for honor, honor_region in cfg['honor'].iteritems():
        front_tile = create_new_front_tile(im, cfg)
        honor_img = make_honor_char_img(im, honor_region)
        out_tile = paste_onto_front_tile(cfg, honor_img, front_tile, tile_transform)
        out_tile.save('{1}/{0}.png'.format(honor, cfg['dir']), 'PNG')
        front_tile = create_new_front_tile(im, cfg)
        out_tile = paste_onto_front_tile(cfg, honor_img, front_tile, laydown_transform(tile_transform))
        out_tile.save('{1}/{0}ld.png'.format(honor, cfg['dir']), 'PNG')

def mk_outdir(cfg):
    if not os.path.exists(cfg['dir']):
        os.makedirs(cfg['dir'])
        
def make_large_img(src):
    mk_outdir(max_size_cfg)
    im = Image.open(src)
    make_all_wan(im, max_size_cfg)
    make_all_tiao(im, max_size_cfg)
    make_all_bing(im, max_size_cfg)
    make_all_honor(im, max_size_cfg)

def make_medium_img(src):
    mk_outdir(medium_size_cfg)
    im = Image.open(src)
    make_all_wan(im, medium_size_cfg)
    make_all_tiao(im, medium_size_cfg)
    make_all_bing(im, medium_size_cfg)
    make_all_honor(im, medium_size_cfg)
    back_region = fetch_region(im, medium_size_cfg['tile']['back'])
    back_region.save('{1}/{0}.png'.format('back', medium_size_cfg['dir']), 'PNG')
    
def make_small_img(src):
    mk_outdir(small_size_cfg)
    im = Image.open(src)
    def flip_tile(tile):
        return tile.transpose(Image.FLIP_TOP_BOTTOM)
    make_all_wan(im, small_size_cfg, flip_tile)
    make_all_tiao(im, small_size_cfg, flip_tile)
    make_all_bing(im, small_size_cfg, flip_tile)
    make_all_honor(im, small_size_cfg, flip_tile)
    back_region = fetch_region(im, small_size_cfg['tile']['back'])
    back_region.save('{1}/{0}.png'.format('back', small_size_cfg['dir']), 'PNG')
    
if __name__ == '__main__':
    make_large_img('img_20.png')
    make_medium_img('img_20.png')
    make_small_img('img_20.png')
