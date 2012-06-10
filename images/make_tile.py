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
           'small_red' : CropRegion(427, 282, 439, 305),
           'small_green' : CropRegion(414, 282, 426, 305),
           'small_blue' : CropRegion(398, 282, 411, 305),
           '8t' : CropRegion(445, 255, 489, 292),
           },
    'tile' :  {
        'front' : CropRegion(157, 301, 229, 435),
        'front_panel' : CropRegion(167, 342, 222, 423),
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

def paste_onto_front_tile(cfg, pic, tile):
    real_center_x, real_center_y = calc_real_front_center(cfg, 0, 0)
    offset = (real_center_x - pic.size[0] // 2, real_center_y - pic.size[1] // 2)
    tile.paste(pic, offset, pic)
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
    def _make_1t():
        pass
    def _make_2t():
        pass
    def _make_3t():
        pass
    def _make_4t():
        pass
    def _make_5t():
        pass
    def _make_6t():
        pass
    def _make_7t():
        pass
    def _make_8t():
        pass
    def _make_9t():
        pass
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
        low_distance = int(large_green_width * 1.2)
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
        distance = int(large_green_width*0.8)
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
        distance = int(large_green_width*1.2)
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
        distance = int(large_green_width*1.2)
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
        distance = int(small_green_width*1.35)
        total_width = distance*2 + small_green_width
        total_height = max(small_green_height, small_blue_height)*2 + small_red_height
        center_x = total_width / 2
        tc_offset = (center_x - small_red_width / 2, 0)
        cl_offset = (0, small_red_height)
        cc_offset = (center_x - small_blue_width / 2, small_red_height)
        cr_offset = (distance*2, small_red_height)
        bl_offset = (0, max(small_green_height, small_blue_height) + small_red_height)
        bc_offset = (center_x - small_blue_width / 2, max(small_green_height, small_blue_height) + small_red_height)
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
        distance = int(small_green_width*1.35)
        total_width = distance*2 + small_green_width
        vdistance = max(small_green_height, small_red_height)
        total_height = vdistance*3
        center_x = total_width / 2
        tl_offset = (0, 0)
        tc_offset = (center_x - small_red_width / 2, 0)
        tr_offset = (distance*2, 0)
        cl_offset = (0, vdistance)
        cc_offset = (center_x - small_red_width / 2, vdistance)
        cr_offset = (distance*2, vdistance)
        bl_offset = (0, vdistance*2)
        bc_offset = (center_x - small_red_width / 2, vdistance*2)
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

def fetch_region(im, region):
    r = im.crop(region.region_tuple())
    return r

def make_all_wan():
    im = Image.open('img_20.png')
    for i in xrange(1, 10):
        front_tile = create_new_front_tile(im, max_size_cfg)
        wan_img = make_wan_char_img(im, max_size_cfg, i)
        out_tile = paste_onto_front_tile(max_size_cfg, wan_img, front_tile)
        out_tile.save('w{0}.png'.format(i), 'PNG')

def test():
    im = Image.open('img_20.png')
    for i in xrange(1, 10):
        front_tile = create_new_front_tile(im, max_size_cfg)
        tiao_img = make_tiao_char_img(im, max_size_cfg, i)
        out_tile = paste_onto_front_tile(max_size_cfg, tiao_img, front_tile)
        out_tile.save('t{0}.png'.format(i), 'PNG')
        
test()
