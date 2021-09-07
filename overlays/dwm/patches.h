/*
 * This file contains patch control flags.
 *
 * In principle you should be able to mix and match any patches
 * you may want. In cases where patches are logically incompatible
 * one patch may take precedence over the other as noted in the
 * relevant descriptions.
 *
 * Although layouts typically come as patches they are differentiated
 * here for grouping purposes.
 *
 * Documentation: https://github.com/bakkeby/dwm-flexipatch/blob/master/patches.def.h
 */

#define NOBORDER_PATCH 1 // removes border when there is only one window visible.
#define NODMENU_PATCH 1
#define PERTAG_PATCH 1
#define RESIZEPOINT_PATCH 1
#define STICKY_PATCH 1
#define SWALLOW_PATCH 1
#define TOGGLEFULLSCREEN_PATCH 1
#define VANITYGAPS_PATCH 1
#define WARP_PATCH 1
#define XRDB_PATCH 1

#define BAR_DWMBLOCKS_PATCH 1
#define BAR_LTSYMBOL_PATCH 1
#define BAR_STATUSCMD_PATCH 1
#define BAR_STATUS_PATCH 1
#define BAR_TAGS_PATCH 1
#define BAR_TITLE_LEFT_PAD_PATCH 1
#define BAR_WINTITLE_PATCH 1

#define BSTACK_LAYOUT 1
#define CENTEREDMASTER_LAYOUT 1
#define TILE_LAYOUT 1
