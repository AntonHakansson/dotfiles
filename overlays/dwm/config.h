/* Reference: https://github.com/bakkeby/dwm-flexipatch/blob/master/config.def.h */

#include <X11/XF86keysym.h>
#include "patches.h"

/* appearance */
static const unsigned int borderpx       = 1;   /* border pixel of windows */
static const unsigned int snap           = 32;  /* snap pixel */
static const unsigned int gappih         = 20;  /* horiz inner gap between windows */
static const unsigned int gappiv         = 10;  /* vert inner gap between windows */
static const unsigned int gappoh         = 10;  /* horiz outer gap between windows and screen edge */
static const unsigned int gappov         = 30;  /* vert outer gap between windows and screen edge */
static const int smartgaps_fact          = 1;   /* gap factor when there is only one client; 0 = no gaps, 3 = 3x outer gaps */
static const int showbar                 = 1;   /* 0 means no bar */
static const int topbar                  = 1;   /* 0 means bottom bar */

/* Indicators: see patch/bar_indicators.h for options */
static int tagindicatortype              = INDICATOR_TOP_LEFT_SQUARE;
static int tiledindicatortype            = INDICATOR_NONE;
static int floatindicatortype            = INDICATOR_TOP_LEFT_SQUARE;
static const char *fonts[]               = { "Fira Code:size=10" };

static char c000000[]                    = "#000000"; // placeholder value

static char normfgcolor[]                = "#bbbbbb";
static char normbgcolor[]                = "#222222";
static char normbordercolor[]            = "#444444";
static char normfloatcolor[]             = "#db8fd9";

static char selfgcolor[]                 = "#eeeeee";
static char selbgcolor[]                 = "#005577";
static char selbordercolor[]             = "#005577";
static char selfloatcolor[]              = "#005577";

static char titlenormfgcolor[]           = "#bbbbbb";
static char titlenormbgcolor[]           = "#222222";
static char titlenormbordercolor[]       = "#444444";
static char titlenormfloatcolor[]        = "#db8fd9";

static char titleselfgcolor[]            = "#eeeeee";
static char titleselbgcolor[]            = "#005577";
static char titleselbordercolor[]        = "#005577";
static char titleselfloatcolor[]         = "#005577";

static char tagsnormfgcolor[]            = "#bbbbbb";
static char tagsnormbgcolor[]            = "#222222";
static char tagsnormbordercolor[]        = "#444444";
static char tagsnormfloatcolor[]         = "#db8fd9";

static char tagsselfgcolor[]             = "#eeeeee";
static char tagsselbgcolor[]             = "#005577";
static char tagsselbordercolor[]         = "#005577";
static char tagsselfloatcolor[]          = "#005577";

static char hidnormfgcolor[]             = "#005577";
static char hidselfgcolor[]              = "#227799";
static char hidnormbgcolor[]             = "#222222";
static char hidselbgcolor[]              = "#222222";

static char urgfgcolor[]                 = "#bbbbbb";
static char urgbgcolor[]                 = "#222222";
static char urgbordercolor[]             = "#ff0000";
static char urgfloatcolor[]              = "#db8fd9";

static char *colors[][ColCount] = {
    /*                       fg                bg                border                float */
    [SchemeNorm]         = { normfgcolor,      normbgcolor,      normbordercolor,      normfloatcolor },
    [SchemeSel]          = { selfgcolor,       selbgcolor,       selbordercolor,       selfloatcolor },
    [SchemeTitleNorm]    = { titlenormfgcolor, titlenormbgcolor, titlenormbordercolor, titlenormfloatcolor },
    [SchemeTitleSel]     = { titleselfgcolor,  titleselbgcolor,  titleselbordercolor,  titleselfloatcolor },
    [SchemeTagsNorm]     = { tagsnormfgcolor,  tagsnormbgcolor,  tagsnormbordercolor,  tagsnormfloatcolor },
    [SchemeTagsSel]      = { tagsselfgcolor,   tagsselbgcolor,   tagsselbordercolor,   tagsselfloatcolor },
    [SchemeHidNorm]      = { hidnormfgcolor,   hidnormbgcolor,   c000000,              c000000 },
    [SchemeHidSel]       = { hidselfgcolor,    hidselbgcolor,    c000000,              c000000 },
    [SchemeUrg]          = { urgfgcolor,       urgbgcolor,       urgbordercolor,       urgfloatcolor },
};

#if BAR_ALPHA_PATCH
static const unsigned int baralpha = 0xd0;
static const unsigned int borderalpha = OPAQUE;
static const unsigned int alphas[][3] = {
    /*                       fg      bg        border     */
    [SchemeNorm]         = { OPAQUE, baralpha, borderalpha },
    [SchemeSel]          = { OPAQUE, baralpha, borderalpha },
    [SchemeTitleNorm]    = { OPAQUE, baralpha, borderalpha },
    [SchemeTitleSel]     = { OPAQUE, baralpha, borderalpha },
    [SchemeTagsNorm]     = { OPAQUE, baralpha, borderalpha },
    [SchemeTagsSel]      = { OPAQUE, baralpha, borderalpha },
    [SchemeHidNorm]      = { OPAQUE, baralpha, borderalpha },
    [SchemeHidSel]       = { OPAQUE, baralpha, borderalpha },
    [SchemeUrg]          = { OPAQUE, baralpha, borderalpha },
};
#endif // BAR_ALPHA_PATCH


static const int swallowfloating         = 0;   /* 1 means swallow floating windows by default */

/* Tags
 * In a traditional dwm the number of tags in use can be changed simply by changing the number
 * of strings in the tags array. This build does things a bit different which has some added
 * benefits. If you need to change the number of tags here then change the NUMTAGS macro in dwm.c.
 *
 * Examples:
 *
 *  1) static char *tagicons[][NUMTAGS*2] = {
 *         [DEFAULT_TAGS] = { "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F", "G", "H", "I" },
 *     }
 *
 *  2) static char *tagicons[][1] = {
 *         [DEFAULT_TAGS] = { "•" },
 *     }
 *
 * The first example would result in the tags on the first monitor to be 1 through 9, while the
 * tags for the second monitor would be named A through I. A third monitor would start again at
 * 1 through 9 while the tags on a fourth monitor would also be named A through I. Note the tags
 * count of NUMTAGS*2 in the array initialiser which defines how many tag text / icon exists in
 * the array. This can be changed to *3 to add separate icons for a third monitor.
 *
 * For the second example each tag would be represented as a bullet point. Both cases work the
 * same from a technical standpoint - the icon index is derived from the tag index and the monitor
 * index. If the icon index is is greater than the number of tag icons then it will wrap around
 * until it an icon matches. Similarly if there are two tag icons then it would alternate between
 * them. This works seamlessly with alternative tags and alttagsdecoration patches.
 */
static char *tagicons[][NUMTAGS] = {
    [DEFAULT_TAGS]        = { "1", "2", "3", "4", "5", "6", "7", "8", "9" },
    [ALTERNATIVE_TAGS]    = { "A", "B", "C", "D", "E", "F", "G", "H", "I" },
    [ALT_TAGS_DECORATION] = { "<1>", "<2>", "<3>", "<4>", "<5>", "<6>", "<7>", "<8>", "<9>" },
};


/* There are two options when it comes to per-client rules:
 *  - a typical struct table or
 *  - using the RULE macro
 *
 * A traditional struct table looks like this:
 *    // class      instance  title  wintype  tags mask  isfloating  monitor
 *    { "Gimp",     NULL,     NULL,  NULL,    1 << 4,    0,          -1 },
 *    { "Firefox",  NULL,     NULL,  NULL,    1 << 7,    0,          -1 },
 *
 * The RULE macro has the default values set for each field allowing you to only
 * specify the values that are relevant for your rule, e.g.
 *
 *    RULE(.class = "Gimp", .tags = 1 << 4)
 *    RULE(.class = "Firefox", .tags = 1 << 7)
 *
 * Refer to the Rule struct definition for the list of available fields depending on
 * the patches you enable.
 */
static const Rule rules[] = {
    /* xprop(1):
    *   WM_CLASS(STRING) = instance, class
    *   WM_NAME(STRING) = title
    *   WM_WINDOW_ROLE(STRING) = role
    *   _NET_WM_WINDOW_TYPE(ATOM) = wintype
    */
    RULE(.wintype = WTYPE "DIALOG", .isfloating = 1)
    RULE(.wintype = WTYPE "UTILITY", .isfloating = 1)
    RULE(.wintype = WTYPE "TOOLBAR", .isfloating = 1)
    RULE(.wintype = WTYPE "SPLASH", .isfloating = 1)
    RULE(.wintype = WTYPE "SPLASH", .isfloating = 1)
    RULE(.class = "Alacritty", .isterminal = 1)
};


/* Bar rules allow you to configure what is shown where on the bar, as well as
 * introducing your own bar modules.
 *
 *    monitor:
 *      -1  show on all monitors
 *       0  show on monitor 0
 *      'A' show on active monitor (i.e. focused / selected) (or just -1 for active?)
 *    bar - bar index, 0 is default, 1 is extrabar
 *    alignment - how the module is aligned compared to other modules
 *    widthfunc, drawfunc, clickfunc - providing bar module width, draw and click functions
 *    name - does nothing, intended for visual clue and for logging / debugging
 */
static const BarRule barrules[] = {
    /* monitor  bar    alignment         widthfunc                drawfunc                clickfunc                name */
    #if BAR_STATUSBUTTON_PATCH
    { -1,       0,     BAR_ALIGN_LEFT,   width_stbutton,          draw_stbutton,          click_stbutton,          "statusbutton" },
    #endif // BAR_STATUSBUTTON_PATCH
    #if BAR_POWERLINE_TAGS_PATCH
    {  0,       0,     BAR_ALIGN_LEFT,   width_pwrl_tags,         draw_pwrl_tags,         click_pwrl_tags,         "powerline_tags" },
    #endif // BAR_POWERLINE_TAGS_PATCH
    #if BAR_TAGS_PATCH
    { -1,       0,     BAR_ALIGN_LEFT,   width_tags,              draw_tags,              click_tags,              "tags" },
    #endif // BAR_TAGS_PATCH
    #if BAR_TAGGRID_PATCH
    { -1,       0,     BAR_ALIGN_LEFT,   width_taggrid,           draw_taggrid,           click_taggrid,           "taggrid" },
    #endif // BAR_TAGGRID_PATCH
    #if BAR_SYSTRAY_PATCH
    {  0,       0,     BAR_ALIGN_RIGHT,  width_systray,           draw_systray,           click_systray,           "systray" },
    #endif // BAR_SYSTRAY_PATCH
    #if BAR_LTSYMBOL_PATCH
    { -1,       0,     BAR_ALIGN_LEFT,   width_ltsymbol,          draw_ltsymbol,          click_ltsymbol,          "layout" },
    #endif // BAR_LTSYMBOL_PATCH
    #if BAR_STATUS2D_PATCH && BAR_STATUSCMD_PATCH
    { 'A',      0,     BAR_ALIGN_RIGHT,  width_status2d,          draw_status2d,          click_statuscmd,         "status2d" },
    #elif BAR_STATUS2D_PATCH
    { 'A',      0,     BAR_ALIGN_RIGHT,  width_status2d,          draw_status2d,          click_status2d,          "status2d" },
    #elif BAR_POWERLINE_STATUS_PATCH
    {  0,       0,     BAR_ALIGN_RIGHT,  width_pwrl_status,       draw_pwrl_status,       click_pwrl_status,       "powerline_status" },
    #elif BAR_STATUS_PATCH && BAR_STATUSCMD_PATCH
    {  0,       0,     BAR_ALIGN_RIGHT,  width_status,            draw_status,            click_statuscmd,         "status" },
    #elif BAR_STATUS_PATCH
    { 'A',      0,     BAR_ALIGN_RIGHT,  width_status,            draw_status,            click_status,            "status" },
    #endif // BAR_STATUS2D_PATCH | BAR_STATUSCMD_PATCH
    #if XKB_PATCH
    {  0,       0,     BAR_ALIGN_RIGHT,  width_xkb,               draw_xkb,               click_xkb,               "xkb" },
    #endif // XKB_PATCH
    #if BAR_FLEXWINTITLE_PATCH
    { -1,       0,     BAR_ALIGN_NONE,   width_flexwintitle,      draw_flexwintitle,      click_flexwintitle,      "flexwintitle" },
    #elif BAR_TABGROUPS_PATCH
    { -1,       0,     BAR_ALIGN_NONE,   width_bartabgroups,      draw_bartabgroups,      click_bartabgroups,      "bartabgroups" },
    #elif BAR_AWESOMEBAR_PATCH
    { -1,       0,     BAR_ALIGN_NONE,   width_awesomebar,        draw_awesomebar,        click_awesomebar,        "awesomebar" },
    #elif BAR_FANCYBAR_PATCH
    { -1,       0,     BAR_ALIGN_NONE,   width_fancybar,          draw_fancybar,          click_fancybar,          "fancybar" },
    #elif BAR_WINTITLE_PATCH
    { -1,       0,     BAR_ALIGN_NONE,   width_wintitle,          draw_wintitle,          click_wintitle,          "wintitle" },
    #endif // BAR_TABGROUPS_PATCH | BAR_AWESOMEBAR_PATCH | BAR_FANCYBAR_PATCH | BAR_WINTITLE_PATCH
    #if BAR_EXTRASTATUS_PATCH
    #if BAR_STATUS2D_PATCH && BAR_STATUSCMD_PATCH
    { 'A',      1,     BAR_ALIGN_CENTER, width_status2d_es,       draw_status2d_es,       click_statuscmd_es,      "status2d_es" },
    #elif BAR_STATUS2D_PATCH
    { 'A',      1,     BAR_ALIGN_CENTER, width_status2d_es,       draw_status2d_es,       click_status2d,          "status2d_es" },
    #elif BAR_POWERLINE_STATUS_PATCH
    {  0,       1,     BAR_ALIGN_RIGHT,  width_pwrl_status_es,    draw_pwrl_status_es,    click_pwrl_status,       "powerline_status" },
    #elif BAR_STATUSCMD_PATCH && BAR_STATUS_PATCH
    { 'A',      1,     BAR_ALIGN_CENTER, width_status_es,         draw_status_es,         click_statuscmd_es,      "status_es" },
    #elif BAR_STATUS_PATCH
    { 'A',      1,     BAR_ALIGN_CENTER, width_status_es,         draw_status_es,         click_status,            "status_es" },
    #endif // BAR_STATUS2D_PATCH | BAR_STATUSCMD_PATCH
    #endif // BAR_EXTRASTATUS_PATCH
    #if BAR_FLEXWINTITLE_PATCH
    #if BAR_WINTITLE_HIDDEN_PATCH
    { -1,       1,  BAR_ALIGN_RIGHT_RIGHT, width_wintitle_hidden, draw_wintitle_hidden,   click_wintitle_hidden,   "wintitle_hidden" },
    #endif
    #if BAR_WINTITLE_FLOATING_PATCH
    { -1,       1,     BAR_ALIGN_LEFT,   width_wintitle_floating, draw_wintitle_floating, click_wintitle_floating, "wintitle_floating" },
    #endif // BAR_WINTITLE_FLOATING_PATCH
    #endif // BAR_FLEXWINTITLE_PATCH
};


/* layout(s) */
static const float mfact     = 0.55; /* factor of master area size [0.05..0.95] */
static const int nmaster     = 1;    /* number of clients in master area */
static const int resizehints = 0;    /* 1 means respect size hints in tiled resizals */

#if NROWGRID_LAYOUT
#define FORCE_VSPLIT 1
#endif

static const Layout layouts[] = {
    /* symbol     arrange function */
    #if TILE_LAYOUT
    { "[]=",      tile },    /* first entry is default */
    #endif
    { "><>",      NULL },    /* no layout function means floating behavior */
    #if MONOCLE_LAYOUT
    { "[M]",      monocle },
    #endif
    #if BSTACK_LAYOUT
    { "TTT",      bstack },
    #endif
    #if BSTACKHORIZ_LAYOUT
    { "===",      bstackhoriz },
    #endif
    #if CENTEREDMASTER_LAYOUT
    { "|M|",      centeredmaster },
    #endif
    #if CENTEREDFLOATINGMASTER_LAYOUT
    { ">M>",      centeredfloatingmaster },
    #endif
    #if COLUMNS_LAYOUT
    { "|||",      col },
    #endif
    #if DECK_LAYOUT
    { "[D]",      deck },
    #endif
    #if FIBONACCI_SPIRAL_LAYOUT
    { "(@)",      spiral },
    #endif
    #if FIBONACCI_DWINDLE_LAYOUT
    { "[\\]",     dwindle },
    #endif
    #if GRIDMODE_LAYOUT
    { "HHH",      grid },
    #endif
    #if HORIZGRID_LAYOUT
    { "---",      horizgrid },
    #endif
    #if GAPPLESSGRID_LAYOUT
    { ":::",      gaplessgrid },
    #endif
    #if NROWGRID_LAYOUT
    { "###",      nrowgrid },
    #endif
    #if CYCLELAYOUTS_PATCH
    { NULL,       NULL },
    #endif
};

#if BAR_STATUSCMD_PATCH
#if BAR_DWMBLOCKS_PATCH
/* This defines the name of the executable that handles the bar (used for signalling purposes) */
#define STATUSBAR "dwmblocks"
#else
/* commands spawned when clicking statusbar, the mouse button pressed is exported as BUTTON */
static const StatusCmd statuscmds[] = {
	{ "notify-send Volume$BUTTON", 1 },
	{ "notify-send CPU$BUTTON", 2 },
	{ "notify-send Battery$BUTTON", 3 },
};
/* test the above with: xsetroot -name "$(printf '\x01Volume |\x02 CPU |\x03 Battery')" */
static const char *statuscmd[] = { "/bin/sh", "-c", NULL, NULL };
#endif // BAR_DWMBLOCKS_PATCH
#endif // BAR_STATUSCMD_PATCH

/* key definitions */
#define MODKEY Mod4Mask
#define TAGKEYS(KEY,TAG) \
    { MODKEY,                       KEY,      view,           {.ui = 1 << TAG} }, \
    { MODKEY|ControlMask,           KEY,      toggleview,     {.ui = 1 << TAG} }, \
    { MODKEY|ShiftMask,             KEY,      tag,            {.ui = 1 << TAG} }, \
    { MODKEY|ControlMask|ShiftMask, KEY,      toggletag,      {.ui = 1 << TAG} },

/* commands */
static const char *dmenucmd[]   = { "rofi", "-show", "drun", "-modi", "drun,run", "-show-icons", "-theme", "~/.config/rofi/theme/appmenu.rasi", NULL };
static const char *termcmd[]    = { "alacritty", NULL };
static const char *emacscmd[]   = { "emacsclient", "--create-frame", "--alternate-editor=\"emacs\"", "--no-wait", NULL };
static const char *browsercmd[] = { "firefox", NULL };
static const char *screenshotcmd[] = { "flameshot", "gui", NULL };
static const char *mutecmd[]    = { "pactl", "set-sink-mute",   "@DEFAULT_SINK@", "toggle", NULL };
static const char *volupcmd[]   = { "pactl", "set-sink-volume", "@DEFAULT_SINK@", "+5%", NULL };
static const char *voldowncmd[] = { "pactl", "set-sink-volume", "@DEFAULT_SINK@", "-5%", NULL };

static Key keys[] = {
    /* modifier                     key        function        argument */

    // Spawning windows
    { MODKEY,                       XK_d,      spawn,          {.v = dmenucmd } },
    { MODKEY,                       XK_Return, spawn,          {.v = termcmd } },
    { MODKEY,                       XK_e,      spawn,          {.v = emacscmd } },
    { MODKEY,                       XK_w,      spawn,          {.v = browsercmd } },
    { MODKEY,                       XK_p,      spawn,          {.v = screenshotcmd } },

    // Window control
    { MODKEY,                       XK_j,      focusstack,     {.i = +1 } },
    { MODKEY,                       XK_k,      focusstack,     {.i = -1 } },
    { MODKEY,                       XK_h,      setmfact,       {.f = -0.05} },
    { MODKEY,                       XK_l,      setmfact,       {.f = +0.05} },
    { MODKEY,                       XK_q,      killclient,     {0} },
    { MODKEY|ShiftMask,             XK_s,      togglesticky,   {0} },

    // Layout control
    { MODKEY,                       XK_Tab,    view,           {0} },
    { MODKEY,                       XK_space,  zoom,           {0} },
    { MODKEY|ShiftMask,             XK_space,  togglefloating, {0} },
    { MODKEY,                       XK_t,      setlayout,      {.v = &layouts[0]} },
    { MODKEY,                       XK_y,      setlayout,      {.v = &layouts[1]} },
    { MODKEY,                       XK_u,      setlayout,      {.v = &layouts[2]} },
    { MODKEY|ShiftMask,             XK_u,      setlayout,      {.v = &layouts[3]} },
    { MODKEY,                       XK_o,      incnmaster,     {.i = +1 } },
    { MODKEY|ShiftMask,             XK_o,      incnmaster,     {.i = -1 } },
    { MODKEY,                       XK_a,      togglegaps,     {0} },
    { MODKEY|ShiftMask,             XK_a,      defaultgaps,    {0} },
    { MODKEY,                       XK_f,      togglefullscreen,  {0} },
    { MODKEY|ShiftMask,             XK_f,      setlayout,      {.v = &layouts[8] } },
    { MODKEY,                       XK_z,      incrgaps,       {.i = +3 } },
    { MODKEY|ShiftMask,             XK_z,      incrgaps,       {.i = -3 } },

    // Bar control
    { MODKEY,                       XK_b,      togglebar,      {0} },

    { MODKEY,                       XK_0,      view,           {.ui = ~0 } },
    { MODKEY|ShiftMask,             XK_0,      tag,            {.ui = ~0 } },
    { MODKEY,                       XK_comma,  focusmon,       {.i = -1 } },
    { MODKEY,                       XK_period, focusmon,       {.i = +1 } },
    { MODKEY|ShiftMask,             XK_comma,  tagmon,         {.i = -1 } },
    { MODKEY|ShiftMask,             XK_period, tagmon,         {.i = +1 } },
    { MODKEY|ShiftMask,             XK_q,      quit,           {0} },
    TAGKEYS(                        XK_1,                      0)
    TAGKEYS(                        XK_2,                      1)
    TAGKEYS(                        XK_3,                      2)
    TAGKEYS(                        XK_4,                      3)
    TAGKEYS(                        XK_5,                      4)
    TAGKEYS(                        XK_6,                      5)
    TAGKEYS(                        XK_7,                      6)
    TAGKEYS(                        XK_8,                      7)
    TAGKEYS(                        XK_9,                      8)

    // Media keys
    { 0,      XF86XK_AudioMute,        spawn, {.v = mutecmd }    },
    { 0,      XF86XK_AudioLowerVolume, spawn, {.v = voldowncmd } },
    { 0,      XF86XK_AudioRaiseVolume, spawn, {.v = volupcmd }   },
    { MODKEY, XK_minus,                spawn, {.v = voldowncmd } },
    { MODKEY, XK_equal,                spawn, {.v = volupcmd }   },

    // Colors
    { MODKEY|ShiftMask,             XK_F5,         xrdb,                   {.v = NULL } },
};

/* button definitions */
/* click can be ClkTagBar, ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin */
static Button buttons[] = {
    /* click                event mask      button          function        argument */
    { ClkLtSymbol,          0,              Button1,        setlayout,      {0} },
    { ClkLtSymbol,          0,              Button3,        setlayout,      {.v = &layouts[2]} },
    { ClkWinTitle,          0,              Button2,        zoom,           {0} },
    { ClkStatusText,        0,              Button1,        sigstatusbar,   {.i = 1 } },
    { ClkStatusText,        0,              Button2,        sigstatusbar,   {.i = 2 } },
    { ClkStatusText,        0,              Button3,        sigstatusbar,   {.i = 3 } },
    { ClkClientWin,         MODKEY,         Button1,        movemouse,      {0} },
    { ClkClientWin,         MODKEY,         Button2,        togglefloating, {0} },
    { ClkClientWin,         MODKEY,         Button3,        resizemouse,    {0} },
    { ClkTagBar,            0,              Button1,        view,           {0} },
    { ClkTagBar,            0,              Button3,        toggleview,     {0} },
    { ClkTagBar,            MODKEY,         Button1,        tag,            {0} },
    { ClkTagBar,            MODKEY,         Button3,        toggletag,      {0} },
};
