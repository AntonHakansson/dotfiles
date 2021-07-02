/* See LICENSE file for copyright and license details. */

/* appearance */
static const unsigned int borderpx  = 1;        /* border pixel of windows */
static const unsigned int snap      = 32;       /* snap pixel */
static const unsigned int gappih    = 20;       /* horiz inner gap between windows */
static const unsigned int gappiv    = 10;       /* vert inner gap between windows */
static const unsigned int gappoh    = 10;       /* horiz outer gap between windows and screen edge */
static const unsigned int gappov    = 30;       /* vert outer gap between windows and screen edge */
static       int smartgaps          = 0;        /* 1 means no outer gap when there is only one window */
static const int showbar            = 1;        /* 0 means no bar */
static const int topbar             = 1;        /* 0 means bottom bar */
static const char *fonts[]          = { "monospace:size=10" };
static const char dmenufont[]       = "monospace:size=10";
static const char col_gray1[]       = "#222222";
static const char col_gray2[]       = "#444444";
static const char col_gray3[]       = "#bbbbbb";
static const char col_gray4[]       = "#eeeeee";
static const char col_cyan[]        = "#005577";
static const char *colors[][3]      = {
    /*               fg         bg         border   */
    [SchemeNorm] = { col_gray3, col_gray1, "#181a23" },
    [SchemeSel]  = { col_gray4, col_cyan,  "#bd93f9" },
};

/* tagging */
static const char *tags[] = { "1", "2", "3", "4", "5", "6", "7", "8", "9" };

static const Rule rules[] = {
    /* xprop(1):
     *  WM_CLASS(STRING) = instance, class
     *  WM_NAME(STRING) = title
     */
    /* class      instance    title       tags mask     isfloating   monitor */
    { "Gimp",     NULL,       NULL,       0,            1,           -1 },
};

/* layout(s) */
static const float mfact     = 0.6;  /* factor of master area size [0.05..0.95] */
static const int nmaster     = 1;    /* number of clients in master area */
static const int resizehints = 1;    /* 1 means respect size hints in tiled resizals */

#define FORCE_VSPLIT 1  /* nrowgrid layout: force two clients to always split vertically */
#include "vanitygaps.c"

static const Layout layouts[] = {
    /* symbol     arrange function */
    { "[]=",      tile },    /* first entry isodefault */
    { "[M]",      monocle },
    { "[@]",      spiral },
    { "[\\]",     dwindle },
    { "H[]",      deck },
    { "TTT",      bstack },
    { "===",      bstackhoriz },
    { "HHH",      grid },
    { "###",      nrowgrid },
    { "---",      horizgrid },
    { ":::",      gaplessgrid },
    { "|M|",      centeredmaster },
    { ">M>",      centeredfloatingmaster },
    { "><>",      NULL },    /* no layout function means floating behavior */
    { NULL,       NULL },
};

/* key definitions */
#define MODKEY Mod4Mask
#define TAGKEYS(KEY,TAG) \
    { MODKEY,                       KEY,      view,           {.ui = 1 << TAG} }, \
    { MODKEY|ControlMask,           KEY,      toggleview,     {.ui = 1 << TAG} }, \
    { MODKEY|ShiftMask,             KEY,      tag,            {.ui = 1 << TAG} }, \
    { MODKEY|ControlMask|ShiftMask, KEY,      toggletag,      {.ui = 1 << TAG} },

/* commands */
static char dmenumon[2] = "0"; /* component of dmenucmd, manipulated in spawn() */

static const char *dmenucmd[]   = { "/etc/profiles/per-user/hakanssn/bin/rofi -show drun -modi drun,run -show-icons -theme ~/.config/rofi/theme/appmenu.rasi", NULL };
static const char *termcmd[]    = { "alacritty", NULL };
static const char *emacscmd[]   = { "emacsclient --create-frame --alternate-editor=\"emacs\" --no-wait", NULL };
static const char *browsercmd[] = { "firefox", NULL };

static Key keys[] = {
    /* modifier                     key        function        argument */

    // Spawning windows
    { MODKEY,                       XK_d,      spawn,          {.v = dmenucmd } },
    { MODKEY,                       XK_Return, spawn,          {.v = termcmd } },
    { MODKEY,                       XK_e,      spawn,          {.v = emacscmd } },
    { MODKEY,                       XK_w,      spawn,          {.v = browsercmd } },

    // Window control
    { MODKEY,                       XK_j,      focusstack,     {.i = +1 } },
    { MODKEY,                       XK_k,      focusstack,     {.i = -1 } },
    { MODKEY,                       XK_h,      setmfact,       {.f = -0.05} },
    { MODKEY,                       XK_l,      setmfact,       {.f = +0.05} },
    { MODKEY,                       XK_q,      killclient,     {0} },

    // Layout control
    { MODKEY,                       XK_Tab,    view,           {0} },
    { MODKEY,                       XK_space,  zoom,           {0} },
    { MODKEY|ShiftMask,             XK_space,  togglefloating, {0} },
    { MODKEY,                       XK_t,      setlayout,      {.v = &layouts[0]} },    /* tile */
    { MODKEY|ShiftMask,             XK_t,      setlayout,      {.v = &layouts[1]} },    /* bstack */
    { MODKEY,                       XK_y,      setlayout,      {.v = &layouts[2]} },    /* spiral */
    { MODKEY|ShiftMask,             XK_y,      setlayout,      {.v = &layouts[3]} },    /* dwindle */
    { MODKEY,                       XK_u,      setlayout,      {.v = &layouts[4]} },    /* deck */
    { MODKEY|ShiftMask,             XK_u,      setlayout,      {.v = &layouts[5]} },    /* monocle */
    { MODKEY,                       XK_i,      setlayout,      {.v = &layouts[6]} },    /* centeredmaster */
    { MODKEY|ShiftMask,             XK_i,      setlayout,      {.v = &layouts[7]} },    /* centeredfloatingmaster */
    { MODKEY,                       XK_o,      incnmaster,     {.i = +1 } },
    { MODKEY|ShiftMask,             XK_o,      incnmaster,     {.i = -1 } },
    { MODKEY,                       XK_a,      togglegaps,     {0} },
    { MODKEY|ShiftMask,             XK_a,      defaultgaps,    {0} },
    { MODKEY,                       XK_s,      togglesticky,   {0} },
    { MODKEY,                       XK_f,      togglefullscr,  {0} },
    { MODKEY|ShiftMask,             XK_f,      setlayout,      {.v = &layouts[8] } },
    { MODKEY,                       XK_z,      incrgaps,       {.i = +3 } },
    { MODKEY|ShiftMask,             XK_z,      incrgaps,       {.i = -3 } },

	// Bar control
    { MODKEY,                       XK_b,      togglebar,      {0} },

    { MODKEY,                       XK_0,      view,           {.ui = ~0 } },
    { MODKEY|ShiftMask,             XK_0,      tag,            {.ui = ~0 } },
    { MODKEY,                       XK_comma,  focusmon,       {.i = -1 } },
    { MODKEY,                       XK_period, focusmon,       {.i = +1 } },
    /* { MODKEY|ShiftMask,             XK_comma,  tagmon,         {.i = -1 } }, */
    /* { MODKEY|ShiftMask,             XK_period, tagmon,         {.i = +1 } }, */
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
};

/* button definitions */
/* click can be ClkTagBar, ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin */
static Button buttons[] = {
    /* click                event mask      button          function        argument */
    { ClkLtSymbol,          0,              Button1,        setlayout,      {0} },
    { ClkLtSymbol,          0,              Button3,        setlayout,      {.v = &layouts[2]} },
    { ClkWinTitle,          0,              Button2,        zoom,           {0} },
    { ClkStatusText,        0,              Button2,        spawn,          {.v = termcmd } },
    { ClkClientWin,         MODKEY,         Button1,        movemouse,      {0} },
    { ClkClientWin,         MODKEY,         Button2,        togglefloating, {0} },
    { ClkClientWin,         MODKEY,         Button3,        resizemouse,    {0} },
    { ClkTagBar,            0,              Button1,        view,           {0} },
    { ClkTagBar,            0,              Button3,        toggleview,     {0} },
    { ClkTagBar,            MODKEY,         Button1,        tag,            {0} },
    { ClkTagBar,            MODKEY,         Button3,        toggletag,      {0} },
};

