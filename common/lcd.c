/*
 * Common LCD routines for supported CPUs
 *
 * (C) Copyright 2001-2002
 * Wolfgang Denk, DENX Software Engineering -- wd@denx.de
 *
 * See file CREDITS for list of people who contributed to this
 * project.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 * MA 02111-1307 USA
 */

/************************************************************************/
/* ** HEADER FILES							*/
/************************************************************************/

/* #define DEBUG */

#include <config.h>
#include <common.h>
#include <command.h>
#include <version.h>
#include <stdarg.h>
#include <linux/types.h>
#include <stdio_dev.h>
#if defined(CONFIG_POST)
#include <post.h>
#endif
#include <lcd.h>
#include <watchdog.h>
#include <asm/io.h>
//#include <8xx_immap.h>

#if defined(CONFIG_CPU_PXA25X) || defined(CONFIG_CPU_PXA27X) || \
	defined(CONFIG_CPU_MONAHANS)
#define CONFIG_CPU_PXA
#include <asm/byteorder.h>
#endif

#if defined(CONFIG_MPC823)
#include <lcdvideo.h>
#endif

#if defined(CONFIG_ATMEL_LCD)
#include <atmel_lcdc.h>
#endif

/************************************************************************/
/* ** FONT DATA								*/
/************************************************************************/
#include <video_font.h>		/* Get font data, width and height	*/
#include <video_font_data.h>

/************************************************************************/
/* ** LOGO DATA								*/
/************************************************************************/
#ifdef CONFIG_LCD_LOGO
# include <bmp_logo.h>		/* Get logo data, width and height	*/
# include <bmp_logo_data.h>
# if (CONSOLE_COLOR_WHITE >= BMP_LOGO_OFFSET) && (LCD_BPP != LCD_COLOR16)
#  error Default Color Map overlaps with Logo Color Map
# endif
#endif

DECLARE_GLOBAL_DATA_PTR;

ulong lcd_setmem (ulong addr);

static void lcd_drawchars(ushort x, ushort y, uchar *str, int count);
static inline void lcd_puts_xy(ushort x, ushort y, uchar *s);
static inline void lcd_putc_xy(ushort x, ushort y, uchar  c);

static int lcd_init(void *lcdbase);

static void *lcd_logo (void);

static int lcd_getbgcolor(void);
static void lcd_setfgcolor(int color);
static void lcd_setbgcolor(int color);

char lcd_is_enabled = 0;
int lcd_line_length;
int lcd_color_fg;
int lcd_color_bg;
/*
 * Frame buffer memory information
 */
void *lcd_base;
void *lcd_console_address;

short console_col;
short console_row;
struct vidinfo panel_info;

void lcd_ctrl_init(void *lcdbase);
void lcd_enable(void);
void lcd_disable(void);
ulong calc_fbsize(void);
void lcd_setcolreg(ushort regno,
		ushort red,ushort green,ushort blue);

#ifdef	NOT_USED_SO_FAR
static void lcd_getcolreg(ushort regno,
		ushort *red, ushort *green, ushort *blue);
static int lcd_getfgcolor(void);
#endif	/* NOT_USED_SO_FAR */

vidinfo_t panel_info = {
	S3CFB_HRES,
	S3CFB_VRES,
	0,
	0,
	4,
};
/************************************************************************/

/*----------------------------------------------------------------------*/

static void console_scrollup(void)
{
	/* Copy up rows ignoring the first one */
	memcpy(CONSOLE_ROW_FIRST, CONSOLE_ROW_SECOND, CONSOLE_SCROLL_SIZE);

	/* Clear the last one */
	memset(CONSOLE_ROW_LAST, COLOR_MASK(lcd_color_bg), CONSOLE_ROW_SIZE);
}

/*----------------------------------------------------------------------*/

static inline void console_back(void)
{
	if (--console_col < 0) {
		console_col = CONSOLE_COLS-1 ;
		if (--console_row < 0) {
			console_row = 0;
		}
	}

	lcd_putc_xy(console_col * VIDEO_FONT_WIDTH,
			console_row * VIDEO_FONT_HEIGHT, ' ');
}

/*----------------------------------------------------------------------*/

static inline void console_newline(void)
{
	++console_row;
	console_col = 0;

	/* Check if we need to scroll the terminal */
	if (console_row >= CONSOLE_ROWS) {
		/* Scroll everything up */
		console_scrollup();
		--console_row;
	}
}

/*----------------------------------------------------------------------*/

void lcd_putc(const char c)
{
	if (!lcd_is_enabled) {
		serial_putc(c);

		return;
	}

	switch (c) {
		case '\r':
			console_col = 0;

			return;
		case '\n':
			console_newline();

			return;
		case '\t':	/* Tab (8 chars alignment) */
			console_col +=  8;
			console_col &= ~7;

			if (console_col >= CONSOLE_COLS)
				console_newline();

			return;
		case '\b':
			console_back();

			return;
		default:
			lcd_putc_xy(console_col * VIDEO_FONT_WIDTH,
					console_row * VIDEO_FONT_HEIGHT, c);
			if (++console_col >= CONSOLE_COLS)
				console_newline();
	}
}

/*----------------------------------------------------------------------*/

void lcd_puts(const char *s)
{
	if (!lcd_is_enabled) {
		serial_puts(s);

		return;
	}

	while (*s) {
		lcd_putc(*s++);
	}
}

/*----------------------------------------------------------------------*/

void lcd_printf(const char *fmt, ...)
{
	va_list args;
	char buf[CONFIG_SYS_PBSIZE];

	va_start(args, fmt);
	vsprintf(buf, fmt, args);
	va_end(args);

	lcd_puts(buf);
}

/************************************************************************/
/* ** Low-Level Graphics Routines					*/
/************************************************************************/

static void lcd_drawchars(ushort x, ushort y, uchar *str, int count)
{
	uchar *dest;
	ushort row;

#if defined(CONFIG_LCD_LOGO) && !defined(CONFIG_LCD_INFO_BELOW_LOGO)
	y += BMP_LOGO_HEIGHT;
#endif

#if LCD_BPP == LCD_MONOCHROME
	ushort off  = x * (1 << LCD_BPP) % 8;
#endif

	dest = (uchar *)(lcd_base + y * lcd_line_length + x * (1 << LCD_BPP) / 8);

	for (row = 0; row < VIDEO_FONT_HEIGHT; ++row, dest += lcd_line_length) {
		uchar *s = str;
		int i;
#if LCD_BPP == LCD_COLOR16
		ushort *d = (ushort *)dest;
#else
		uchar *d = dest;
#endif

#if LCD_BPP == LCD_MONOCHROME
		uchar rest = *d & -(1 << (8-off));
		uchar sym;
#endif
		for (i = 0; i < count; ++i) {
			uchar c, bits;

			c = *s++;
			bits = video_fontdata[c * VIDEO_FONT_HEIGHT + row];

#if LCD_BPP == LCD_MONOCHROME
			sym  = (COLOR_MASK(lcd_color_fg) & bits) |
				(COLOR_MASK(lcd_color_bg) & ~bits);

			*d++ = rest | (sym >> off);
			rest = sym << (8-off);
#elif LCD_BPP == LCD_COLOR8
			for (c = 0; c < 8; ++c) {
				*d++ = (bits & 0x80) ?
					lcd_color_fg : lcd_color_bg;
				bits <<= 1;
			}
#elif LCD_BPP == LCD_COLOR16
			for (c = 0; c < 8; ++c) {
				*d++ = (bits & 0x80) ?
					lcd_color_fg : lcd_color_bg;
				bits <<= 1;
			}
#endif
		}
#if LCD_BPP == LCD_MONOCHROME
		*d  = rest | (*d & ((1 << (8-off)) - 1));
#endif
	}
}

/*----------------------------------------------------------------------*/

static inline void lcd_puts_xy(ushort x, ushort y, uchar *s)
{
	lcd_drawchars(x, y, s, strlen((char *)s));
}

/*----------------------------------------------------------------------*/

static inline void lcd_putc_xy(ushort x, ushort y, uchar c)
{
	lcd_drawchars(x, y, &c, 1);
}

/************************************************************************/
/**  Small utility to check that you got the colours right		*/
/************************************************************************/
#ifdef LCD_TEST_PATTERN

#define	N_BLK_VERT	2
#define	N_BLK_HOR	3

static int test_colors[N_BLK_HOR*N_BLK_VERT] = {
	CONSOLE_COLOR_RED,	CONSOLE_COLOR_GREEN,	CONSOLE_COLOR_YELLOW,
	CONSOLE_COLOR_BLUE,	CONSOLE_COLOR_MAGENTA,	CONSOLE_COLOR_CYAN,
};

static void test_pattern(void)
{
	ushort v_max  = panel_info.vl_row;
	ushort h_max  = panel_info.vl_col;
	ushort v_step = (v_max + N_BLK_VERT - 1) / N_BLK_VERT;
	ushort h_step = (h_max + N_BLK_HOR  - 1) / N_BLK_HOR;
	ushort v, h;
	uchar *pix = (uchar *)lcd_base;

	printf("[LCD] Test Pattern: %d x %d [%d x %d]\n",
			h_max, v_max, h_step, v_step);

	/* WARNING: Code silently assumes 8bit/pixel */
	for (v = 0; v < v_max; ++v) {
		uchar iy = v / v_step;
		for (h = 0; h < h_max; ++h) {
			uchar ix = N_BLK_HOR * iy + (h/h_step);
			*pix++ = test_colors[ix];
		}
	}
}
#endif /* LCD_TEST_PATTERN */


/************************************************************************/
/* ** GENERIC Initialization Routines					*/
/************************************************************************/

int drv_lcd_init (void)
{
	struct stdio_dev lcddev;
	int rc;

	lcd_base = (void *)(gd->fb_base);

	lcd_line_length = (panel_info.vl_col * NBITS (panel_info.vl_bpix)) / 8;

	lcd_init(lcd_base);		/* LCD initialization */

	/* Device initialization */
	memset(&lcddev, 0, sizeof(lcddev));

	strcpy(lcddev.name, "lcd");
	lcddev.ext   = 0;			/* No extensions */
	lcddev.flags = DEV_FLAGS_OUTPUT;	/* Output only */
	lcddev.putc  = lcd_putc;		/* 'putc' function */
	lcddev.puts  = lcd_puts;		/* 'puts' function */

	rc = stdio_register (&lcddev);

	return (rc == 0) ? 1 : rc;
}

/*----------------------------------------------------------------------*/
	static
int do_lcd_clear(cmd_tbl_t *cmdtp, int flag, int argc, char *const argv[])
{
	lcd_clear();
	return 0;
}

void lcd_clear(void)
{
#if LCD_BPP == LCD_MONOCHROME
	/* Setting the palette */
	lcd_initcolregs();

#elif LCD_BPP == LCD_COLOR8
	/* Setting the palette */
	lcd_setcolreg(CONSOLE_COLOR_BLACK, 0, 0, 0);
	lcd_setcolreg(CONSOLE_COLOR_RED, 0xFF, 0, 0);
	lcd_setcolreg(CONSOLE_COLOR_GREEN, 0, 0xFF, 0);
	lcd_setcolreg(CONSOLE_COLOR_YELLOW, 0xFF, 0xFF, 0);
	lcd_setcolreg(CONSOLE_COLOR_BLUE, 0, 0, 0xFF);
	lcd_setcolreg(CONSOLE_COLOR_MAGENTA, 0xFF, 0, 0xFF);
	lcd_setcolreg(CONSOLE_COLOR_CYAN, 0, 0xFF, 0xFF);
	lcd_setcolreg(CONSOLE_COLOR_GREY, 0xAA, 0xAA, 0xAA);
	lcd_setcolreg(CONSOLE_COLOR_WHITE, 0xFF, 0xFF, 0xFF);
#endif

#ifndef CONFIG_SYS_WHITE_ON_BLACK
	lcd_setfgcolor(CONSOLE_COLOR_BLACK);
	lcd_setbgcolor(CONSOLE_COLOR_WHITE);
#else
	lcd_setfgcolor(CONSOLE_COLOR_WHITE);
	lcd_setbgcolor(CONSOLE_COLOR_BLACK);
#endif	/* CONFIG_SYS_WHITE_ON_BLACK */

#ifdef	LCD_TEST_PATTERN
	test_pattern();
#else
	/* set framebuffer to background color */
	memset((char *)lcd_base,
			COLOR_MASK(lcd_getbgcolor()),
			lcd_line_length*panel_info.vl_row);
#endif
	/* Paint the logo and retrieve LCD base address */
	debug("[LCD] Drawing the logo...\n");
	lcd_console_address = lcd_logo ();

	console_col = 0;
	console_row = 0;
}

U_BOOT_CMD(
		cls,	1,	1,	do_lcd_clear,
		"clear screen",
		""
		);

/*----------------------------------------------------------------------*/

static int lcd_init(void *lcdbase)
{
	/* Initialize the lcd controller */
	debug("[LCD] Initializing LCD frambuffer at %p\n", lcdbase);

	lcd_ctrl_init(lcdbase);
	lcd_is_enabled = 1;
	lcd_clear();
	lcd_enable ();

	/* Initialize the console */
	console_col = 0;
#ifdef CONFIG_LCD_INFO_BELOW_LOGO
	console_row = 7 + BMP_LOGO_HEIGHT / VIDEO_FONT_HEIGHT;
#else
	console_row = 1;	/* leave 1 blank line below logo */
#endif

	return 0;
}


/************************************************************************/
/* ** ROM capable initialization part - needed to reserve FB memory	*/
/************************************************************************/
/*
 * This is called early in the system initialization to grab memory
 * for the LCD controller.
 * Returns new address for monitor, after reserving LCD buffer memory
 *
 * Note that this is running from ROM, so no write access to global data.
 */
ulong lcd_setmem(ulong addr)
{
	ulong size;
	int line_length = (panel_info.vl_col * NBITS(panel_info.vl_bpix)) / 8;

	debug("LCD panel info: %d x %d, %d bit/pix\n", panel_info.vl_col,
			panel_info.vl_row, NBITS(panel_info.vl_bpix));

	size = line_length * panel_info.vl_row;

	/* Round up to nearest full page */
	size = (size + (PAGE_SIZE - 1)) & ~(PAGE_SIZE - 1);

	/* Allocate pages for the frame buffer. */
	addr -= size;

	debug("Reserving %ldk for LCD Framebuffer at: %08lx\n", size>>10, addr);

	return addr;
}

/*----------------------------------------------------------------------*/

static void lcd_setfgcolor(int color)
{
	lcd_color_fg = color;
}

/*----------------------------------------------------------------------*/

static void lcd_setbgcolor(int color)
{
	lcd_color_bg = color;
}

/*----------------------------------------------------------------------*/

#ifdef	NOT_USED_SO_FAR
static int lcd_getfgcolor(void)
{
	return lcd_color_fg;
}
#endif	/* NOT_USED_SO_FAR */

/*----------------------------------------------------------------------*/

static int lcd_getbgcolor(void)
{
	return lcd_color_bg;
}

/*----------------------------------------------------------------------*/
void lcd_disable (void)
{
	S3C_VIDCON0 &= (~(S3C_VIDCON0_ENVID_ENABLE | S3C_VIDCON0_ENVID_F_ENABLE));
}

void lcd_enable (void)
{
	S3C_VIDCON0 |= (S3C_VIDCON0_ENVID_ENABLE | S3C_VIDCON0_ENVID_F_ENABLE);
}


void lcd_panel_disable(void)
{
	MIFPCON_REG |= SEL_BYPASS_MASK;
}

/************************************************************************/
/* ** Chipset depending Bitmap / Logo stuff...                          */
/************************************************************************/
static inline ushort *configuration_get_cmap(void)
{
#if defined CONFIG_CPU_PXA
	struct pxafb_info *fbi = &panel_info.pxa;
	return (ushort *)fbi->palette;
#elif defined(CONFIG_MPC823)
	immap_t *immr = (immap_t *) CONFIG_SYS_IMMR;
	cpm8xx_t *cp = &(immr->im_cpm);
	return (ushort *)&(cp->lcd_cmap[255 * sizeof(ushort)]);
#elif defined(CONFIG_ATMEL_LCD)
	return (ushort *)(panel_info.mmio + ATMEL_LCDC_LUT(0));
#elif !defined(CONFIG_ATMEL_HLCD) && !defined(CONFIG_EXYNOS_FB)
	return panel_info.cmap;
#else
#if defined(CONFIG_LCD_LOGO)
	return bmp_logo_palette;
#else
	return NULL;
#endif
#endif
}

#ifdef CONFIG_LCD_LOGO
void bitmap_plot(int x, int y)
{
#ifdef CONFIG_ATMEL_LCD
	uint *cmap = (uint *)bmp_logo_palette;
#else
	ushort *cmap = (ushort *)bmp_logo_palette;
#endif
	ushort i, j;
	uchar *bmap;
	uchar *fb;
	ushort *fb16;
#if defined(CONFIG_MPC823)
	immap_t *immr = (immap_t *) CONFIG_SYS_IMMR;
	cpm8xx_t *cp = &(immr->im_cpm);
#endif

	debug("Logo: width %d  height %d  colors %d  cmap %d\n",
			BMP_LOGO_WIDTH, BMP_LOGO_HEIGHT, BMP_LOGO_COLORS,
			ARRAY_SIZE(bmp_logo_palette));

	bmap = &bmp_logo_bitmap[0];
	fb   = (uchar *)(lcd_base + y * lcd_line_length + x);

	if (NBITS(panel_info.vl_bpix) < 12) {
		/* Leave room for default color map
		 * default case: generic system with no cmap (most likely 16bpp)
		 * cmap was set to the source palette, so no change is done.
		 * This avoids even more ifdefs in the next stanza
		 */
#if defined(CONFIG_MPC823)
		cmap = (ushort *) &(cp->lcd_cmap[BMP_LOGO_OFFSET * sizeof(ushort)]);
#elif defined(CONFIG_ATMEL_LCD)
		cmap = (uint *)configuration_get_cmap();
#else
		cmap = configuration_get_cmap();
#endif

		WATCHDOG_RESET();

		/* Set color map */
		for (i = 0; i < ARRAY_SIZE(bmp_logo_palette); ++i) {
			ushort colreg = bmp_logo_palette[i];
#ifdef CONFIG_ATMEL_LCD
			uint lut_entry;
#ifdef CONFIG_ATMEL_LCD_BGR555
			lut_entry = ((colreg & 0x000F) << 11) |
				((colreg & 0x00F0) <<  2) |
				((colreg & 0x0F00) >>  7);
#else /* CONFIG_ATMEL_LCD_RGB565 */
			lut_entry = ((colreg & 0x000F) << 1) |
				((colreg & 0x00F0) << 3) |
				((colreg & 0x0F00) << 4);
#endif
			*(cmap + BMP_LOGO_OFFSET) = lut_entry;
			cmap++;
#else /* !CONFIG_ATMEL_LCD */
#ifdef  CONFIG_SYS_INVERT_COLORS
			*cmap++ = 0xffff - colreg;
#else
			*cmap++ = colreg;
#endif
#endif /* CONFIG_ATMEL_LCD */
		}

		WATCHDOG_RESET();

		for (i = 0; i < BMP_LOGO_HEIGHT; ++i) {
			memcpy(fb, bmap, BMP_LOGO_WIDTH);
			bmap += BMP_LOGO_WIDTH;
			fb   += panel_info.vl_col;
		}
	}
	else { /* true color mode */
		u16 col16;
		fb16 = (ushort *)(lcd_base + y * lcd_line_length + x);
		for (i = 0; i < BMP_LOGO_HEIGHT; ++i) {
			for (j = 0; j < BMP_LOGO_WIDTH; j++) {
				col16 = bmp_logo_palette[(bmap[j]-16)];
				fb16[j] =
					((col16 & 0x000F) << 1) |
					((col16 & 0x00F0) << 3) |
					((col16 & 0x0F00) << 4);
			}
			bmap += BMP_LOGO_WIDTH;
			fb16 += panel_info.vl_col;
		}
	}

	WATCHDOG_RESET();
}
#else
static inline void bitmap_plot(int x, int y) {}
#endif /* CONFIG_LCD_LOGO */

/*----------------------------------------------------------------------*/
#if defined(CONFIG_CMD_BMP) || defined(CONFIG_SPLASH_SCREEN)
/*
 * Display the BMP file located at address bmp_image.
 * Only uncompressed.
 */

#ifdef CONFIG_SPLASH_SCREEN_ALIGN
#define BMP_ALIGN_CENTER	0x7FFF

static void splash_align_axis(int *axis, unsigned long panel_size,
		unsigned long picture_size)
{
	unsigned long panel_picture_delta = panel_size - picture_size;
	unsigned long axis_alignment;

	if (*axis == BMP_ALIGN_CENTER)
		axis_alignment = panel_picture_delta / 2;
	else if (*axis < 0)
		axis_alignment = panel_picture_delta + *axis + 1;
	else
		return;

	*axis = max(0, axis_alignment);
}
#endif

#if defined(CONFIG_MPC823) || defined(CONFIG_MCC200)
#define FB_PUT_BYTE(fb, from) *(fb)++ = (255 - *(from)++)
#else
#define FB_PUT_BYTE(fb, from) *(fb)++ = *(from)++
#endif

#if defined(CONFIG_BMP_16BPP)
#if defined(CONFIG_ATMEL_LCD_BGR555)
static inline void fb_put_word(uchar **fb, uchar **from)
{
	*(*fb)++ = (((*from)[0] & 0x1f) << 2) | ((*from)[1] & 0x03);
	*(*fb)++ = ((*from)[0] & 0xe0) | (((*from)[1] & 0x7c) >> 2);
	*from += 2;
}
#else
static inline void fb_put_word(uchar **fb, uchar **from)
{
	*(*fb)++ = *(*from)++;
	*(*fb)++ = *(*from)++;
}
#endif
#endif /* CONFIG_BMP_16BPP */

int lcd_display_bitmap(ulong bmp_image, int x, int y)
{
#if !defined(CONFIG_MCC200)
	ushort *cmap = NULL;
#endif
	ushort *cmap_base = NULL;
	ushort i, j;
	uchar *fb;
	bmp_image_t *bmp=(bmp_image_t *)bmp_image;
	uchar *bmap;
	ushort padded_line;
	unsigned long width, height, byte_width;
	unsigned long pwidth = panel_info.vl_col;
	unsigned colors, bpix, bmp_bpix;

	if (!bmp || !((bmp->header.signature[0] == 'B') &&
				(bmp->header.signature[1] == 'M'))) {
		printf("Error: no valid bmp image at %lx\n", bmp_image);

		return 1;
	}

	width = le32_to_cpu(bmp->header.width);
	height = le32_to_cpu(bmp->header.height);
	bmp_bpix = le16_to_cpu(bmp->header.bit_count);
	colors = 1 << bmp_bpix;

	bpix = NBITS(panel_info.vl_bpix);

	if ((bpix != 1) && (bpix != 8) && (bpix != 16) && (bpix != 32)) {
		printf ("Error: %d bit/pixel mode, but BMP has %d bit/pixel\n",
				bpix, bmp_bpix);

		return 1;
	}

	/* We support displaying 8bpp BMPs on 16bpp LCDs */
	if (bpix != bmp_bpix && (bmp_bpix != 8 || bpix != 16 || bpix != 32)) {
		printf ("Error: %d bit/pixel mode, but BMP has %d bit/pixel\n",
				bpix,
				le16_to_cpu(bmp->header.bit_count));

		return 1;
	}

	debug("Display-bmp: %d x %d  with %d colors\n",
			(int)width, (int)height, (int)colors);

#if !defined(CONFIG_MCC200)
	/* MCC200 LCD doesn't need CMAP, supports 1bpp b&w only */
	if (bmp_bpix == 8) {
		cmap = configuration_get_cmap();
		cmap_base = cmap;

		/* Set color map */
		for (i = 0; i < colors; ++i) {
			bmp_color_table_entry_t cte = bmp->color_table[i];
#if !defined(CONFIG_ATMEL_LCD)
			ushort colreg =
				( ((cte.red)   << 8) & 0xf800) |
				( ((cte.green) << 3) & 0x07e0) |
				( ((cte.blue)  >> 3) & 0x001f) ;
#ifdef CONFIG_SYS_INVERT_COLORS
			*cmap = 0xffff - colreg;
#else
			*cmap = colreg;
#endif
#if defined(CONFIG_MPC823)
			cmap--;
#else
			cmap++;
#endif
#else /* CONFIG_ATMEL_LCD */
			lcd_setcolreg(i, cte.red, cte.green, cte.blue);
#endif
		}
	}
#endif

	/*
	 *  BMP format for Monochrome assumes that the state of a
	 * pixel is described on a per Bit basis, not per Byte.
	 *  So, in case of Monochrome BMP we should align widths
	 * on a byte boundary and convert them from Bit to Byte
	 * units.
	 *  Probably, PXA250 and MPC823 process 1bpp BMP images in
	 * their own ways, so make the converting to be MCC200
	 * specific.
	 */
#if defined(CONFIG_MCC200)
	if (bpix == 1) {
		width = ((width + 7) & ~7) >> 3;
		x     = ((x + 7) & ~7) >> 3;
		pwidth= ((pwidth + 7) & ~7) >> 3;
	}
#endif

	padded_line = (width&0x3) ? ((width&~0x3)+4) : (width);

#ifdef CONFIG_SPLASH_SCREEN_ALIGN
	splash_align_axis(&x, pwidth, width);
	splash_align_axis(&y, panel_info.vl_row, height);
#endif /* CONFIG_SPLASH_SCREEN_ALIGN */

	if ((x + width) > pwidth)
		width = pwidth - x;
	if ((y + height) > panel_info.vl_row)
		height = panel_info.vl_row - y;

	bmap = (uchar *)bmp + le32_to_cpu(bmp->header.data_offset);
	fb   = (uchar *) (lcd_base +
			(y + height - 1) * lcd_line_length + x * bpix / 8);

	switch (bmp_bpix) {
		case 1: /* pass through */
		case 8:
			if (bpix != 16)
				byte_width = width;
			else
				byte_width = width * 2;

			for (i = 0; i < height; ++i) {
				WATCHDOG_RESET();
				for (j = 0; j < width; j++) {
					if (bpix != 16) {
						FB_PUT_BYTE(fb, bmap);
					} else {
						*(uint16_t *)fb = cmap_base[*(bmap++)];
						fb += sizeof(uint16_t) / sizeof(*fb);
					}
				}
				bmap += (width - padded_line);
				fb   -= (byte_width + lcd_line_length);
			}
			break;

#if defined(CONFIG_BMP_16BPP)
		case 16:
			for (i = 0; i < height; ++i) {
				WATCHDOG_RESET();
				for (j = 0; j < width; j++)
					fb_put_word(&fb, &bmap);

				bmap += (padded_line - width) * 2;
				fb   -= (width * 2 + lcd_line_length);
			}
			break;
#endif /* CONFIG_BMP_16BPP */

#if defined(CONFIG_BMP_32BPP)
		case 32:
			for (i = 0; i < height; ++i) {
				for (j = 0; j < width; j++) {
					*(fb++) = *(bmap++);
					*(fb++) = *(bmap++);
					*(fb++) = *(bmap++);
					*(fb++) = *(bmap++);
				}
				fb  -= (lcd_line_length + width * (bpix / 8));
			}
			break;
#endif /* CONFIG_BMP_32BPP */
		default:
			break;
	};

	return 0;
}
#endif

static void *lcd_logo(void)
{
#ifdef CONFIG_SPLASH_SCREEN
	char *s;
	ulong addr;
	static int do_splash = 1;

	if (do_splash && (s = getenv("splashimage")) != NULL) {
		int x = 0, y = 0;
		do_splash = 0;

		addr = simple_strtoul (s, NULL, 16);
#ifdef CONFIG_SPLASH_SCREEN_ALIGN
		s = getenv("splashpos");
		if (s != NULL) {
			if (s[0] == 'm')
				x = BMP_ALIGN_CENTER;
			else
				x = simple_strtol(s, NULL, 0);

			s = strchr(s + 1, ',');
			if (s != NULL) {
				if (s[1] == 'm')
					y = BMP_ALIGN_CENTER;
				else
					y = simple_strtol (s + 1, NULL, 0);
			}
		}
#endif /* CONFIG_SPLASH_SCREEN_ALIGN */

		if (bmp_display(addr, x, y) == 0)
			return (void *)lcd_base;
	}
#endif /* CONFIG_SPLASH_SCREEN */

	bitmap_plot(0, 0);

#ifdef CONFIG_LCD_INFO
	console_col = LCD_INFO_X / VIDEO_FONT_WIDTH;
	console_row = LCD_INFO_Y / VIDEO_FONT_HEIGHT;
	lcd_show_board_info();
#endif /* CONFIG_LCD_INFO */

#if defined(CONFIG_LCD_LOGO) && !defined(CONFIG_LCD_INFO_BELOW_LOGO)
	return (void *)((ulong)lcd_base + BMP_LOGO_HEIGHT * lcd_line_length);
#else
	return (void *)lcd_base;
#endif /* CONFIG_LCD_LOGO && !CONFIG_LCD_INFO_BELOW_LOGO */
}


ulong calc_fbsize (void)
{
	ulong size;

	int line_length = (panel_info.vl_col * NBITS(panel_info.vl_bpix)) / 8;
	size = line_length * panel_info.vl_row;
	//#ifdef LCD_FRAMEBUFFER
	//  size = 4096;
	//#endif

	return size;
}

/************************************************************************/
/*   根据给定的显示缓冲，初始化lcd                                           */
/*   uboot使用内存有限，只能开辟一个基本窗口                                  */
void lcd_ctrl_init(void *lcdbase)
{
	ulong freq_lcdclk;
	ulong freq_Hclk;
	ulong fb_size;
	unsigned char nn;
	//unsigned short *pp;
	//int i;

	printf("********************************************************\n");
	printf("initial lcd controller\n");
	//配置管脚
	GPICON_REG = 0xaaaaaaaa;
	GPIPUD_REG = 0xaaaaaaaa;
	GPJCON_REG = 0xaaaaaaaa;
	GPJPUD_REG = 0xaaaaaaaa;

	lcd_disable();
	S3C_WINCON0 &= (~(S3C_WINCONx_ENWIN_F_ENABLE));


	// (1)MOFPCON:SEL_BYPASS[3] value@0x7410800C 必须设置为’0’.
	MIFPCON_REG &= (~SEL_BYPASS_MASK);
	//(2)SPCON:LCD_SEL[1:0]value@0x74F0081A0 必须设置为’00’,使用主机 I/F 类型,或者设置为‘01’ 使用 RGB I/F 类型。
	SPCON_REG &= (~LCD_SEL_MASK);
	SPCON_REG |= (RGB_IF_STYLE_MASK);
	//(3)VIDCON0:配置视频输出格式和显示使能/禁止。

	freq_lcdclk = S3CFB_PIXEL_CLOCK;
	freq_Hclk = get_HCLK();
	nn = (unsigned char)(freq_Hclk / freq_lcdclk) - 1;

	if(freq_lcdclk < freq_Hclk/2)
	{

		S3C_VIDCON0 = S3C_VIDCON0_INTERLACE_F_PROGRESSIVE + S3C_VIDCON0_VIDOUT_RGB_IF + \
					  S3C_VIDCON0_PNRMODE_RGB_P + S3C_VIDCON0_CLKVALUP_ST_FRM + S3C_VIDCON0_CLKVAL_F(nn) + \
					  S3C_VIDCON0_CLKDIR_DIVIDED + S3C_VIDCON0_CLKSEL_F_HCLK; 
	}else
	{
		S3C_VIDCON0 = S3C_VIDCON0_INTERLACE_F_PROGRESSIVE + S3C_VIDCON0_VIDOUT_RGB_IF + \
					  S3C_VIDCON0_PNRMODE_RGB_P + S3C_VIDCON0_CLKVALUP_ST_FRM + S3C_VIDCON0_CLKVAL_F(0) + \
					  S3C_VIDCON0_CLKDIR_DIRECTED  + S3C_VIDCON0_CLKSEL_F_HCLK; 
	}
	printf("  clk_freq:%d MHz,  div_freq:%d ,rea_freq:%d MHz \n",(int)freq_lcdclk/1000000,(int)nn,(int)freq_Hclk/(nn+1)/1000000);
	//(4)VIDCON1:RGB I/F 控制信号。
	nn = 0;

	if(S3CFB_IVCLK)
	{
		nn += S3C_VIDCON1_IVCLK_RISE_EDGE;
	}
	if(S3CFB_IHSYNC)
	{
		nn += S3C_VIDCON1_IHSYNC_INVERT;
	}
	if(S3CFB_IVSYNC)
	{
		nn += S3C_VIDCON1_IVSYNC_INVERT;
	}
	if(S3CFB_IVDEN)
	{
		nn += S3C_VIDCON1_IVDEN_INVERT;
	}
	S3C_VIDCON1 = (unsigned int)nn;
	S3C_VIDCON2 = 0;
	//(5)I80IFCONx:i80 系统 I/F 控制信号。
	//(6)ITUIFCON0:ITU(BT.601)接口控制
	//(7)VIDTCONx:配置视频输出时序和显示尺寸。
	S3C_VIDTCON0 = S3C_VIDTCON0_VBPD(S3CFB_VBP - 1) | S3C_VIDTCON0_VFPD(S3CFB_VFP - 1) | S3C_VIDTCON0_VSPW(S3CFB_VSW - 1);
	S3C_VIDTCON1 = S3C_VIDTCON1_HBPD(S3CFB_HBP - 1) | S3C_VIDTCON1_HFPD(S3CFB_HFP -1) | S3C_VIDTCON1_HSPW(S3CFB_HSW - 1);
	S3C_VIDTCON2 = S3C_VIDTCON2_LINEVAL(S3CFB_VRES - 1) | S3C_VIDTCON2_HOZVAL(S3CFB_HRES - 1);

	printf("\n HBP = %d HFP = %d HSW = %d,Hpixs:%d",S3CFB_HBP,S3CFB_HFP,S3CFB_HSW,S3CFB_HRES);
	printf("\n VBP = %d VFP = %d VSW = %d,Vpixs:%d",S3CFB_VBP,S3CFB_VFP,S3CFB_VSW,S3CFB_VRES);

	//(8)WINCONx:窗口格式设置
	S3C_WINCON0 = S3C_WINCONx_BPPMODE_F_16BPP_565|S3C_WINCONx_BYTSWP_ENABLE;
	S3C_WINCON1 = 0;
	S3C_WINCON2 = 0;
	S3C_WINCON3 = 0;
	S3C_WINCON4 = 0;
	//(9)VIDOSDxA ,VIDOSDxB:窗口位置设置

	S3C_VIDOSD0A = S3C_VIDOSDxA_OSD_LTX_F(0) + S3C_VIDOSDxA_OSD_LTY_F(0);
	S3C_VIDOSD0B = S3C_VIDOSDxB_OSD_RBX_F(S3CFB_HRES) | S3C_VIDOSDxB_OSD_RBY_F(S3CFB_VRES);
	S3C_VIDOSD0C = S3C_VIDOSD0C_OSDSIZE(S3CFB_HRES*S3CFB_VRES);

	S3C_VIDOSD1A = 0;
	S3C_VIDOSD1B = 0;
	S3C_VIDOSD1C = 0;
	S3C_VIDOSD1D = 0;
	S3C_VIDOSD2A = 0;
	S3C_VIDOSD2B = 0;
	S3C_VIDOSD2C = 0;
	S3C_VIDOSD2D = 0;
	S3C_VIDOSD3A = 0;
	S3C_VIDOSD3B = 0;
	S3C_VIDOSD3C = 0;
	S3C_VIDOSD4A = 0;
	S3C_VIDOSD4B = 0;
	S3C_VIDOSD4C = 0;

	//(10)VIDOSDxC:alpha 值设置

	//(11)VIDWxxADDx:源图像地址设置

	fb_size = calc_fbsize();
	//#ifdef LCD_FRAMEBUFFER
	// fb_size =   (panel_info.vl_col * panel_info.vl_bpix) / 8 * panel_info.vl_row;
	//#endif
	// S3C_VIDW00ADD0B0 = (unsigned int)(lcdbase) | S3C_VIDWxxADD0_VBANK_F((unsigned int)lcdbase);
	S3C_VIDW00ADD0B0 = virt_to_phys(lcdbase);
	S3C_VIDW00ADD0B1 = 0;
	S3C_VIDW01ADD0B0 = 0;
	S3C_VIDW01ADD0B1 = 0;
	S3C_VIDW02ADD0 = 0;
	S3C_VIDW03ADD0 = 0;
	S3C_VIDW04ADD0 = 0;

	// S3C_VIDW00ADD1B0 = S3C_VIDWxxADD1_VBASEL_F((unsigned int)(lcdbase) + fb_size);
	S3C_VIDW00ADD1B0 = virt_to_phys((unsigned int)(lcdbase) + fb_size);
	S3C_VIDW00ADD1B1 = 0;
	S3C_VIDW01ADD1B0 = 0;
	S3C_VIDW01ADD1B1 = 0;
	S3C_VIDW02ADD1 = 0;
	S3C_VIDW03ADD1 = 0;
	S3C_VIDW04ADD1 = 0;

	S3C_VIDW00ADD2 = S3C_VIDWxxADD2_OFFSIZE_F(0) | (S3C_VIDWxxADD2_PAGEWIDTH_F(panel_info.vl_col*panel_info.vl_bpix/8));
	S3C_VIDW01ADD2 = 0;
	S3C_VIDW02ADD2 = 0;
	S3C_VIDW03ADD2 = 0;
	S3C_VIDW04ADD2 = 0;
	//(12)WxKEYCONx:色键值寄存器
	//(13)WINxMAP:窗口颜色控制
	//(14)WPALCON:调色板控制寄存器
	//(15)WxPDATAxx:索引窗口调色板数据
	printf("\nFrameBuff:%08x",(unsigned int)lcdbase);
#if 1
	memset(lcdbase,0x55,fb_size);
#else
	pp = lcdbase;
	for(i=0;i< S3CFB_HRES * S3CFB_VRES;i++)
	{
		*pp = 0xf100;
		pp++; 
	}
#endif
	lcd_enable();
	S3C_WINCON0 |= S3C_WINCONx_ENWIN_F_ENABLE;
	printf("\n  LCD initialization Finished. \n");


}

lcd_setcolreg(ushort regno,
		ushort red,ushort green,ushort blue)
{/*
	volatile immap_t *immr = (immap_t *)CFG_IMMR;
	volatile cpm8xx_t *cp = &(immr->im_cpm);
	unsigned short colreg,*cmap_ptr;

	cmap_ptr =(unsigned short *)&cp->lcd_cmap[regno *2];

	colreg = ((red & 0x0F) << 8) |
		((green &0x0F) <<4) |
		(blue & 0x0F);
#ifdef CFG_INVERT_COLORS
	colreg ^=0x0FFF；
#endif
		*cmap_ptr = colreg;
	debug("setcolreg: reg:%2d @ %p:R=%02x G=%02x B=%02x =>%02x%02x\n",
			regno,&(cp->lcd_cmap[regno*2]),
			red,green,blue,
			cp->lcd_cmap[regno*2],cp-lcd_cmap[regno*2]+1);
*/}

/************************************************************************/
/************************************************************************/
