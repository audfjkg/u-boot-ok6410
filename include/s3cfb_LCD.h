#ifndef S3C6410_LCD_H
#define S3C6410_LCD_H


#include "s3cfb-RegLCD.h"

#define CFG_HIGH  1
#define CFG_LOW   0

//1376 //1178
#define S3CFB_HFP		2	/* front porch */
#define S3CFB_HSW		41	/* hsync width */
#define S3CFB_HBP		2	/* back porch */
//805 //807
#define S3CFB_VFP		2	/* front porch */
#define S3CFB_VSW		10	/* vsync width */
#define S3CFB_VBP		2	/* back porch */

#define S3CFB_HRES		480	/* horizon pixel  x resolition */
#define S3CFB_VRES		272	/* line cnt       y resolution */
#define S3CFB_VFRAME_FREQ     	60	/* frame rate freq */
#define PIXELBITS               16

#define S3CFB_IVCLK             CFG_LOW
#define S3CFB_IHSYNC            CFG_HIGH
#define S3CFB_IVSYNC            CFG_HIGH
#define S3CFB_IVDEN             CFG_LOW


#define S3CFB_HRES_VIRTUAL	(S3CFB_HRES)	/* horizon pixel  x resolition */
#define S3CFB_VRES_VIRTUAL	(S3CFB_VRES*2)	/* line cnt       y resolution */

#define S3CFB_HRES_OSD		(S3CFB_HRES)	/* horizon pixel  x resolition */
#define S3CFB_VRES_OSD		(S3CFB_VRES)	/* line cnt       y resolution */
#define S3CFB_PIXEL_CLOCK	(S3CFB_VFRAME_FREQ * (S3CFB_HFP + S3CFB_HSW + S3CFB_HBP + S3CFB_HRES) * (S3CFB_VFP + S3CFB_VSW + S3CFB_VBP + S3CFB_VRES))
//malloc和uboot最多只能分配512K空间
#define LCD_FRAMEBUFFER         (CONFIG_SYS_TEXT_BASE - 0x300000)          

#endif
