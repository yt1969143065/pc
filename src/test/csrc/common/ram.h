#ifndef __RAM_H
#define __RAM_H

#include "common.h"

void* get_ram_start();
long get_ram_size();
void* get_img_start();
long get_img_size();
void init_ram(const char *img);
void ram_finish();
#endif
