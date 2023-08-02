#ifndef __FLASH_H
#define __FLASH_H

#include "common.h"

char *get_flash_path();
long get_flash_size();

void init_flash(const char *flash_bin);
#endif // __FLASH_H
