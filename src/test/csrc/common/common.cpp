#include "common.h"

int assert_count = 0;
static pthread_mutex_t assert_mutex;

void assert_init() {
  pthread_mutex_init(&assert_mutex, 0);
}

void assert_finish() {
   pthread_mutex_destroy(&assert_mutex);
}
