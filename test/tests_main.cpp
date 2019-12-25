#include <gtest/gtest.h>

#include "racket/racket.hpp"
#include "util/env.hpp"

#include <scheme.h>

namespace {
  struct main_data {
    int    argc;
    char** argv;
  };
}

static int
do_main(void* data) {
  main_data* d = reinterpret_cast<main_data*>(data);
  ::testing::InitGoogleTest(&d->argc, d->argv);
  return RUN_ALL_TESTS();
}

int
main(int argc, char** argv) {
  game::env_init(argc, argv);
  main_data d{argc, argv};
  return game::rkt::setup(&d, do_main);
}
