#include "city.h"
#include "hs_city.h"

uint64_t hs_CityHash64(const char* buf, size_t len) {
  return CityHash64(buf,len);
}

uint64_t hs_CityHash64WithSeed(const char *buf, size_t len, uint64_t seed) {
  return CityHash64WithSeed(buf,len,seed);
}

uint64_t hs_CityHash64WithSeeds(const char *buf, size_t len,
                                uint64 seed0, uint64 seed1) {
  return CityHash64WithSeeds(buf,len,seed0,seed1);
}

void hs_CityHash128(const char* buf, size_t len, uint64_t* lo, uint64_t* hi) {
  uint128 res = CityHash128(buf,len);
  *lo = Uint128Low64(res);
  *hi = Uint128High64(res);
}

void hs_CityHash128WithSeed(const char* buf, size_t len, uint64_t in1, uint64_t in2,
                            uint64_t* lo, uint64_t* hi) {
  uint128 seed;
  seed.first = in1; seed.second = in2;

  uint128 r = CityHash128WithSeed(buf, len, seed);
  *lo = Uint128Low64(r);
  *hi = Uint128High64(r);
}
