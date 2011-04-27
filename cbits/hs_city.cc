#include "city.h"
#include "hs_city.h"

uint64_t hs_CityHash64(const char* buf, size_t len) {
  return CityHash64(buf,len);
}

void hs_CityHash128(const char* buf, size_t len, uint64_t* lo, uint64_t* hi) {
  uint128 res = CityHash128(buf,len);
  *lo = Uint128Low64(res);
  *hi = Uint128High64(res);
}
