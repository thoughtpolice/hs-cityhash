#ifndef _HS_CITY_H_
#define _HS_CITY_H_

extern "C" {

uint64_t hs_CityHash64(const char* buf, size_t len);
void hs_CityHash128(const char* buf, size_t len, uint64_t* lo, uint64_t* hi);

}

#endif
