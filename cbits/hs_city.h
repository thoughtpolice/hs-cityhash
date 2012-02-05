#ifndef _HS_CITY_H_
#define _HS_CITY_H_

extern "C" {

uint64_t hs_CityHash64(const char* buf, size_t len);
uint64_t hs_CityHash64WithSeed(const char *buf, size_t len, uint64_t seed);
uint64_t hs_CityHash64WithSeeds(const char *buf, size_t len,
                                uint64 seed0, uint64 seed1);

void hs_CityHash128(const char* buf, size_t len, uint64_t* lo, uint64_t* hi);
void hs_CityHash128WithSeed(const char* buf, size_t len, uint64_t in1, uint64_t in2,
                            uint64_t* lo, uint64_t* hi);
}

#endif
