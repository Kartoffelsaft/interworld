#ifdef BUILD_FOR_TIC
#include "tic80.h"
#else
#include "tic80mock.h"
#ifdef TESTING
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#endif
#endif
#include <math.h>
#include <stddef.h>

#define SECONDS_PER_FRAME (1.0/60)
uint8_t lastButtonInputs = 0; // btnp doesn't seem to be working right

#define PI 3.14159
#define INV_PI 1/PI

#define FWIDTH ((float)WIDTH)
#define FHEIGHT ((float)HEIGHT)

int writeInt(char* buf, int n);
void traceNum(int n) {
    char buf[32] = {0};
    writeInt(buf, n);
    trace(buf, 1);
}

typedef enum {
    ALLOC,
    FREE,
    REALLOC,
    EMPTY,
} MemOperator;
typedef void (*MemFn)(void* info, MemOperator operation, void** ptr, size_t oldSize, size_t newSize);
typedef struct {
    MemFn operate;
    void* info;
} Allocator;

void* aalloc(Allocator a, size_t amount) {
    void* ret = NULL;
    a.operate(a.info, ALLOC, &ret, 0, amount);
    return ret;
}
void afree(Allocator a, void* what, size_t amount) {
    a.operate(a.info, FREE, &what, amount, 0);
}
void arealloc(Allocator a, void** what, size_t oldSize, size_t newSize) {
    a.operate(a.info, REALLOC, what, oldSize, newSize);
}
void aempty(Allocator a) {
    a.operate(a.info, EMPTY, NULL, 0, 0);
}

void* memcpy(void* restrict dest, void const * restrict src, size_t n) {
    for (int i = 0; i < n; i++) *(uint8_t*)(dest++) = *(uint8_t*)(src++);
    return dest-n;
}
size_t strlen(char const * s) {
    size_t n = 0;
    while(*(s++)) n++;
    return n;
}

struct PairTickAllocInfo_s;
struct PairPersistAllocInfo_s;
typedef struct PairTickAllocInfo_s {
    void* head;
    void* start;
    struct PairPersistAllocInfo_s* other;
} PairTickAllocInfo;
typedef struct PairPersistAllocInfo_s {
    void* head;
    void* start;
    struct PairTickAllocInfo_s* other;
} PairPersistAllocInfo;

void pairTickAllocOp(void* info, MemOperator op, void** ptr, size_t oldSize, size_t newSize) {
    PairTickAllocInfo* a = info;
    size_t osAligned = ((oldSize-1)/8 + 1)*8;
    size_t nsAligned = ((newSize-1)/8 + 1)*8;

    switch (op) {
        case ALLOC:
            if (a->head + nsAligned > a->other->head) {
                *ptr = NULL;
            } else {
                *ptr = a->head;
                a->head += nsAligned;
            }
            break;
        case FREE:
            a->head = *ptr;
            break;
        case REALLOC:
            if (*ptr + osAligned == a->head) {
                // TODO: doesn't check if there's enough space but I am lazy right now
                a->head = *ptr + nsAligned;
            } else {
                trace("WARNING: tick allocator can only realloc last alloc", 1);
            }
            break;
        case EMPTY:
            a->head = a->start;
            break;
    }
}

void pairPersistAllocOp(void* info, MemOperator op, void** ptr, size_t oldSize, size_t newSize) {
    PairTickAllocInfo* a = info;
    size_t osAligned = ((oldSize-1)/8 + 1)*8;
    size_t nsAligned = ((newSize-1)/8 + 1)*8;

    switch (op) {
        case ALLOC:
            if (a->head - nsAligned < a->other->head) {
                *ptr = NULL;
            } else {
                a->head -= nsAligned;
                *ptr = a->head;
            }
            break;
        case FREE:
            // TODO: need some info on the mem size to properly do this
            break;
        case REALLOC:
            trace("WARNING: persist allocator doesn't implement realloc", 1);
            break;
        case EMPTY:
            a->head = a->start;
    }
}

void initAllocators(Allocator* tickAllocator, Allocator* persistAllocator) {
    // add some amount to avoid .rodata and .data sections
    // TODO: somehow figure that out automatically
    void* startOfUsableMemory = WASM_FREE_RAM + 0x1000;

    tickAllocator->info = startOfUsableMemory; startOfUsableMemory += sizeof(PairTickAllocInfo);
    *(PairTickAllocInfo*)tickAllocator->info = (PairTickAllocInfo){0};
    tickAllocator->operate = &pairTickAllocOp;

    persistAllocator->info = startOfUsableMemory; startOfUsableMemory += sizeof(PairPersistAllocInfo);
    *(PairPersistAllocInfo*)persistAllocator->info = (PairPersistAllocInfo){0};
    persistAllocator->operate = &pairPersistAllocOp;

    ((PairTickAllocInfo*)tickAllocator->info)->head = startOfUsableMemory;
    ((PairTickAllocInfo*)tickAllocator->info)->start = startOfUsableMemory;
    ((PairTickAllocInfo*)tickAllocator->info)->other = persistAllocator->info;

    ((PairPersistAllocInfo*)persistAllocator->info)->head = WASM_FREE_RAM+WASM_FREE_RAM_SIZE;
    ((PairPersistAllocInfo*)persistAllocator->info)->start = WASM_FREE_RAM+WASM_FREE_RAM_SIZE;
    ((PairPersistAllocInfo*)persistAllocator->info)->other = tickAllocator->info;
}

Allocator tickAllocator;
Allocator persistAllocator;

size_t freeMemory() {
    return ((PairPersistAllocInfo*)persistAllocator.info)->head - ((PairTickAllocInfo*)tickAllocator.info)->head;
}

void persistify(void** oldPtr, size_t size) {
    void* newPtr = aalloc(persistAllocator, size);
    memcpy(newPtr, *oldPtr, size);
    afree(tickAllocator, *oldPtr, size);
    *oldPtr = newPtr;
}

uint32_t rands(uint64_t iseed) {
    static uint64_t seed = 915780157;
    if (iseed != 0) {
        seed = iseed;
    }
    seed = (seed * 22695477 + 1);
    return seed >> 2;
}

typedef struct {
    float x, y;
} Vec2;
typedef struct {
    float x, y, z;
} Vec3;
typedef struct {
    float aa, ab, ba, bb;
} Mtx22;
typedef struct {
    float aa,ab,ac,ba,bb,bc,ca,cb,cc;
} Mtx33;

Vec2 v2Sub(Vec2 lhs, Vec2 rhs) {
    return (Vec2){
        lhs.x - rhs.x,
        lhs.y - rhs.y,
    };
}
Vec2 v2Add(Vec2 lhs, Vec2 rhs) {
    return (Vec2){
        lhs.x + rhs.x,
        lhs.y + rhs.y,
    };
}
Vec2 v2Scale(Vec2 v, float amount) {
    return (Vec2){v.x * amount, v.y * amount};
}
float v2LenSqr(Vec2 v) {
    return v.x*v.x + v.y*v.y;
}
float v2Len(Vec2 v) {
    return sqrtf(v2LenSqr(v));
}
Vec2 v2Norm(Vec2 v) {
    return v2Scale(v, 1/v2Len(v));
}
Vec3 v3Sub(Vec3 lhs, Vec3 rhs) {
    return (Vec3){
        lhs.x - rhs.x,
        lhs.y - rhs.y,
        lhs.z - rhs.z,
    };
}
Vec3 v3Add(Vec3 lhs, Vec3 rhs) {
    return (Vec3){
        lhs.x + rhs.x,
        lhs.y + rhs.y,
        lhs.z + rhs.z,
    };
}
Vec3 v3Scale(Vec3 v, float amount) {
    return (Vec3){v.x * amount, v.y * amount, v.z * amount};
}
Vec3 v3Cross(Vec3 lhs, Vec3 rhs) {
    return (Vec3){
        lhs.y * rhs.z,
        lhs.x * rhs.z,
        lhs.x * rhs.y,
    };
}
float v3LenSqr(Vec3 v) {
    return v.x*v.x + v.y*v.y + v.z*v.z;
}
float v3Len(Vec3 v) {
    return sqrtf(v3LenSqr(v));
}
Vec3 v3Norm(Vec3 v) {
    return v3Scale(v, 1/v3Len(v));
}

Vec3 mtx33MulVec(Mtx33 mtx, Vec3 v) {
    return (Vec3){
        .x = mtx.aa*v.x + mtx.ab*v.y + mtx.ac*v.z,
        .y = mtx.ba*v.x + mtx.bb*v.y + mtx.bc*v.z,
        .z = mtx.ca*v.x + mtx.cb*v.y + mtx.cc*v.z,
    };
}
Mtx33 mtx33Mul(Mtx33 lhs, Mtx33 rhs) {
    return (Mtx33){
        lhs.aa*rhs.aa + lhs.ab*rhs.ba + lhs.ac*rhs.ca, lhs.aa*rhs.ab + lhs.ab*rhs.bb + lhs.ac*rhs.cb, lhs.aa*rhs.ac + lhs.ab*rhs.bc + lhs.ac*rhs.cc,
        lhs.ba*rhs.aa + lhs.bb*rhs.ba + lhs.bc*rhs.ca, lhs.ba*rhs.ab + lhs.bb*rhs.bb + lhs.bc*rhs.cb, lhs.ba*rhs.ac + lhs.bb*rhs.bc + lhs.bc*rhs.cc,
        lhs.ca*rhs.aa + lhs.cb*rhs.ba + lhs.cc*rhs.ca, lhs.ca*rhs.ab + lhs.cb*rhs.bb + lhs.cc*rhs.cb, lhs.ca*rhs.ac + lhs.cb*rhs.bc + lhs.cc*rhs.cc,
    };
}

Mtx33 invUnscaled(Mtx33 mtx) {
    return (Mtx33){
        mtx.bb*mtx.cc - mtx.bc*mtx.cb, mtx.ac*mtx.cb - mtx.ab*mtx.cc, mtx.ab*mtx.bc - mtx.ac*mtx.bb,
        mtx.bc*mtx.ca - mtx.ba*mtx.cc, mtx.aa*mtx.cc - mtx.ac*mtx.ca, mtx.ac*mtx.ba - mtx.aa*mtx.bc,
        mtx.ba*mtx.cb - mtx.bb*mtx.ca, mtx.ab*mtx.ca - mtx.aa*mtx.cb, mtx.aa*mtx.bb - mtx.ba*mtx.ab,
    };
}

Mtx33 mtx33LookAt(Vec3 v) {
    Vec3 zax = v3Norm(v);
    Vec3 xax = v3Norm(v3Cross(v, (Vec3){0, 1, 0}));
    Vec3 yax = v3Cross(zax, xax);
    return (Mtx33){
        xax.x, yax.x, zax.x,
        xax.y, yax.y, zax.y,
        xax.z, yax.z, zax.z,
    };
}

Vec2 mtx22MulVec(Mtx22 mtx, Vec2 v) {
    return (Vec2){
        mtx.aa*v.x + mtx.ab*v.y,
        mtx.ba*v.x + mtx.bb*v.y,
    };
}

Mtx33 rotX(float theta) {
    float s = sinf(theta);
    float c = cosf(theta);
    return (Mtx33) {
        1, 0, 0,
        0, c,-s,
        0, s, c,
    };
}
Mtx33 rotY(float theta) {
    float s = sinf(theta);
    float c = cosf(theta);
    return (Mtx33) {
        c, 0, s,
        0, 1, 0,
       -s, 0, c,
    };
}
Mtx33 rotZ(float theta) {
    float s = sinf(theta);
    float c = cosf(theta);
    return (Mtx33) {
        c,-s, 0,
        s, c, 0,
        0, 0, 1,
    };
}

float sinfa(float);
float cosfa(float);
Mtx33 rotXA(float theta) {
    float s = sinfa(theta);
    float c = cosfa(theta);
    return (Mtx33) {
        1, 0, 0,
        0, c,-s,
        0, s, c,
    };
}
Mtx33 rotYA(float theta) {
    float s = sinfa(theta);
    float c = cosfa(theta);
    return (Mtx33) {
        c, 0, s,
        0, 1, 0,
       -s, 0, c,
    };
}
Mtx33 rotZA(float theta) {
    float s = sinfa(theta);
    float c = cosfa(theta);
    return (Mtx33) {
        c,-s, 0,
        s, c, 0,
        0, 0, 1,
    };
}

#define MATRIX33_IDENTITY (Mtx33){\
        1, 0, 0,\
        0, 1, 0,\
        0, 0, 1,\
    }

/// https://stackoverflow.com/questions/3380628/fast-arc-cos-algorithm/26030435#26030435
float acosf(float x) {
    return (-0.69813170079773212 * x * x - 0.87266462599716477) * x + 1.5707963267948966;
}
float asinf(float x) {
    return PI - acosf(x);
}

float sinfa(float x) {
    x *= 1/(2*PI);
    x -= (int)x;
    if (x < 0.5) return -8 * x * (2*x - 1);
    else return 8 * x * (2*x - 3) + 8;
}
float cosfa(float x) {
    return sinfa(x + PI/2);
}

int writeInt(char* buf, int x) {
    if (x == 0) {
        buf[0] = '0';
        buf[1] = 0;
        return 1;
    }

    if (x < 0) {
        x *= -1;
        *buf = '-';
        buf++;
    }

    char* p;
    for (p = buf; x > 0; p++) {
        int m = x%10;
        x /= 10;
        *p = '0'+m;
    }
    *p = 0;

    int ret = p-buf;

    p--;

    for (char* s = buf; s<p; s++,p--) {
        char tmp = *p;
        *p = *s;
        *s = tmp;
    }

    return ret;
}

int writeFloat(char* buf, float x) {
    char* rest = buf;

    if (x<0) {
        *rest = '-';
        rest++;
        x *= -1;
    }

    int ipart = x;
    float fpart = x-ipart;

    rest += writeInt(rest, ipart);
    *rest = '.';
    rest++;

#define PRECISION 3
    for (int i = 0; i < PRECISION; i++) {
        fpart *= 10;
        *rest = '0'+fpart;
        rest++;
        fpart -= (int)fpart;
    }
    *rest = 0;

    return rest-buf;
}

char* generateName() {
    char* output = aalloc(tickAllocator, 1);
    if (output == NULL) return NULL;
    char* consonants = "QWRTYPSDFGHJKLZXCVBNM";
    size_t consonant_n = strlen(consonants);
    char* vowels = "AEIOUY";
    size_t vowel_n = strlen(vowels);

    for (int i = 0; i < 4; i++) {
        size_t consonants_i = rands(0) % consonant_n;
        size_t vowel_i = rands(0) % vowel_n;
        arealloc(tickAllocator, (void**)&output, i*2+1, i*2+3);
        output[i*2+0] = consonants[consonants_i];
        output[i*2+1] = vowels[vowel_i];
        output[i*2+2] = 0;
    }

    persistify((void**)&output, strlen(output)+1);
    return output;
}

Mtx33 playerRot = MATRIX33_IDENTITY;
Vec3 playerPos = {0, 0, -50};
float playerSpeed = 0;

int8_t menuOpt = 0;
#define MENU_OPT_COUNT 8
uint8_t enabledMenuOpts = 0;
#define MENU_OPTION_DAMPENERS 0x01
#define MENU_OPTION_ORRERY 0x02
#define MENU_OPTION_MAP 0x04
#define MENU_OPTION_DEBUG 0x80

typedef struct {
    bool show;
    bool selected;
    Vec2 screenPos;
    char const * tag;
} DeferredPOI;

#define MAX_DEFERRED_POINTS_OF_INTEREST 4
DeferredPOI deferredPOIs[MAX_DEFERRED_POINTS_OF_INTEREST] = {0};

Vec2 screenShake;

void drawBackHud() {
    uint8_t transparentColor = 0;

    if (enabledMenuOpts & MENU_OPTION_DEBUG) {
        char* buf = aalloc(tickAllocator, 64);

        writeFloat(buf, playerSpeed);
        print(buf, 120, 10, 5, false, 1, false);

        writeFloat(buf, playerPos.x);
        print(buf, 80, 10, 5, false, 1, false);
        writeFloat(buf, playerPos.y);
        print(buf, 80, 20, 5, false, 1, false);
        writeFloat(buf, playerPos.z);
        print(buf, 80, 30, 5, false, 1, false);

        writeInt(buf, freeMemory());
        print(buf, 0, 0, 5, false, 1, false);
        writeFloat(buf, 100.0 * freeMemory() / WASM_FREE_RAM_SIZE);
        print(buf, 0, 10, 5, false, 1, false);
    }

    for (int i = 0; i<MAX_DEFERRED_POINTS_OF_INTEREST; i++) {
        DeferredPOI dp = deferredPOIs[i];
        if (dp.show) {
            if (dp.selected) {
                spr(260, dp.screenPos.x - 8, dp.screenPos.y - 8, &transparentColor, 1, 1, 0, 0, 2, 2);
                line(dp.screenPos.x + 7, dp.screenPos.y, dp.screenPos.x + 16, dp.screenPos.y, 2);
                line(dp.screenPos.x + 16, dp.screenPos.y, dp.screenPos.x + 20, dp.screenPos.y, 10);
                if (dp.tag) print(dp.tag, dp.screenPos.x + 8, dp.screenPos.y + 2, 2, false, 1, true);
            } else {
                spr(274, dp.screenPos.x - 4, dp.screenPos.y - 4, &transparentColor, 1, 1, 0, 0, 1, 1);
                line(dp.screenPos.x + 3, dp.screenPos.y, dp.screenPos.x + 5, dp.screenPos.y, 2);
                line(dp.screenPos.x + 5, dp.screenPos.y, dp.screenPos.x + 7, dp.screenPos.y, 10);
            }
        }
    }
}

void drawFrontHud() {
    uint8_t transparentColor = 0;

    int const start = (WIDTH - 16*MENU_OPT_COUNT)/2;
    if (btn(7)) {
        for (int i = 0; i<MENU_OPT_COUNT; i++) {
            spr(288+2*i, start + i*16 + 2*screenShake.x, 20 + 2*screenShake.y, &transparentColor, 1, 1, false, 0, 2, 2);
            if (enabledMenuOpts & (1<<i)) spr(258, start + i*16 + 4 + 2*screenShake.x, 36 + 2*screenShake.y, &transparentColor, 1, 1, false, 0, 1, 1);
        }
        spr(256, start + menuOpt*16 + 2*screenShake.x, 20 + 2*screenShake.y, &transparentColor, 1, 1, false, 0, 2, 2);
    }

    pix(WIDTH/2, HEIGHT/2, 3);
}

void drawCockpit() {
    uint8_t transparentColor = 0;

    tri(
        -10 + screenShake.x, 20     + screenShake.y, 
        100 + screenShake.x, HEIGHT + screenShake.y, 
        -10 + screenShake.x, 30     + screenShake.y, 

        9
    );

    tri(
        100 + screenShake.x, HEIGHT + screenShake.y, 
        90  + screenShake.x, HEIGHT + screenShake.y, 
        -10 + screenShake.x, 30     + screenShake.y, 

        9
    );
    tri(
        WIDTH + 10 + screenShake.x, 20     + screenShake.y, 
        140        + screenShake.x, HEIGHT + screenShake.y, 
        WIDTH + 10 + screenShake.x, 30     + screenShake.y, 

        9
    );

    tri(
        140        + screenShake.x, HEIGHT + screenShake.y, 
        150        + screenShake.x, HEIGHT + screenShake.y, 
        WIDTH + 10 + screenShake.x, 30     + screenShake.y, 

        9
    );

    tri(
        80       + screenShake.x, HEIGHT-20 + screenShake.y,
        WIDTH-80 + screenShake.x, HEIGHT-20 + screenShake.y,
        -10      + screenShake.x, HEIGHT+10 + screenShake.y,

        1
    );
    tri(
        WIDTH-80 + screenShake.x, HEIGHT-20 + screenShake.y,
        WIDTH+10 + screenShake.x, HEIGHT+10 + screenShake.y,
        -10      + screenShake.x, HEIGHT+10 + screenShake.y,

        1
    );
}

uint64_t noiseMap[] = {
    0x0001000010001100,
    0x0000011000011100,
    0x0011111010021000,
    0x0011211010000001,
    0x1011112211110001,
    0x2211212211211000,
    0x2112122100120011,
    0x0011122000000000,
    0x0011222111001110,
    0x0000112111001100,
    0x0001122100000100,
    0x0010112100001100,
    0x1000111100101111,
    0x0010000000000000,
    0x0022100011122100,
    0x0001010012221100,
};

void drawPlanetSurface(Vec2 pos, float r, Vec2 texOffset, Mtx22 texTransform, Vec3 lightDir) {
    if (r < 4) {
        circ(pos.x, pos.y, r, 13);
        return;
    }

    circ(pos.x, pos.y, r, 5);

    Vec2 lightNorm = v2Norm((Vec2){.x = lightDir.x, lightDir.y});
    float lightSlope = -lightNorm.x / lightNorm.y;

    float invR = 1/r;
    for (float j = -r; j < +r; j+=1)
    for (float i = -sqrtf(r*r-j*j)+0.5; i < +r; i+=2) {
        float thisr2 = i*i + j*j;
        if (thisr2 > r*r) break;

        int si = i + pos.x;
        int sj = j + pos.y;
        if (si < 0) {i = -pos.x; continue;}
        if (si >= WIDTH) break;
        if (sj < 0) {j = -pos.y; continue;}
        if (sj >= HEIGHT) return;

        Vec2 ftex = mtx22MulVec(texTransform, (Vec2){
            (thisr2*i+i)*invR*invR*invR*8,
            (thisr2*j+j)*invR*invR*invR*8,
        });

        int tx = (int)(ftex.x+texOffset.x+65536)%(sizeof(uint64_t)*8/BPP);
        int ty = (int)(ftex.y+texOffset.y+65536)%(sizeof(uint64_t)*8/BPP);

        uint8_t pix = (noiseMap[ty] >> (tx*4)) & 0xf;

        // FRAMEBUFFER->SCREEN[(sj*WIDTH + si)/2] = (sx<<4) | sy;

        if ((pix)>0) {
            FRAMEBUFFER->SCREEN[(sj*WIDTH + si)/2] = 0x66;
        }

        tx = (int)((ftex.x+texOffset.x)*4+65536)%(sizeof(uint64_t)*8/BPP);
        ty = (int)((ftex.y+texOffset.y)*4+65536)%(sizeof(uint64_t)*8/BPP);

        pix = (noiseMap[ty] >> (tx*4)) & 0xf;

        if ((pix)>1) {
            FRAMEBUFFER->SCREEN[(sj*WIDTH + si)/2] = 0x44;
        }

        float xp = (lightNorm.x * i + lightNorm.y * j) * invR;
        float yp = (lightNorm.y * i - lightNorm.x * j) * invR;
        bool betweenTerminators = xp*xp + lightDir.z*lightDir.z*yp*yp < lightDir.z*lightDir.z;
        bool onLitSide = (j - i*lightSlope)*lightNorm.y > 0;
        if (lightDir.z > 0) {
            if (onLitSide && !betweenTerminators) {
                FRAMEBUFFER->SCREEN[(sj*WIDTH + si)/2] |= 0x88;
            }
        } else {
            if (onLitSide || betweenTerminators) {
                FRAMEBUFFER->SCREEN[(sj*WIDTH + si)/2] |= 0x88;
            }
        }
    }

    // Looks like TIC-80 doesn't support simd :(
    // Not like I should have expected it to though
    //int16_t iposx = pos.x;
    //int16_t iposy = pos.y;
    //int16_t ir = r;
    //v128_t irs = wasm_i16x8_splat(ir);

    //int16_t leftBound = pos.x-r;
    //int16_t rightBound = pos.x+r;
    //int16_t topBound = pos.y-r;
    //int16_t bottomBound = pos.y+r;

    //if (leftBound < 0) leftBound = 0;
    //if (topBound < 0) topBound = 0;
    //if (rightBound > WIDTH) rightBound = WIDTH;
    //if (bottomBound > HEIGHT) bottomBound = HEIGHT;

    //int hOffset = leftBound%sizeof(uint32_t);
    //leftBound -= hOffset;

    //for (int j = topBound; j < bottomBound; j+=sizeof(uint32_t))
    //for (int i = leftBound; i < rightBound; i+=sizeof(uint32_t)) {
    //    int16_t xsOffsets[] = {0, 1, 2, 3, 4, 5, 6, 7};
    //    v128_t xs = wasm_i16x8_add(wasm_i16x8_load8x8(xsOffsets), wasm_i16x8_splat(i-hOffset));
    //    v128_t ys = wasm_i16x8_splat(i-hOffset);

    //    xs = wasm_i16x8_sub(xs, irs);
    //    ys = wasm_i16x8_sub(ys, irs);

    //    v128_t theseR2 = wasm_i16x8_add(wasm_i16x8_mul(xs, xs), wasm_i16x8_mul(ys, ys));
    //    if (!wasm_v128_any_true(wasm_i16x8_lt(theseR2, wasm_i16x8_mul(irs, irs)))) continue;

    //    for (int d = 0; d < 8; d++) {
    //        int16_t* data = &theseR2;
    //        int16_t c = data[d]/1000;
    //        FRAMEBUFFER->SCREEN[(j*WIDTH + i + d)/2] |= (c&0xf) << (c&1);
    //    }
    //}
}

uint64_t frame = 0;

#define SKYBOX_STAR_COUNT 64
Vec3 skyboxStars[SKYBOX_STAR_COUNT];

#define ORBIT_LINE_RESOLUTION 20
typedef struct{
    Vec3 points[ORBIT_LINE_RESOLUTION];
} OrbitLine;

OrbitLine* generateOrbitLine(Vec3 parent, Vec3 position) {
    OrbitLine* ret = aalloc(persistAllocator, sizeof(OrbitLine));
    Mtx33 nextPointMtx = rotY(PI / ORBIT_LINE_RESOLUTION * 2);

    ret->points[0] = v3Sub(position, parent);
    for (int i = 1; i < ORBIT_LINE_RESOLUTION; i++) {
        ret->points[i] = mtx33MulVec(nextPointMtx, ret->points[i-1]);
    }

    for (int i = 0; i < ORBIT_LINE_RESOLUTION; i++) {
        ret->points[i] = v3Add(ret->points[i], parent);
    }

    return ret;
}

typedef struct {
    float radius;
} Star;
typedef struct {
    float radius;
    OrbitLine* orbitLine;
    uint8_t colorLand[3];
    uint8_t colorSea[3];
    uint8_t colorClouds[3];
} Planet;

typedef struct {
    enum {
        INVALID = 0,
        STAR,
        PLANET,
    } type;
    Vec3 position;
    char* tag;
    union {
        Star star;
        Planet planet;
    } info;
} CelestialBody;

#define MAX_SYSTEM_OBJECT_COUNT 4
CelestialBody celestialBodies[MAX_SYSTEM_OBJECT_COUNT] = {0};

CelestialBody generatePlanet() {
    enum {
        TERRESTRIAL,
        PLANET_VARIETY,
    } kind = rands(0) % PLANET_VARIETY;

    CelestialBody planet = {0};
    planet.position = mtx33MulVec(rotYA((float)(rands(0)%4096)), (Vec3){(rands(0)%10 + 4) * (rands(0)%10 + 4), 0, 0});
    planet.tag = generateName();
    planet.type = PLANET;
    planet.info.planet.orbitLine = generateOrbitLine((Vec3){0, 0, 0}, planet.position);

    switch (kind) {
        case TERRESTRIAL:
            planet.info.planet.radius = ((rands(0)%128 + 64) / 196.0);
            planet.info.planet.colorLand  [0] = 0x4C;
            planet.info.planet.colorLand  [1] = 0x81;
            planet.info.planet.colorLand  [2] = 0x28;
            planet.info.planet.colorSea   [0] = 0x79;
            planet.info.planet.colorSea   [1] = 0x9D;
            planet.info.planet.colorSea   [2] = 0xFF;
            planet.info.planet.colorClouds[0] = 0xF4;
            planet.info.planet.colorClouds[1] = 0xF4;
            planet.info.planet.colorClouds[2] = 0xF4;
            break;
        case PLANET_VARIETY: break;
    }

    return planet;
}

typedef struct {
    Vec3 realPos;
    Vec3 relPos;
    Vec3 visPos;
    float dist;
    float distInv;
    float dz;
    Vec2 screenspacePos;
} BodyVisualInfo;
void initBodyVisualInfo(BodyVisualInfo *self, Vec3 bodyPos, Vec3 playerPos, Mtx33 invPlayerRot){
    self->realPos = bodyPos;
    self->relPos = v3Sub(bodyPos, playerPos);
    self->visPos = mtx33MulVec(invPlayerRot, self->relPos);
    self->dist = v3Len(self->relPos);
    self->distInv = 1/self->dist;
    self->dz = HEIGHT / self->visPos.z;
    self->screenspacePos = (Vec2){self->visPos.x*self->dz + FWIDTH/2, self->visPos.y*self->dz + FHEIGHT/2};
}

typedef struct {
    Mtx33 rot;
    Mtx33 invRot;
    Vec3 forward;
} PlayerVisualInfo;
void initPlayerVisualInfo(PlayerVisualInfo* self) {
    self->rot = playerRot;
    self->invRot = invUnscaled(self->rot);
    self->forward = mtx33MulVec(playerRot, (Vec3){0, 0, 1});
}

void drawOrbitLine(PlayerVisualInfo* pvi, OrbitLine* orbitLine) {
    if (enabledMenuOpts & MENU_OPTION_ORRERY) {
        Vec3 lastVisPoint = mtx33MulVec(pvi->invRot, v3Sub(orbitLine->points[ORBIT_LINE_RESOLUTION-1], playerPos));
        for (int i = 0; i<ORBIT_LINE_RESOLUTION; i++) {
            Vec3 thisVisPoint = mtx33MulVec(pvi->invRot, v3Sub(orbitLine->points[i], playerPos));
            if (thisVisPoint.z > 0 || lastVisPoint.z > 0) {
                float ldz = HEIGHT/lastVisPoint.z;
                if (ldz < 0) ldz *= -1000;
                float tdz = HEIGHT/thisVisPoint.z;
                if (tdz < 0) tdz *= -1000;
                line(lastVisPoint.x * ldz + FWIDTH/2, lastVisPoint.y * ldz + FHEIGHT/2, thisVisPoint.x * tdz + FWIDTH/2, thisVisPoint.y * tdz + FHEIGHT/2, 3);
            }
            lastVisPoint = thisVisPoint;
        }
    }
}

void drawPlanet(PlayerVisualInfo* pvi, BodyVisualInfo* bvi, Planet* planet) {
    drawOrbitLine(pvi, planet->orbitLine);

    if (bvi->visPos.z > 0) {
        Vec3 visPole = v3Sub(mtx33MulVec(pvi->invRot, v3Add(bvi->relPos, (Vec3){0, 1, 0})), bvi->visPos);
        Vec2 poleDir = v2Norm((Vec2){visPole.x, visPole.y});
        //Vec3 visLightPos = mtx33MulVec(pvi->invRot, v3Sub((Vec3){0, 0, 0}, playerPos));
        //Vec3 lightDir = mtx33MulVec(mtx33LookAt(v3Sub(visLightPos, bvi->visPos)), (Vec3){0, 0, 1});
        //Vec3 lightDir = (Vec3){0.707, 0.001, -0.707};
        Vec3 lightDir = v3Norm(mtx33MulVec(pvi->invRot, v3Sub((Vec3){0, 0, 0}, bvi->realPos)));
        drawPlanetSurface(bvi->screenspacePos, (FHEIGHT/2)*planet->radius*bvi->distInv, 
            (Vec2){
                -asinf(bvi->relPos.y*bvi->distInv)*8, 
                acosf(bvi->relPos.x*bvi->distInv)*(bvi->relPos.z<0?1:-1)*INV_PI*8
            },(Mtx22){
                poleDir.x, poleDir.y,
               -poleDir.y, poleDir.x,
            },
            lightDir
        );
    }
}

void drawStar(PlayerVisualInfo* pvi, BodyVisualInfo* bvi, Star* star) {
    if (bvi->visPos.z > 0) {
        circ(bvi->screenspacePos.x, bvi->screenspacePos.y, (FHEIGHT/2)*star->radius*bvi->distInv, 8);
    }
}

void draw() {
    PlayerVisualInfo pvi = {0};
    initPlayerVisualInfo(&pvi);

    for (int i = 0; i<SKYBOX_STAR_COUNT; i++) {
        Vec3 vsPos = mtx33MulVec(pvi.invRot, skyboxStars[i]);
        if (vsPos.z <= 0) continue;
        float dz = HEIGHT / vsPos.z;
        pix(vsPos.x*dz + FWIDTH/2, vsPos.y*dz + FHEIGHT/2, 8);
    }

    for (int i = 0; i<MAX_DEFERRED_POINTS_OF_INTEREST; i++) deferredPOIs[i] = (DeferredPOI){0};

    {
        int nextDeferredPOIIndex = 0;
        for (int i = 0; i<MAX_SYSTEM_OBJECT_COUNT; i++) {
            CelestialBody body = celestialBodies[i];

            if (body.type == INVALID) continue;

            BodyVisualInfo bvi;
            initBodyVisualInfo(&bvi, body.position, playerPos, pvi.invRot);
            if (bvi.visPos.z > 0) {
                deferredPOIs[nextDeferredPOIIndex] = (DeferredPOI) {
                    .show = true,
                    .selected = true,
                    .tag = body.tag,
                    .screenPos = bvi.screenspacePos,
                };
                nextDeferredPOIIndex++;
            }

            if (body.type == PLANET) {
                drawPlanet(&pvi, &bvi, &body.info.planet);
            }
            if (body.type == STAR) {
                drawStar(&pvi, &bvi, &body.info.star);
            }
        }
    }
}

WASM_EXPORT("BOOT")
void BOOT() {
    initAllocators(&tickAllocator, &persistAllocator);

    for (int i = 0; i<SKYBOX_STAR_COUNT; i++) {
        float x = (float)rands(0) * ((rands(0)& 8)?-1:1);
        float y = (float)rands(0) * ((rands(0)& 4)?-1:1);
        float z = (float)rands(0) * ((rands(0)&32)?-1:1);

        skyboxStars[i] = (Vec3){x, y, z};
    }

    int nextOrbitLineIndex = 0;

    celestialBodies[0] = (CelestialBody){
        .type = STAR,
        .position = {0, 0, 0},
        .tag = "STAR",
        .info = {.star = (Star){
            .radius = 10,
        }},
    };

    celestialBodies[1] = generatePlanet();
    celestialBodies[2] = generatePlanet();
    celestialBodies[3] = generatePlanet();
}

WASM_EXPORT("TIC")
void TIC() {
    screenShake = v2Scale(screenShake, 0.5);

    if (!btn(7)) {
        if (btn(0)) {playerRot = mtx33Mul(playerRot, rotX(-0.01)); screenShake.y -= 3;}
        if (btn(1)) {playerRot = mtx33Mul(playerRot, rotX( 0.01)); screenShake.y += 3;}
        if (btn(2)) {playerRot = mtx33Mul(playerRot, rotZ(-0.01));}
        if (btn(3)) {playerRot = mtx33Mul(playerRot, rotZ( 0.01));}
    } else {
        if (btn(0)) {enabledMenuOpts |= 1 << menuOpt;}
        if (btn(1)) {enabledMenuOpts &= ~(1 << menuOpt);}
        if (btn(2) && !(lastButtonInputs&4)) {menuOpt--;}
        if (btn(3) && !(lastButtonInputs&8)) {menuOpt++;}
        if (menuOpt<0) menuOpt = 0;
        if (menuOpt>=MENU_OPT_COUNT) menuOpt = MENU_OPT_COUNT-1;
    }

    Mtx33 const invPlayerRot = invUnscaled(playerRot);
    Vec3 const playerForward = mtx33MulVec(playerRot, (Vec3){0, 0, 1});

    {
        float oldPlayerSpeed = playerSpeed;
        if (btn(4)) {playerSpeed += 0.1 * SECONDS_PER_FRAME;}
        if (btn(5)) {playerSpeed -= 0.1 * SECONDS_PER_FRAME;}
        if (enabledMenuOpts&MENU_OPTION_DAMPENERS && !btn(4) && !btn(5)) {
            if (fabsf(playerSpeed) > 0.05 * SECONDS_PER_FRAME) {
                if (playerSpeed<0) playerSpeed += 0.05 * SECONDS_PER_FRAME;
                else playerSpeed -= 0.05 * SECONDS_PER_FRAME;
                playerSpeed *= 0.99;
            } else {
                playerSpeed = 0;
            }
        }

        screenShake.x += ((((int)time() * 118517) % 1024) / 512.0 - 1) * fminf(playerSpeed - oldPlayerSpeed, 0.05) * 100;
        screenShake.y += ((((int)time() * 193141) % 1024) / 512.0 - 1) * fminf(playerSpeed - oldPlayerSpeed, 0.05) * 100;
    }

    playerPos = v3Add(playerPos, v3Scale(playerForward, playerSpeed*SECONDS_PER_FRAME));

    cls(0);
    draw();
    drawBackHud();
    drawCockpit();
    drawFrontHud();

    frame++;
    lastButtonInputs = GAMEPADS[0];
    aempty(tickAllocator);
}

WASM_EXPORT("BDR")
void BDR() {
}

#ifdef TESTING
// for testing
void printMtx(Mtx33 mtx) {
    printf("|%8.4f\t%8.4f\t%8.4f|\n", mtx.aa, mtx.ab, mtx.ac);
    printf("|%8.4f\t%8.4f\t%8.4f|\n", mtx.ba, mtx.bb, mtx.bc);
    printf("|%8.4f\t%8.4f\t%8.4f|\n", mtx.ca, mtx.cb, mtx.cc);
}
int main() {
    Mtx33 rx = rotX(0.3);
    printf("rx:\n");
    printMtx(rx);

    Mtx33 ry = rotY(0.3);
    printf("ry:\n");
    printMtx(ry);

    Mtx33 mul = mtx33Mul(rx, ry);
    printf("mul:\n");
    printMtx(mul);

    Mtx33 invMul = invUnscaled(mul);
    printf("invMul:\n");
    printMtx(invMul);

    uint8_t data[0x10000];
    void* startOfUsableMemory = data;

    tickAllocator.info = startOfUsableMemory; startOfUsableMemory += sizeof(PairTickAllocInfo);
    *(PairTickAllocInfo*)tickAllocator.info = (PairTickAllocInfo){0};
    tickAllocator.operate = &pairTickAllocOp;

    persistAllocator.info = startOfUsableMemory; startOfUsableMemory += sizeof(PairPersistAllocInfo);
    *(PairPersistAllocInfo*)persistAllocator.info = (PairPersistAllocInfo){0};
    persistAllocator.operate = &pairPersistAllocOp;

    ((PairTickAllocInfo*)tickAllocator.info)->head = startOfUsableMemory;
    ((PairTickAllocInfo*)tickAllocator.info)->start = startOfUsableMemory;
    ((PairTickAllocInfo*)tickAllocator.info)->other = persistAllocator.info;

    ((PairPersistAllocInfo*)persistAllocator.info)->head = data+0x10000;
    ((PairPersistAllocInfo*)persistAllocator.info)->start = data+0x10000;
    ((PairPersistAllocInfo*)persistAllocator.info)->other = tickAllocator.info;

    printf("%s\n", generateName());
    printf("%s\n", generateName());
    printf("%s\n", generateName());

    for (float i = 0; i < 2*PI; i += 0.1) {
        printf("sin(%6.6f)=%6.6f\n", i, sinfa(i));
    }

    CelestialBody cb = generatePlanet();
}
#endif
