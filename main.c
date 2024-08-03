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

/**
 * Convert an integer to a string
 * @param buf output. Must be long enough for any potential number
 * @param n number to convert
 * @return length of written string
 */
int writeInt(char* buf, int n);

/**
 * Write an int to the TIC-80's console
 * @param n number to print
 */
void traceNum(int n) {
    char buf[32] = {0};
    writeInt(buf, n);
    trace(buf, 1);
}

/**
 * Specifies which memory operation to do
 * @see MemFn
 * @see Allocator
 */
typedef enum {
    ALLOC,
    FREE,
    REALLOC,
    EMPTY,
} MemOperator;

/**
 * Function pointer for a particular allocator
 * @param info Arbitrary internal data for the allocator
 * @param operation Which memory operation is expected
 * @param ptr In/Out reference to the pointer to act on
 * @param oldSize The size of memory currently pointed at by *ptr
 * @param newSize The expected size of memory pointed at by *ptr after calling
 * @see MemOperator
 * @see Allocator
 */
typedef void (*MemFn)(void* info, MemOperator operation, void** ptr, size_t oldSize, size_t newSize);

/**
 * Abstract struct for any allocator implementation
 * @see MemOperator
 * @see MemFn
 */
typedef struct {
    /**
     * Function that defines how this allocator manages memory
     *
     * @see bufferAllocOp for an example
     *
     * @see MemFn
     */
    MemFn operate;

    /**
     * Arbitrary data for a given allocator, passed with every allocation
     * operation for this allocator
     *
     * @see BufferAllocInfo for an example
     */
    void* info;
} Allocator;

/**
 * Abstracted memory allocation operation
 *
 * @param a Which allocator to retrieve the memory from
 * @param amount Number of bytes to retrieve
 * @return pointer to the allocated memory
 */
void* aalloc(Allocator a, size_t amount) {
    void* ret = NULL;
    a.operate(a.info, ALLOC, &ret, 0, amount);
    return ret;
}
/**
 * Abstracted memory free operation
 *
 * @param a Which allocator the memory came from
 * @param what Pointer to the memory to free
 * @param amount Size of the memory getting freed
 */
void afree(Allocator a, void* what, size_t amount) {
    a.operate(a.info, FREE, &what, amount, 0);
}
/**
 * Abstracted memory reallocation operation
 *
 * @param a Which allocator the memory came from
 * @param what In/Out reference to pointer that will have data resized
 * @param oldSize Current size of the memory pointed to by *ptr
 * @param newSize Desired size of the memory pointed to by *ptr
 */
void arealloc(Allocator a, void** what, size_t oldSize, size_t newSize) {
    a.operate(a.info, REALLOC, what, oldSize, newSize);
}
/**
 * Abstracted memory empty operation
 *
 * This frees all of the data of a particular allocator, invalidating all
 * pointers allocated by it. @see afree for freeing just a single pointer
 *
 * @param a Allocator to empty
 */
void aempty(Allocator a) {
    a.operate(a.info, EMPTY, NULL, 0, 0);
}

// Included because certain c stdlib stuff won't get linked for whatever reason
void* memcpy(void* restrict dest, void const * restrict src, size_t n) {
    for (int i = 0; i < n; i++) *(uint8_t*)(dest++) = *(uint8_t*)(src++);
    return dest-n;
}
size_t strlen(char const * s) {
    if (s == NULL) return 0;
    size_t n = 0;
    while(*(s++)) n++;
    return n;
}

/**
 * Move memory from one allocator to another
 *
 * @param oldPtr reference to the pointer to move
 * @param size Number of bytes pointed to by *oldPtr
 * @param from The allocator *oldPtr is currently pointing to
 * @param to The allocator to move the memory to
 */
void changeAllocator(void** oldPtr, size_t size, Allocator from, Allocator to) {
    void* newPtr = aalloc(to, size);
    memcpy(newPtr, *oldPtr, size);
    afree(from, *oldPtr, size);
    *oldPtr = newPtr;
}

typedef struct {
    void* head;
    void* buf;
    size_t bufsize;
} BufferAllocInfo;

/**
 * Buffer allocator implementation
 *
 * SUPPORTS:
 * - ALLOC
 * - FREE (only for last alloc)
 * - REALLOC (only for last alloc)
 * - EMPTY
 */
void bufferAllocOp(void* info, MemOperator op, void** ptr, size_t oldSize, size_t newSize) {
    BufferAllocInfo* a = info;
    size_t osAligned = ((oldSize-1)/8 + 1)*8;
    size_t nsAligned = ((newSize-1)/8 + 1)*8;

    switch (op) {
        case ALLOC:
            if (a->head + nsAligned > a->buf + a->bufsize) {
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
            a->head = a->buf;
            break;
    }
}

/**
 * Create a buffer allocator, using another allocator as a backing
 *
 * @param out Where to put the buffer allocator
 * @param from Where the memory for the buffer allocator comes from
 * @param size The maximum data to be managed by the buffer allocator
 */
void initBufferAllocator(Allocator* out, Allocator from, size_t size) {
    BufferAllocInfo* bai = aalloc(from, sizeof(BufferAllocInfo));

    *bai = (BufferAllocInfo){0};
    bai->buf = aalloc(from, size);
    bai->head = bai->buf;
    bai->bufsize = size;

    *out = (Allocator){
        .info = bai,
        .operate = bufferAllocOp,
    };
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

/**
 * Set up the tick and persist allocators using the memory given by the TIC-80
 */
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

/**
 * Allocator for memory needed for a single frame. All allocations from this
 * allocator are freed at the end of the frame
 *
 * @see PairTickAllocInfo
 * @see pairTickAllocOp
 */
Allocator tickAllocator;
/**
 * Allocator for memory kept between frames.
 *
 * @see PairPersistAllocInfo
 * @see pairPersistAllocOp
 */
Allocator persistAllocator;

/**
 * Allocator for the current system the player is in
 */
Allocator currentSystemAllocator;

/**
 * @return the amount of memory free between the tick and persist allocators
 */
size_t freeMemory() {
    return ((PairPersistAllocInfo*)persistAllocator.info)->head - ((PairTickAllocInfo*)tickAllocator.info)->head;
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

/**
 * Inverts a 3x3 matrix, but without scaling by the determinant. Useful if you
 * know the determinant of your matrix is 1
 */
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

Mtx22 mtx22Mul(Mtx22 lhs, Mtx22 rhs) {
    return (Mtx22){
        lhs.aa*rhs.aa + lhs.ab*rhs.ba, lhs.aa*rhs.ab + lhs.ab*rhs.bb,
        lhs.ba*rhs.aa + lhs.bb*rhs.ba, lhs.ba*rhs.ab + lhs.bb*rhs.bb,
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

/**
 * Approximate sin, included because for some reason -lm isn't working
 */
float sinfa(float);
/**
 * Approximate cos, included because for some reason -lm isn't working
 */
float cosfa(float);

/**
 * 3x3 matrix to rotate around X, approximate
 */
Mtx33 rotXA(float theta) {
    float s = sinfa(theta);
    float c = cosfa(theta);
    return (Mtx33) {
        1, 0, 0,
        0, c,-s,
        0, s, c,
    };
}
/**
 * 3x3 matrix to rotate around Y, approximate
 */
Mtx33 rotYA(float theta) {
    float s = sinfa(theta);
    float c = cosfa(theta);
    return (Mtx33) {
        c, 0, s,
        0, 1, 0,
       -s, 0, c,
    };
}
/**
 * 3x3 matrix to rotate around Z, approximate
 */
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

// Documented in forward declaration
float sinfa(float x) {
    x *= 1/(2*PI);
    x -= (int)x;
    if (x < 0.5) return -8 * x * (2*x - 1);
    else return 8 * x * (2*x - 3) + 8;
}
// Documented in forward declaration
float cosfa(float x) {
    return sinfa(x + PI/2);
}

// Documented in forward declaration
int writeInt(char* buf, int x) {
    if (x == 0) {
        buf[0] = '0';
        buf[1] = 0;
        return 1;
    }

    bool negated = false;
    if (x < 0) {
        x *= -1;
        *buf = '-';
        buf++;
        negated = true;
    }

    char* p;
    for (p = buf; x > 0; p++) {
        int m = x%10;
        x /= 10;
        *p = '0'+m;
    }
    *p = 0;

    int ret = p-buf + (negated? 1:0);

    p--;

    for (char* s = buf; s<p; s++,p--) {
        char tmp = *p;
        *p = *s;
        *s = tmp;
    }

    return ret;
}

/**
 * Convert an float to a string
 *
 * @param buf output. Must be long enough for any potential number
 * @param n number to convert
 * @return length of written string
 */
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

    for (int i = 0; i < 3; i++) {
        fpart *= 10;
        *rest = '0'+fpart;
        rest++;
        fpart -= (int)fpart;
    }
    *rest = 0;

    return rest-buf;
}

/**
 * Append to a given string.
 *
 * @param str Reference to string to append to. Assumed to be allocated under @see tickAllocator
 * @param appicand String to append to *str
 */
void stringAppend(char** str, char* appicand) {
    if (appicand == NULL) return;
    if (str == NULL) return;
    if (*str == NULL) return; // could copy appicand into tick alloc if needed

    size_t al = strlen(appicand);
    if (al == 0) return;

    size_t sl = strlen(*str);

    arealloc(tickAllocator, (void**)str, sl+1, sl+al+1);

    for (int i = 0; i < al; i++) {
        (*str)[sl+i] = appicand[i];
    }
    (*str)[sl+al] = '\0';
}

/**
 * Generate a name for a planet / star / etc.
 *
 * @param where Where the return string will be placed
 * @return The generated name
 */
char* generateName(Allocator where) {
    char* output = aalloc(tickAllocator, 1);
    if (output == NULL) return NULL;
    *output = '\0';

    char* consonants = 
        "QU\0"
        "TH\0"
        "SH\0"
        "CH\0"
        "PH\0"
        "W\0\0"
        "R\0\0"
        "T\0\0"
        "Y\0\0"
        "P\0\0"
        "S\0\0"
        "D\0\0"
        "F\0\0"
        "G\0\0"
        "H\0\0"
        "J\0\0"
        "K\0\0"
        "L\0\0"
        "Z\0\0"
        "X\0\0"
        "C\0\0"
        "V\0\0"
        "B\0\0"
        "N\0\0"
        "M\0\0"
        "\0\0\0";
    size_t const MAX_CONSONANT_WIDTH = 3;
    size_t consonant_n = 26;
    char* vowels = 
        "EE\0"
        "OO\0"
        "IE\0"
        "EI\0"
        "AE\0"
        "A\0\0"
        "E\0\0"
        "I\0\0"
        "O\0\0"
        "U\0\0"
        "Y\0\0"
        "\0\0\0";
    size_t const MAX_VOWEL_WIDTH = 3;
    size_t vowel_n = 12;

    int lengthModifier = (rands(0)%3)+2;
    for (int i = 0; i < lengthModifier; i++) {
        size_t consonant_i = (rands(0) % consonant_n)*MAX_CONSONANT_WIDTH;
        size_t vowel_i = (rands(0) % vowel_n)*MAX_VOWEL_WIDTH;
        stringAppend(&output, consonants+consonant_i);
        stringAppend(&output, vowels+vowel_i);

        if (rands(0) % 4 == 0) {
            consonant_i = (rands(0) % consonant_n)*MAX_CONSONANT_WIDTH;
            stringAppend(&output, consonants+consonant_i);
        }
    }

    if (where.info != tickAllocator.info) {
        changeAllocator((void**)&output, strlen(output)+1, tickAllocator, where);
    }
    return output;
}

typedef enum {
    NONE = 0,
    UNIQUE = 1,
    WATER,
} InventoryItemType;

typedef struct {
    char const * name;
    float mass;
} InventoryItemIntrinsics;

typedef struct {
    InventoryItemType type;
    int32_t amount;
} InventoryItemSlot;

/**
 * Get information about a particular item type
 *
 * @param type What item to look up
 * @return information about that item. returns NULL for item types without known information
 */
InventoryItemIntrinsics const * getItemIntrinsics(InventoryItemType type) {
    switch (type) {
        case NONE: return NULL;
        case UNIQUE: return NULL;

        case WATER: {
            static InventoryItemIntrinsics const water = (InventoryItemIntrinsics){
                .name = "water",
                .mass = 10.0,
            };
            return &water;
        }

        default: return NULL;
    }
}

#define PLAYER_INVENTORY_SIZE 12

typedef struct {
    Mtx33 rot;
    Vec3  pos;
    float speed;
    InventoryItemSlot* inventory;
} Player;

Player player = (Player){
    .rot = MATRIX33_IDENTITY,
    .pos = {0, 0, -50},
    .speed = 0,
    .inventory = NULL, // initialized in BOOT
};

/** The currently selected option in the pilot menu */
int8_t toggleableMenuOptSelection = 0;
#ifndef NDEBUG
#define TOGGLEABLE_MENU_OPTION_COUNT 8
#else
#define TOGGLEABLE_MENU_OPTION_COUNT 2
#endif

/** bitfield of options in the pilot menu that are enabled */
uint8_t enabledMenuOpts = 0;
#define TOGGLEABLE_MENU_OPTION_DAMPENERS 0x01
#define TOGGLEABLE_MENU_OPTION_ORRERY 0x02
#define TOGGLEABLE_MENU_OPTION_DEBUG 0x80

int8_t triggerableMenuOptSelection = 0;

#define TRIGGERABLE_MENU_OPTION_COUNT 2

#define TRIGGERABLE_MENU_OPTION_INVENTORY 0
#define TRIGGERABLE_MENU_OPTION_MAP 1

enum {
    IN_WORLD,
    IN_INVENTORY,
} gameState;

/**
 * Defines information about how and where to place markers on screen for
 * various objects. Deferred so that it gets drawn on top of other objects
 */
typedef struct {
    Vec2 screenPos; /** where on screen the center of the sprite is, in terms of pixels */
    char const * tag; /** label to show when selected. Nullable */
    bool show:1; /** whether to draw this. you probably want to enable this */
    bool selected:1; /** whether the player has this POI node focused */
} DeferredPOI;

#define MAX_DEFERRED_POINTS_OF_INTEREST 8
/** list of points to defer this frame */
DeferredPOI deferredPOIs[MAX_DEFERRED_POINTS_OF_INTEREST] = {0};

/** Offset of the cockpit on screen */
Vec2 screenShake;

/**
 *
 */
void drawUIBox(int left, int right, int top, int bottom, uint8_t lightColor, uint8_t darkColor) {
    line( left,    top,  left, bottom, lightColor);
    line(right,    top, right, bottom, lightColor);
    line( left,    top, right,    top, lightColor);
    line( left, bottom, right, bottom, lightColor);

    if (lightColor != darkColor) {
        pix( left,    top, darkColor);
        pix( left, bottom, darkColor);
        pix(right, bottom, darkColor);
        pix(right,    top, darkColor);
    }
}

/**
 * Draw elements of the hud that appear behind the cockpit
 */
void drawBackHud() {
    uint8_t transparentColor = 0;

    if (gameState == IN_WORLD) {
        if (enabledMenuOpts & TOGGLEABLE_MENU_OPTION_DEBUG) {
            char* buf = aalloc(tickAllocator, 64);

            writeFloat(buf, player.speed);
            print(buf, 120, 10, 5, false, 1, false);

            writeFloat(buf, player.pos.x);
            print(buf, 80, 10, 5, false, 1, false);
            writeFloat(buf, player.pos.y);
            print(buf, 80, 20, 5, false, 1, false);
            writeFloat(buf, player.pos.z);
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
}

/**
 * Draw elements of the hud that appear in front of the cockpit
 */
void drawFrontHud() {
    uint8_t transparentColor = 0;

    if (gameState == IN_WORLD) {
        if (btn(7)) {
            int const start = (WIDTH - 16*TOGGLEABLE_MENU_OPTION_COUNT)/2;
            for (int i = 0; i<TOGGLEABLE_MENU_OPTION_COUNT; i++) {
                spr(288+2*i, 
                    start + i*16 + 2*screenShake.x, 
                    20 + 2*screenShake.y, 
                    &transparentColor, 
                    1, 1, false, 0, 2, 2
                );
                if (enabledMenuOpts & (1<<i)) {
                    spr(258, 
                        start + i*16 + 4 + 2*screenShake.x, 
                        36 + 2*screenShake.y, 
                        &transparentColor, 
                        1, 1, false, 0, 1, 1
                    );
                }
            }

            spr(256, start + toggleableMenuOptSelection*16 + 2*screenShake.x, 20 + 2*screenShake.y, &transparentColor, 1, 1, false, 0, 2, 2);

        } else if (btn(6)) {
            int const start = (WIDTH - 16*TRIGGERABLE_MENU_OPTION_COUNT)/2;
            for (int i = 0; i < TRIGGERABLE_MENU_OPTION_COUNT; i++) {
                spr(320+2*i, 
                    start + i*16 + 2*screenShake.x, 
                    20 + 2*screenShake.y, 
                    &transparentColor, 
                    1, 1, false, 0, 2, 2
                );
            }

            spr(256, 
                start + triggerableMenuOptSelection*16 + 2*screenShake.x, 
                20 + 2*screenShake.y, 
                &transparentColor, 
                1, 1, false, 0, 2, 2
            );
        }

        pix(WIDTH/2, HEIGHT/2, 3);

    } else if (gameState == IN_INVENTORY) {
        drawUIBox(50, WIDTH-50, 10, HEIGHT-40, 10, 2);
        drawUIBox(52, 110, 40, HEIGHT-42, 2, 3);
        drawUIBox(112, WIDTH-52, 12, HEIGHT-42, 2, 3);

        print("[Ship stats]", 54, 42, 10, false, 1, true);

        for (size_t i = 0; i < PLAYER_INVENTORY_SIZE; i++) {
            InventoryItemIntrinsics const * iii = getItemIntrinsics(player.inventory[i].type);
            if (iii == NULL) continue;

            char* buf = aalloc(tickAllocator, 16); // TODO: make into to string that implicitly does it this way
            writeInt(buf, player.inventory[i].amount);
            print(buf, 114, 14 + 7*i, 10, false, 1, true);
            print(iii->name, 124, 14 + 7*i, 10, false, 1, true);
        }
    }
}

typedef struct {
    int16_t x;
    int16_t y;
} TrianglePoint;

typedef struct {
    uint16_t pointIds[3];
    uint8_t color:4;
} Triangle;

/**
 * Draw a list of triangles to the screen
 *
 * @param tris Array of tringles to draw
 * @param nTris number of triangles
 * @param pts Array of points indexed by tris
 * @param offset shift of triangles on screen
 */
void drawTriangles(Triangle const * tris, size_t nTris, TrianglePoint const * pts, Vec2 offset) {
    for (int i = 0; i < nTris; i++) {
        Triangle const * t = &tris[i];
        tri(
            pts[t->pointIds[0]].x + offset.x, pts[t->pointIds[0]].y + offset.y, 
            pts[t->pointIds[1]].x + offset.x, pts[t->pointIds[1]].y + offset.y, 
            pts[t->pointIds[2]].x + offset.x, pts[t->pointIds[2]].y + offset.y, 

            t->color
        );
    }
}

void drawCockpit() {
    uint8_t transparentColor = 0;

    TrianglePoint pts[] = (TrianglePoint[]) {
        {     -10, 20       },// 0
        {      50, HEIGHT   },// 1
        {     -10, 30       },// 2
        {      40, HEIGHT   },// 3
        {WIDTH+10, 20       },// 4
        {WIDTH-50, HEIGHT   },// 5
        {WIDTH+10, 30       },// 6
        {WIDTH-40, HEIGHT   },// 7
        {      40, HEIGHT-20},// 8
        {WIDTH-40, HEIGHT-20},// 9
        {     -10, HEIGHT+10},//10
        {WIDTH+10, HEIGHT+10},//11
        {      60, HEIGHT-25},//12
        {WIDTH-60, HEIGHT-25},//13
    };

    Triangle tris[] = (Triangle[]) {
        {{ 0,  1,  2}, 9},
        {{ 1,  3,  2}, 9},
        {{ 4,  5,  6}, 9},
        {{ 5,  7,  6}, 9},
        {{ 8,  9, 10}, 1},
        {{ 9, 11, 10}, 1},
        {{ 9, 13, 12}, 1},
        {{ 8,  9, 12}, 1},
    };

    drawTriangles(tris, 8, pts, screenShake);
}

uint64_t frame = 0;

#define SKYBOX_STAR_COUNT 64
/** Random distant points in 3d space to mimic stars */
Vec3 skyboxStars[SKYBOX_STAR_COUNT];

#define ORBIT_LINE_RESOLUTION 20
/**
 * Static array of points that form a circle (and perhaps in future an ellipse).
 * Used for the orrery. @see TOGGLEABLE_MENU_OPTION_ORRERY
 */
typedef struct{
    Vec3 points[ORBIT_LINE_RESOLUTION];
} OrbitLine;

/**
 * Create a plausible orbit line for an object. Uses the system allocator
 *
 * @param parent The object the object orbits around
 * @param position Where the object is
 * @return The generated orbit line
 */
OrbitLine* generateOrbitLine(Vec3 parent, Vec3 position) {
    OrbitLine* ret = aalloc(currentSystemAllocator, sizeof(OrbitLine));
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

/** Data specific to a star */
typedef struct {
    float radius;
} Star;
/** Data specific to a planet */
typedef struct {
    float radius;

    /** 
     * how "stretched" the surface looks when drawn. Used for gas giants and such
     */
    float surfaceStretchFactor; 

    OrbitLine* orbitLine;

    uint8_t colorClouds[3]; /** rgb triplet */
    uint8_t colorSea[3]; /** rgb triplet */
    uint8_t colorLand[3]; /** rgb triplet */
} Planet;

/**
 * Object present in a solar system
 */
typedef struct {
    enum {
        INVALID = 0,
        STAR,
        PLANET,
    } type;
    Vec3 position;
    char* tag; /** @see DeferredPOI */
    union {
        Star star;
        Planet planet;
    } info;
} CelestialBody;

#define MAX_SYSTEM_OBJECT_COUNT 8
CelestialBody celestialBodies[MAX_SYSTEM_OBJECT_COUNT] = {0};

CelestialBody generatePlanet() {
    enum {
        GAIA,
        DESERT,
        ICY,
        JUPITER,
        PLANET_VARIETY,
    } kind = rands(0) % PLANET_VARIETY;

    CelestialBody planet = {0};
    planet.position = mtx33MulVec(rotYA((float)(rands(0)%4096)), (Vec3){(rands(0)%10 + 4) * (rands(0)%10 + 4), 0, 0});
    planet.tag = generateName(currentSystemAllocator);
    planet.type = PLANET;
    planet.info.planet.orbitLine = generateOrbitLine((Vec3){0, 0, 0}, planet.position);

    uint32_t colorVarier[2] = {~(rands(0)|rands(0)), ~(rands(0)|rands(0))};
    colorVarier[0] &= 0x3f3f3f3f;
    colorVarier[1] &= 0x3f3f3f3f;

    switch (kind) {
        case GAIA:
            planet.info.planet.radius = ((rands(0)%128 + 64) / 196.0);
            planet.info.planet.surfaceStretchFactor = 1.0;
            planet.info.planet.colorClouds[0] = 0xF4;
            planet.info.planet.colorClouds[1] = 0xF4;
            planet.info.planet.colorClouds[2] = 0xF4;
            planet.info.planet.colorSea   [0] = 0x79;
            planet.info.planet.colorSea   [1] = 0x9D;
            planet.info.planet.colorSea   [2] = 0xFF;
            planet.info.planet.colorLand  [0] = 0x4C;
            planet.info.planet.colorLand  [1] = 0x81;
            planet.info.planet.colorLand  [2] = 0x28;
            break;
        case DESERT:
            planet.info.planet.radius = ((rands(0)%128 + 64) / 196.0);
            planet.info.planet.surfaceStretchFactor = 1.0;
            planet.info.planet.colorClouds[0] = 0xF4;
            planet.info.planet.colorClouds[1] = 0xF4;
            planet.info.planet.colorClouds[2] = 0x90;
            planet.info.planet.colorSea   [0] = 0xA8;
            planet.info.planet.colorSea   [1] = 0x88;
            planet.info.planet.colorSea   [2] = 0x4C;
            planet.info.planet.colorLand  [0] = 0xE2;
            planet.info.planet.colorLand  [1] = 0xC8;
            planet.info.planet.colorLand  [2] = 0x77;
            break;
        case ICY:
            planet.info.planet.radius = ((rands(0)%128 + 32) / 256.0);
            planet.info.planet.surfaceStretchFactor = 1.0;
            planet.info.planet.colorClouds[0] = 0xF4;
            planet.info.planet.colorClouds[1] = 0xF4;
            planet.info.planet.colorClouds[2] = 0xF4;
            planet.info.planet.colorSea   [0] = 0x90;
            planet.info.planet.colorSea   [1] = 0x90;
            planet.info.planet.colorSea   [2] = 0x90;
            planet.info.planet.colorLand  [0] = 0xC2;
            planet.info.planet.colorLand  [1] = 0xC8;
            planet.info.planet.colorLand  [2] = 0xC9;
            break;
        case JUPITER:
            planet.info.planet.radius = ((rands(0)%128 + 32) / 64.0);
            planet.info.planet.surfaceStretchFactor = 3.0;
            planet.info.planet.colorClouds[0] = 0x90;
            planet.info.planet.colorClouds[1] = 0x61;
            planet.info.planet.colorClouds[2] = 0x4D;
            planet.info.planet.colorSea   [0] = 0xC8;
            planet.info.planet.colorSea   [1] = 0x8B;
            planet.info.planet.colorSea   [2] = 0x3A;
            planet.info.planet.colorLand  [0] = 0xD3;
            planet.info.planet.colorLand  [1] = 0x9C;
            planet.info.planet.colorLand  [2] = 0x7E;
            break;
        case PLANET_VARIETY: break;
    }

    for (int i = 0; i < 8; i++) {
        planet.info.planet.colorSea[i] ^= ((uint8_t*)(&colorVarier[0]))[i];
    }

    return planet;
}

/**
 * Update TIC-80's palette to render a specific planet
 *
 * @param planet Planet to use palette info from
 */
void setPlanetPalette(Planet const * planet) {
    int const CLOUD_SHADE = 0x4;
    int const WATER_SHADE = 0x5;
    int const LAND_SHADE  = 0x6;
    int const CLOUD_LIT   = 0x8 | CLOUD_SHADE;
    int const WATER_LIT   = 0x8 | WATER_SHADE;
    int const LAND_LIT    = 0x8 | LAND_SHADE ;

    memcpy(&FRAMEBUFFER->PALETTE[CLOUD_LIT*3], &planet->colorClouds[0], 9);
    for (int i = 0; i < 9; i++) {
        FRAMEBUFFER->PALETTE[CLOUD_SHADE*3+i] = planet->colorClouds[i] / 6;
    }
}

/**
 * A 4bpp image of hand-generated noise, values currently ranging from 0-2.
 * 16x16 with each uint64_t representing a full row.
 */
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

/**
 * Render the planet to a particular position on screen
 *
 * @param pos Center of the planet visually on screen, in pixels
 * @param r Visual radius, in pixels
 * @param texOffset Amount to shift noise image, helpful to mimic viewing from different angles
 * @param texTransform Applied to noise texture, helpful for rotating / stretching ground
 * @param lightDir unit direction light comes from, from view perspective
 * @return whether high LOD
 */
bool drawPlanetSurface(Vec2 pos, float r, Vec2 texOffset, Mtx22 texTransform, Vec3 lightDir) {
    if (r < 4) {
        circ(pos.x, pos.y, r, 13);
        return false;
    }

    circ(pos.x, pos.y, r, 5);

    Vec2 lightNorm = v2Norm((Vec2){.x = lightDir.x, lightDir.y});
    float lightSlope = -lightNorm.x / lightNorm.y;

    float invR = 1/r;
    for (float j = -r; j < +r; j+=1) {
    for (float i = -sqrtf(r*r-j*j)+0.5; i < +r; i+=2) {
        float thisr2 = i*i + j*j;
        if (thisr2 > r*r) break;

        int si = i + pos.x;
        int sj = j + pos.y;
        if (si < 0) {i = -pos.x; continue;}
        if (si >= WIDTH) break;
        if (sj < 0) {j = -pos.y; continue;}
        if (sj >= HEIGHT) return true;

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
    }}

    return true;

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

/** information relevant to rendering a celestial body */
typedef struct {
    Vec3 realPos;
    Vec3 relPos;
    Vec3 visPos;
    float dist;
    float distInv;
    float dz;
    Vec2 screenspacePos;
} BodyVisualInfo;
void initBodyVisualInfo(BodyVisualInfo* self, Vec3 bodyPos, Vec3 playerPos, Mtx33 invPlayerRot){
    self->realPos = bodyPos;
    self->relPos = v3Sub(bodyPos, playerPos);
    self->visPos = mtx33MulVec(invPlayerRot, self->relPos);
    self->dist = v3Len(self->relPos);
    self->distInv = 1/self->dist;
    self->dz = HEIGHT / self->visPos.z;
    self->screenspacePos = (Vec2){self->visPos.x*self->dz + FWIDTH/2, self->visPos.y*self->dz + FHEIGHT/2};
}

/** common information relevant to rendering things from the player's perspective */
typedef struct {
    Mtx33 rot;
    Mtx33 invRot;
    Vec3 forward;
} PlayerVisualInfo;
void initPlayerVisualInfo(PlayerVisualInfo* self) {
    self->rot = player.rot;
    self->invRot = invUnscaled(self->rot);
    self->forward = mtx33MulVec(player.rot, (Vec3){0, 0, 1});
}

/**
 * Draw an orbit line to the screen
 *
 * @param pvi Perspective info to draw from
 * @param orbitLine The orbit line to draw
 */
void drawOrbitLine(PlayerVisualInfo const * pvi, OrbitLine const * orbitLine) {
    if (enabledMenuOpts & TOGGLEABLE_MENU_OPTION_ORRERY) {
        Vec3 lastVisPoint = mtx33MulVec(pvi->invRot, v3Sub(orbitLine->points[ORBIT_LINE_RESOLUTION-1], player.pos));
        for (int i = 0; i<ORBIT_LINE_RESOLUTION; i++) {
            Vec3 thisVisPoint = mtx33MulVec(pvi->invRot, v3Sub(orbitLine->points[i], player.pos));
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

/**
 * Draw a planet to the screen
 *
 * @param pvi Perspective info to draw from
 * @param bvi Body info to draw
 * @param planet Planet to draw
 */
void drawPlanet(PlayerVisualInfo const * pvi, BodyVisualInfo const * bvi, Planet const * planet) {
    drawOrbitLine(pvi, planet->orbitLine);

    if (bvi->visPos.z > 0) {
        Vec3 visPole = v3Sub(mtx33MulVec(pvi->invRot, v3Add(bvi->relPos, (Vec3){0, 1, 0})), bvi->visPos);
        Vec2 poleDir = v2Norm((Vec2){visPole.x, visPole.y});
        Vec3 lightDir = v3Norm(mtx33MulVec(pvi->invRot, v3Sub((Vec3){0, 0, 0}, bvi->realPos)));
        Mtx22 texTransform = (Mtx22){
            poleDir.x, poleDir.y,
           -poleDir.y, poleDir.x,
        };
        texTransform = mtx22Mul((Mtx22){planet->surfaceStretchFactor, 0, 0, 1/planet->surfaceStretchFactor}, texTransform);

        bool highLOD = drawPlanetSurface(
            bvi->screenspacePos, 
            (FHEIGHT/2)*planet->radius*bvi->distInv, 
            (Vec2){
                -asinf(bvi->relPos.y*bvi->distInv)*8, 
                acosf(bvi->relPos.x*bvi->distInv)*(bvi->relPos.z<0?1:-1)*INV_PI*8
            },
            texTransform,
            lightDir
        );

        if (highLOD) setPlanetPalette(planet);
    }
}

/**
 * Draw a star to the screen
 *
 * @param pvi Perspective info to draw from
 * @param bvi Body info to draw
 * @param star Star to draw
 */
void drawStar(PlayerVisualInfo const * pvi, BodyVisualInfo const * bvi, Star const * star) {
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
            initBodyVisualInfo(&bvi, body.position, player.pos, pvi.invRot);
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

void triggerCurrentSelection() {
    switch (triggerableMenuOptSelection) {
        case TRIGGERABLE_MENU_OPTION_INVENTORY:
            gameState = IN_INVENTORY;
            break;
        case TRIGGERABLE_MENU_OPTION_MAP:
            break;
        default:
            break;
    }
}

void processInput() {
    switch (gameState) {
    case IN_WORLD:
        if (btn(7)) {
            if (btn(0)) {enabledMenuOpts |= 1 << toggleableMenuOptSelection;}
            if (btn(1)) {enabledMenuOpts &= ~(1 << toggleableMenuOptSelection);}
            if (btn(2) && !(lastButtonInputs&4)) {toggleableMenuOptSelection--;}
            if (btn(3) && !(lastButtonInputs&8)) {toggleableMenuOptSelection++;}
            if (toggleableMenuOptSelection<0) toggleableMenuOptSelection = 0;
            if (toggleableMenuOptSelection>=TOGGLEABLE_MENU_OPTION_COUNT) toggleableMenuOptSelection = TOGGLEABLE_MENU_OPTION_COUNT-1;
        } else if (btn(6)) {
            if (btn(0) && !(lastButtonInputs&1)) {triggerCurrentSelection();}
            if (btn(2) && !(lastButtonInputs&4)) {triggerableMenuOptSelection--;}
            if (btn(3) && !(lastButtonInputs&8)) {triggerableMenuOptSelection++;}
            if (triggerableMenuOptSelection<0) triggerableMenuOptSelection = 0;
            if (triggerableMenuOptSelection>=TRIGGERABLE_MENU_OPTION_COUNT) triggerableMenuOptSelection = TRIGGERABLE_MENU_OPTION_COUNT-1;
        } else {
            if (btn(0)) {player.rot = mtx33Mul(player.rot, rotX(-0.01)); screenShake.y -= 3;}
            if (btn(1)) {player.rot = mtx33Mul(player.rot, rotX( 0.01)); screenShake.y += 3;}
            if (btn(2)) {player.rot = mtx33Mul(player.rot, rotZ(-0.01));}
            if (btn(3)) {player.rot = mtx33Mul(player.rot, rotZ( 0.01));}
        }

        {
            float oldPlayerSpeed = player.speed;
            if (btn(4)) {player.speed += 0.1 * SECONDS_PER_FRAME;}
            if (btn(5)) {player.speed -= 0.1 * SECONDS_PER_FRAME;}
            if (enabledMenuOpts&TOGGLEABLE_MENU_OPTION_DAMPENERS && !btn(4) && !btn(5)) {
                if (fabsf(player.speed) > 0.05 * SECONDS_PER_FRAME) {
                    if (player.speed<0) player.speed += 0.05 * SECONDS_PER_FRAME;
                    else player.speed -= 0.05 * SECONDS_PER_FRAME;
                    player.speed *= 0.99;
                } else {
                    player.speed = 0;
                }
            }

            screenShake.x += ((((int)time() * 118517) % 1024) / 512.0 - 1) * fminf(player.speed - oldPlayerSpeed, 0.05) * 100;
            screenShake.y += ((((int)time() * 193141) % 1024) / 512.0 - 1) * fminf(player.speed - oldPlayerSpeed, 0.05) * 100;
        }
        break;

    case IN_INVENTORY:
        if (btn(5)) {
            gameState = IN_WORLD;
            break;
        }

        break;
    }
}

WASM_EXPORT("BOOT")
void BOOT() {
    initAllocators(&tickAllocator, &persistAllocator);

    initBufferAllocator(&currentSystemAllocator, persistAllocator, 8192);

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
    celestialBodies[4] = generatePlanet();
    celestialBodies[5] = generatePlanet();
    celestialBodies[6] = generatePlanet();

    player.inventory = aalloc(persistAllocator, sizeof(InventoryItemSlot) * PLAYER_INVENTORY_SIZE);
    // for testing
    player.inventory[3] = (InventoryItemSlot){
        .type = WATER,
        .amount = 10,
    };
}

WASM_EXPORT("TIC")
void TIC() {
    screenShake = v2Scale(screenShake, 0.5);

    processInput();

    if (gameState == IN_WORLD) {
        Mtx33 const invPlayerRot = invUnscaled(player.rot);
        Vec3 const playerForward = mtx33MulVec(player.rot, (Vec3){0, 0, 1});

        player.pos = v3Add(player.pos, v3Scale(playerForward, player.speed*SECONDS_PER_FRAME));
    }

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

    initBufferAllocator(&currentSystemAllocator, persistAllocator, 8192);

    printf("%d\n", strlen("hello"));

    char* str = aalloc(tickAllocator, 1);
    *str = '\0';

    stringAppend(&str, "test 1");
    stringAppend(&str, "test 2");
    stringAppend(&str, "test G");
    printf("%s\n", str);

    afree(tickAllocator, str, strlen(str)+1);
    str = aalloc(tickAllocator, 1);
    *str = '\0';

    stringAppend(&str, "hello,");
    stringAppend(&str, " ");
    stringAppend(&str, "WORLD!!!");
    printf("%s\n", str);

    printf("%s\n", generateName(tickAllocator));
    printf("%s\n", generateName(tickAllocator));
    printf("%s\n", generateName(tickAllocator));

    for (float i = 0; i < 2*PI; i += 0.1) {
        printf("sin(%6.6f)=%6.6f\n", i, sinfa(i));
    }

    CelestialBody cb = generatePlanet();
}
#endif
