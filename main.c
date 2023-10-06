#ifndef TESTING
#include "tic80.h"
#else
#include "tic80mock.h"
#endif
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define SECONDS_PER_FRAME (1.0/60)
uint8_t lastButtonInputs = 0; // btnp doesn't seem to be working right

#define PI 3.14159
#define INV_PI 1/PI


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

#define MATRIX33_IDENTITY (Mtx33){\
        1, 0, 0,\
        0, 1, 0,\
        0, 0, 1,\
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
float v3LenSqr(Vec3 v) {
    return v.x*v.x + v.y*v.y + v.z*v.z;
}
float v3Len(Vec3 v) {
    return sqrtf(v3LenSqr(v));
}
Vec3 v3Norm(Vec3 v) {
    return v3Scale(v, 1/v3Len(v));
}

/// https://stackoverflow.com/questions/3380628/fast-arc-cos-algorithm/26030435#26030435
float acosf(float x) {
    return (-0.69813170079773212 * x * x - 0.87266462599716477) * x + 1.5707963267948966;
}
float asinf(float x) {
    return PI - acosf(x);
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

Mtx33 playerRot = MATRIX33_IDENTITY;
Vec3 playerPos = {0, 0, -50};
float playerSpeed = 0;
int8_t menuOpt = 0;
#define MENU_OPT_COUNT 3

uint8_t enabledMenuOpts = 0;
#define MENU_OPTION_DAMPENERS 0x01
#define MENU_OPTION_ORRERY 0x02
#define MENU_OPTION_MAP 0x04

typedef struct {
    bool show;
    bool selected;
    Vec2 screenPos;
    char const * tag;
} DeferredPOI;

#define MAX_DEFERRED_POINTS_OF_INTEREST 4
DeferredPOI deferredPOIs[MAX_DEFERRED_POINTS_OF_INTEREST] = {0};

Vec2 screenShake;
void drawCockpit() {
    uint8_t transparentColor = 0;

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

void drawPlanet(Vec2 pos, float r, Vec2 texOffset, Mtx22 texTransform) {
    circ(pos.x, pos.y, r, 13);
    if (r < 4) return;
    
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
            FRAMEBUFFER->SCREEN[(sj*WIDTH + si)/2] = 0xee;
        }

        tx = (int)((ftex.x+texOffset.x)*4+65536)%(sizeof(uint64_t)*8/BPP);
        ty = (int)((ftex.y+texOffset.y)*4+65536)%(sizeof(uint64_t)*8/BPP);

        pix = (noiseMap[ty] >> (tx*4)) & 0xf;

        if ((pix)>1) {
            FRAMEBUFFER->SCREEN[(sj*WIDTH + si)/2] = 0xcc;
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
#define MAX_ORBIT_LINES 4
OrbitLine orbitLines[MAX_ORBIT_LINES] = {0};

OrbitLine generateOrbitLine(float r) {
    OrbitLine ret = {0};
    Mtx33 nextPointMtx = rotZ(PI / ORBIT_LINE_RESOLUTION * 2);

    ret.points[0] = (Vec3){r, 0, 0};
    for (int i = 1; i < ORBIT_LINE_RESOLUTION; i++) {
        ret.points[i] = mtx33MulVec(nextPointMtx, ret.points[i-1]);
    }

    return ret;
}

typedef struct {
    float radius;
} Star;
typedef struct {
    float radius;
    OrbitLine* orbitLine;
} Planet;

typedef struct {
    enum {
        INVALID = 0,
        STAR,
        PLANET,
    } type;
    Vec3 position;
    union {
        Star star;
        Planet planet;
    } info;
} CelestialBody;

#define MAX_SYSTEM_OBJECT_COUNT 4
CelestialBody celestialBodies[MAX_SYSTEM_OBJECT_COUNT] = {0};

WASM_EXPORT("BOOT")
void BOOT() {
    char traceBuf[32];
    uint32_t rand_data = 2985085101;
    for (int i = 0; i<SKYBOX_STAR_COUNT; i++) {
        rand_data = (rand_data * 22695477 + 1);
        float x = (float)rand_data * ((rand_data&8)?-1:1);
        rand_data = (rand_data * 22695477 + 1);
        float y = (float)rand_data * ((rand_data&4)?-1:1);
        rand_data = (rand_data * 22695477 + 1);
        float z = (float)rand_data * ((rand_data&32)?-1:1);

        skyboxStars[i] = (Vec3){x, y, z};
    }

    int nextOrbitLineIndex = 0;

    celestialBodies[0] = (CelestialBody){
        .type = STAR,
        .position = {0, 0, 0},
        .info = {.star = (Star){
            .radius = 10,
        }},
    };

    orbitLines[nextOrbitLineIndex] = generateOrbitLine(40);
    celestialBodies[1] = (CelestialBody){
        .type = PLANET,
        .position = {40, 0, 0},
        .info = {.planet = (Planet){
            .radius = 0.4,
            .orbitLine = &orbitLines[nextOrbitLineIndex],
        }},
    };
    nextOrbitLineIndex++;
}

WASM_EXPORT("TIC")
void TIC() {
    for (int i = 0; i<MAX_DEFERRED_POINTS_OF_INTEREST; i++) deferredPOIs[i].show = false;
    screenShake = v2Scale(screenShake, 0.5);
    cls(0);

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

    for (int i = 0; i<SKYBOX_STAR_COUNT; i++) {
        Vec3 vsPos = mtx33MulVec(invPlayerRot, skyboxStars[i]);
        if (vsPos.z <= 0) continue;
        float dz = HEIGHT / vsPos.z;
        pix(vsPos.x*dz + WIDTH/2, vsPos.y*dz + HEIGHT/2, 8);
    }

    {
        int nextDeferredPOIIndex = 0;
        for (int i = 0; i<MAX_SYSTEM_OBJECT_COUNT; i++) {
            CelestialBody body = celestialBodies[i];

            if (body.type == INVALID) continue;

            Vec3 relPos = v3Sub(body.position, playerPos);
            Vec3 visPos = mtx33MulVec(invPlayerRot, relPos);
            float dist = v3Len(relPos);
            float distInv = 1/dist;
            float dz = HEIGHT / visPos.z;
            Vec2 screenspacePos = (Vec2){visPos.x*dz + WIDTH/2, visPos.y*dz + HEIGHT/2};

            if (body.type == PLANET) {
                if (enabledMenuOpts & MENU_OPTION_ORRERY) {
                    Vec3 lastVisPoint = mtx33MulVec(invPlayerRot, v3Sub(body.info.planet.orbitLine->points[ORBIT_LINE_RESOLUTION-1], playerPos));
                    for (int i = 0; i<ORBIT_LINE_RESOLUTION; i++) {
                        Vec3 thisVisPoint = mtx33MulVec(invPlayerRot, v3Sub(body.info.planet.orbitLine->points[i], playerPos));
                        if (thisVisPoint.z > 0 || lastVisPoint.z > 0) {
                            float ldz = HEIGHT/lastVisPoint.z;
                            if (ldz < 0) ldz *= -1000;
                            float tdz = HEIGHT/thisVisPoint.z;
                            if (tdz < 0) tdz *= -1000;
                            line(lastVisPoint.x * ldz + WIDTH/2, lastVisPoint.y * ldz + HEIGHT/2, thisVisPoint.x * tdz + WIDTH/2, thisVisPoint.y * tdz + HEIGHT/2, 3);
                        }
                        lastVisPoint = thisVisPoint;
                    }
                }

                if (visPos.z > 0) {
                    Vec3 visPole = v3Sub(mtx33MulVec(invPlayerRot, v3Add(relPos, (Vec3){0, 1, 0})), visPos);
                    Vec2 poleDir = v2Norm((Vec2){visPole.x, visPole.y});
                    drawPlanet(screenspacePos, (HEIGHT/2)*body.info.planet.radius*distInv, 
                        (Vec2){
                            -asinf(relPos.y*distInv)*8, 
                            acosf(relPos.x*distInv)*(relPos.z<0?1:-1)*INV_PI*8
                        },(Mtx22){
                            poleDir.x, poleDir.y,
                           -poleDir.y, poleDir.x,
                    });

                    deferredPOIs[nextDeferredPOIIndex] = (DeferredPOI) {
                        .show = true,
                        .selected = true,
                        .tag = "Planet",
                        .screenPos = screenspacePos,
                    };
                    nextDeferredPOIIndex++;
                }

            }
            if (body.type == STAR) {
                if (visPos.z > 0) {
                    circ(screenspacePos.x, screenspacePos.y, (HEIGHT/2)*body.info.star.radius*distInv, 8);

                    deferredPOIs[nextDeferredPOIIndex] = (DeferredPOI) {
                        .show = true,
                        .selected = false,
                        .tag = "Star",
                        .screenPos = screenspacePos,
                    };
                    nextDeferredPOIIndex++;
                }
            }
        }
    }

    char buf[64] = {0};

    writeFloat(buf, playerSpeed);
    print(buf, 120, 10, 5, false, 1, false);

    writeFloat(buf, playerPos.x);
    print(buf, 80, 10, 5, false, 1, false);
    writeFloat(buf, playerPos.y);
    print(buf, 80, 20, 5, false, 1, false);
    writeFloat(buf, playerPos.z);
    print(buf, 80, 30, 5, false, 1, false);

    drawCockpit();

    frame++;
    lastButtonInputs = GAMEPADS[0];
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
}
#endif
