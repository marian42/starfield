const float FLIGHT_SPEED = 12.0;

const float DRAW_DISTANCE = 60.0;
const float FADEOUT_DISTANCE = 40.0; // must be < DRAW_DISTANCE    
const float FIELD_OF_VIEW = radians(60.0);   

const float STAR_SIZE = 0.4; // must be > 0 and < 1
const float STAR_CORE_SIZE = 0.2;    

const float CLUSTER_SCALE = 0.02;
const float STAR_THRESHOLD = 0.6;

// https://stackoverflow.com/questions/4200224/random-noise-functions-for-glsl
float rand(vec2 co){
    return fract(sin(dot(co.xy ,vec2(12.9898,78.233))) * 43758.5453);
}

//
// Description : Array and textureless GLSL 2D/3D/4D simplex 
//               noise functions.
//      Author : Ian McEwan, Ashima Arts.
//  Maintainer : stegu
//     Lastmod : 20110822 (ijm)
//     License : Copyright (C) 2011 Ashima Arts. All rights reserved.
//               Distributed under the MIT License. See LICENSE file.
//               https://github.com/ashima/webgl-noise
//               https://github.com/stegu/webgl-noise
// 
    vec3 mod289(vec3 x) {
      return x - floor(x * (1.0 / 289.0)) * 289.0;
    }

    vec4 mod289(vec4 x) {
      return x - floor(x * (1.0 / 289.0)) * 289.0;
    }

    vec4 permute(vec4 x) {
         return mod289(((x*34.0)+1.0)*x);
    }

    vec4 taylorInvSqrt(vec4 r)
    {
      return 1.79284291400159 - 0.85373472095314 * r;
    }

    float snoise(vec3 v)
      { 
      const vec2  C = vec2(1.0/6.0, 1.0/3.0) ;
      const vec4  D = vec4(0.0, 0.5, 1.0, 2.0);

    // First corner
      vec3 i  = floor(v + dot(v, C.yyy) );
      vec3 x0 =   v - i + dot(i, C.xxx) ;

    // Other corners
      vec3 g = step(x0.yzx, x0.xyz);
      vec3 l = 1.0 - g;
      vec3 i1 = min( g.xyz, l.zxy );
      vec3 i2 = max( g.xyz, l.zxy );

      //   x0 = x0 - 0.0 + 0.0 * C.xxx;
      //   x1 = x0 - i1  + 1.0 * C.xxx;
      //   x2 = x0 - i2  + 2.0 * C.xxx;
      //   x3 = x0 - 1.0 + 3.0 * C.xxx;
      vec3 x1 = x0 - i1 + C.xxx;
      vec3 x2 = x0 - i2 + C.yyy; // 2.0*C.x = 1/3 = C.y
      vec3 x3 = x0 - D.yyy;      // -1.0+3.0*C.x = -0.5 = -D.y

    // Permutations
      i = mod289(i); 
      vec4 p = permute( permute( permute( 
                 i.z + vec4(0.0, i1.z, i2.z, 1.0 ))
               + i.y + vec4(0.0, i1.y, i2.y, 1.0 )) 
               + i.x + vec4(0.0, i1.x, i2.x, 1.0 ));

    // Gradients: 7x7 points over a square, mapped onto an octahedron.
    // The ring size 17*17 = 289 is close to a multiple of 49 (49*6 = 294)
      float n_ = 0.142857142857; // 1.0/7.0
      vec3  ns = n_ * D.wyz - D.xzx;

      vec4 j = p - 49.0 * floor(p * ns.z * ns.z);  //  mod(p,7*7)

      vec4 x_ = floor(j * ns.z);
      vec4 y_ = floor(j - 7.0 * x_ );    // mod(j,N)

      vec4 x = x_ *ns.x + ns.yyyy;
      vec4 y = y_ *ns.x + ns.yyyy;
      vec4 h = 1.0 - abs(x) - abs(y);

      vec4 b0 = vec4( x.xy, y.xy );
      vec4 b1 = vec4( x.zw, y.zw );

      //vec4 s0 = vec4(lessThan(b0,0.0))*2.0 - 1.0;
      //vec4 s1 = vec4(lessThan(b1,0.0))*2.0 - 1.0;
      vec4 s0 = floor(b0)*2.0 + 1.0;
      vec4 s1 = floor(b1)*2.0 + 1.0;
      vec4 sh = -step(h, vec4(0.0));

      vec4 a0 = b0.xzyw + s0.xzyw*sh.xxyy ;
      vec4 a1 = b1.xzyw + s1.xzyw*sh.zzww ;

      vec3 p0 = vec3(a0.xy,h.x);
      vec3 p1 = vec3(a0.zw,h.y);
      vec3 p2 = vec3(a1.xy,h.z);
      vec3 p3 = vec3(a1.zw,h.w);

    //Normalise gradients
      vec4 norm = taylorInvSqrt(vec4(dot(p0,p0), dot(p1,p1), dot(p2, p2), dot(p3,p3)));
      p0 *= norm.x;
      p1 *= norm.y;
      p2 *= norm.z;
      p3 *= norm.w;

    // Mix final noise value
      vec4 m = max(0.6 - vec4(dot(x0,x0), dot(x1,x1), dot(x2,x2), dot(x3,x3)), 0.0);
      m = m * m;
      return 42.0 * dot( m*m, vec4( dot(p0,x0), dot(p1,x1), 
                                    dot(p2,x2), dot(p3,x3) ) );
      }

vec3 getRayDirection(vec2 fragCoord, vec3 cameraDirection) {
    vec2 uv = fragCoord.xy / iResolution.xy;
	
    const float screenWidth = 1.0;
    float originToScreen = screenWidth / 2.0 / tan(FIELD_OF_VIEW / 2.0);
    
    vec3 screenCenter = originToScreen * normalize(cameraDirection);
    vec3 baseX = normalize(cross(screenCenter, vec3(0, -1.0, 0)));
    vec3 baseY = normalize(cross(screenCenter, baseX));
    
    return normalize(screenCenter + (uv.x - 0.5) * baseX + (uv.y - 0.5) * iResolution.y / iResolution.x * baseY);
}

float getDistance(ivec3 chunkPath, vec3 localStart, vec3 localPosition) {
    return length(vec3(chunkPath) + localPosition - localStart);
}

void move(inout vec3 localPosition, vec3 rayDirection) {
    vec3 directionSign = vec3(rayDirection.x > 0.0 ? 1.0 : -1.0, rayDirection.y > 0.0 ? 1.0 : -1.0, rayDirection.z > 0.0 ? 1.0 : -1.0);
	vec3 bound = vec3(0.5, 0.5, 0.5) + 0.5 * directionSign;
    
    vec3 amountVector = (bound - directionSign * localPosition) / abs(rayDirection);
    
    float amount = min(amountVector.x, min(amountVector.y, amountVector.z));
    
    localPosition += amount * rayDirection;
}

vec3 getStarToRayVector(vec3 rayBase, vec3 rayDirection, vec3 starPosition) {
	float r = (dot(rayDirection, starPosition) - dot(rayDirection, rayBase)) / dot(rayDirection, rayDirection);
    vec3 pointOnRay = rayBase + r * rayDirection;
    return starPosition - pointOnRay;
}

float getStarBrightness(float starDistance, float brightnessFactor) {
    float full = 1.0 * brightnessFactor;
    if (starDistance < STAR_CORE_SIZE) {
        return full * 2.0;
    }
    return 0.25 * full * (1.0 - pow((starDistance - STAR_CORE_SIZE) / (1.0 - STAR_CORE_SIZE), 1.0));
}

// Makes sure that each component of localPosition is >= 0 and <= 1
void moveInsideBox(inout vec3 localPosition, inout ivec3 chunk, vec3 directionSign) {
    vec3 bound = vec3(0.5, 0.5, 0.5) + 0.5 * directionSign;
    if (localPosition.x * directionSign.x >= bound.x - 0.001) {
        localPosition.x -= directionSign.x;
        chunk.x += int(directionSign.x);
    } 
    if (localPosition.y * directionSign.y >= bound.y - 0.001) {
        localPosition.y -= directionSign.y;
        chunk.y += int(directionSign.y);
    } 
    if (localPosition.z * directionSign.z >= bound.z - 0.001) {
        localPosition.z -= directionSign.z;
        chunk.z += int(directionSign.z);
    }
}

int getStarSeed(ivec3 chunk) {
	return chunk.x * 242343423 + chunk.y * 32323575 + chunk.z * 234345734;
}

vec3 getStarPosition(ivec3 chunk, float starSize) {
    vec3 position = abs(vec3(rand(vec2(float(chunk.x) / float(chunk.y) + 0.24, float(chunk.y) / float(chunk.z) + 0.66)),
                             rand(vec2(float(chunk.x) / float(chunk.z) + 0.73, float(chunk.z) / float(chunk.y) + 0.45)),
                             rand(vec2(float(chunk.y) / float(chunk.x) + 0.12, float(chunk.y) / float(chunk.z) + 0.76))));
    
    return starSize * vec3(1.0, 1.0, 1.0) + (1.0 - 2.0 * starSize) * position;
}

bool hasStar(ivec3 chunk) {
    return texture2D(iChannel0, mod(CLUSTER_SCALE * (vec2(chunk.xy) + vec2(chunk.zx)) + vec2(0.724, 0.111), 1.0)).g > STAR_THRESHOLD
        && texture2D(iChannel0, mod(CLUSTER_SCALE * (vec2(chunk.xz) + vec2(chunk.zy)) + vec2(0.333, 0.777), 1.0)).g > STAR_THRESHOLD;
}

// http://lolengine.net/blog/2013/07/27/rgb-to-hsv-in-glsl
vec3 hsv2rgb(vec3 c) {
    vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
    vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
    return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}

vec3 getNebulaColor(vec3 globalPosition, vec3 rayDirection) {
    vec3 color = vec3(0.0, 0.0, 0.0);
    float spaceLeft = 1.0;    
    
    const float layerDistance = 20.0;
    float rayLayerStep = rayDirection.z / layerDistance;
    
    const int steps = 4;
    for (int i = 0; i <= steps; i++) {
    	vec3 noiseeval = globalPosition + rayDirection * ((1.0 - fract(globalPosition.z / layerDistance) + float(i)) * layerDistance / rayDirection.z);
    	
        float value = 0.4 * pow(snoise(0.03 * noiseeval), 2.5);
        
        if (i == 0) {
            value *= 1.0 - fract(globalPosition.z / layerDistance);
        }
        if (i == steps) {
            value *= fract(globalPosition.z / layerDistance);
        }
        
        float hue = mod(noiseeval.z / layerDistance / 34.444, 1.0);
        
        color += spaceLeft * hsv2rgb(vec3(hue, 1.0, value));
        spaceLeft = max(0.0, spaceLeft - value * 2.0);
    }
    return color;
}


void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    //float k = 0.4;
    //vec3 movementDirection = vec3(sin(k * iGlobalTime), 0.0,  cos(k * iGlobalTime));
    vec3 movementDirection = normalize(vec3(0.5, 0.0, 1.0));
    
    vec3 rayDirection = getRayDirection(fragCoord, movementDirection);
    vec3 directionSign = vec3(rayDirection.x > 0.0 ? 1.0 : -1.0, rayDirection.y > 0.0 ? 1.0 : -1.0, rayDirection.z > 0.0 ? 1.0 : -1.0);
    
    vec3 globalPosition = vec3(3.14159, 3.14159, 0.0) + (iGlobalTime + 1000.0) * FLIGHT_SPEED * movementDirection;
    ivec3 chunk = ivec3(globalPosition);
    vec3 localPosition = mod(globalPosition, 1.0);
    moveInsideBox(localPosition, chunk, directionSign);
    
    // TODO compute starting position
        
    ivec3 startChunk = chunk;
    vec3 localStart = localPosition;
    
    float brightness = 0.0;
    
    for (int i = 0; i < 100; i++) {
        move(localPosition, rayDirection);
        moveInsideBox(localPosition, chunk, directionSign);
        
        if (hasStar(chunk)) {
            vec3 starPosition = getStarPosition(chunk, 0.5 * STAR_SIZE);       

            float currentDistance = getDistance(chunk - startChunk, localStart, starPosition);
            if (currentDistance > DRAW_DISTANCE) {
                break;
            }

            vec3 starToRayVector = getStarToRayVector(localPosition, rayDirection, starPosition);

            float distanceToStar = length(starToRayVector);
            float starMaxBrightness = min(1.0, (DRAW_DISTANCE - currentDistance) / FADEOUT_DISTANCE);
            if (distanceToStar < 0.5 * STAR_SIZE) {
                brightness += getStarBrightness(distanceToStar / (0.5 * STAR_SIZE), starMaxBrightness);
                if (brightness >= 1.0) {
                    break;
                }
            }
        }
    }
    
    brightness = min(1.0, brightness);
    
    vec3 color = getNebulaColor(globalPosition, rayDirection);    
    color += brightness * vec3(1.0, 1.0, 1.0);
    
    if (iGlobalTime < 1.0) {
        color *= iGlobalTime;
    }
    fragColor = vec4(color, 1.0);
}