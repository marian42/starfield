const float FLIGHT_SPEED = 12.0;

const float DRAW_DISTANCE = 60.0;
const float FADEOUT_DISTANCE = 40.0; // must be < DRAW_DISTANCE    
const float FIELD_OF_VIEW = radians(60.0);   

const float STAR_SIZE = 0.4; // must be > 0 and < 1
const float STAR_CORE_SIZE = 0.2;    

const float CLUSTER_SCALE = 0.02;
const float STAR_THRESHOLD = 0.6;

float rand(vec2 co){
    return fract(sin(dot(co.xy ,vec2(12.9898,78.233))) * 43758.5453);
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
        
    //float brightness = 0.2 * exp(-pow(rayDirection.y * 60.0, 2.0)) + 1.0 * exp(-pow(length(rayDirection - vec3(0.0, 0.0, 1.0)) * 14.0, 2.0));
    float brightness = 0.0;
    
    ivec3 startChunk = chunk;
    vec3 localStart = localPosition;
    
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
    if (iGlobalTime < 1.0) {
        brightness *= iGlobalTime;
    }
    fragColor = vec4(brightness, brightness, brightness, 1.0);
}