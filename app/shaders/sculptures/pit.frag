#version 330 core

uniform vec3 uDimensions;
uniform float uTime;

const float MAX_TRACE_DISTANCE = 50.;           // max trace distance
const float INTERSECTION_PRECISION = 0.001;        // precision of the intersection
const int NUM_OF_TRACE_STEPS = 100;
const float PI  = 3.14159;

in vec3 vPos;
in vec3 vEye;
in vec3 vNorm;

in vec3 vHand1;
in vec3 vHand2;

in vec2 vUV;

out vec4 color;


float sdSphere( vec3 p, float s ){
  return length(p)-s;
}

float sdBox( vec3 p, vec3 b )
{
  vec3 d = abs(p) - b;
  return min(max(d.x,max(d.y,d.z)),0.0) +
         length(max(d,0.0));
}

float opRepSphere( vec3 p, vec3 c , float r)
{
    vec3 q = mod(p,c)-0.5*c;
    return sdSphere( q  , r );
}

float opU( float d1, float d2 ){
  return min(d1,d2);
}

vec2 opU( vec2 d1, vec2 d2 )
{
    return  d1.x < d2.x ? d1 : d2 ;
}

vec2 smoothU( vec2 d1, vec2 d2, float k)
{
    float a = d1.x;
    float b = d2.x;
    float h = clamp(0.5+0.5*(b-a)/k, 0.0, 1.0);
    return vec2( mix(b, a, h) - k*h*(1.0-h), mix(d2.y, d1.y, pow(h, 2.0)));
}


//--------------------------------
// Modelling 
//--------------------------------
vec2 map( vec3 pos ){

  vec2 res = vec2(-sdBox( pos + vec3( 0. , .75 , 0. )  , vec3( uDimensions.x * .5 + INTERSECTION_PRECISION * 2. ) + vec3(0. , .75 , 0.) ) , 1. );


  res = opU( res , vec2( opRepSphere( pos ,vec3( .07 , .01 , .07 ), .02 * -(pos.y+uDimensions.y/2.) ) , 1. ));
  
  res = smoothU( res , vec2( sdSphere( pos - vec3( 0. , uDimensions.x / 2. , 0.) , .1) , .5 ) , .06);
  res = smoothU( res , vec2( opRepSphere( pos + vec3(0., mod( uTime * .02 , 2. ) , 0.),vec3( .07 , .1 , .07 ), .01 / (pow(length( pos.xz),4.) * 1000000.+ .3)  ) , 1. ) , min( .05 , .01 / (length( pos.xz) * 3.+ .02)));

  return res;

}


vec2 calcIntersection( in vec3 ro, in vec3 rd ){

    float h =  INTERSECTION_PRECISION * 2.0;
    float t = 0.0;
    float res = -1.0;
    float id = -1.;
    
    for( int i=0; i < NUM_OF_TRACE_STEPS ; i++ ){
        
        if( h < INTERSECTION_PRECISION || t > MAX_TRACE_DISTANCE ) break;
        vec2 m = map( ro+rd*t );
        h = m.x;
        t += h;
        id = m.y;
        
    }

    if( t < MAX_TRACE_DISTANCE ) res = t;
    if( t > MAX_TRACE_DISTANCE ) id =-1.0;
    
    return vec2( res , id );
     
}

// Calculates the normal by taking a very small distance,
// remapping the function, and getting normal for that
vec3 calcNormal( in vec3 pos ){
    
  vec3 eps = vec3( 0.001, 0.0, 0.0 );
  vec3 nor = vec3(
      map(pos+eps.xyy).x - map(pos-eps.xyy).x,
      map(pos+eps.yxy).x - map(pos-eps.yxy).x,
      map(pos+eps.yyx).x - map(pos-eps.yyx).x );

  return normalize(nor);
}


void main(){

  vec3 ro = vPos;
  vec3 rd = normalize( vPos - vEye );


  vec3 handDir1 = normalize( vHand1 - ro);
  vec3 handDir2 = normalize( vHand2 - ro);

  vec2 res = calcIntersection( ro , rd );


  vec3 col = vec3( 0. );
  if( res.y > .5 ){

    vec3 pos = ro + rd * res.x;

    vec3 handDir1 = normalize( vHand1 - pos);
    vec3 handDir2 = normalize( vHand2 - pos);
    vec3 norm;

    
    norm = calcNormal( pos );

    col = norm * .5 + .5;

        vec3 refrR = refract( rd , norm , 1. / 1.1 );
    vec3 refrG = refract( rd , norm , 1. / 1.2 );
    vec3 refrB = refract( rd , norm , 1. / 1.3 );


    float dR = -dot( refrR , norm );
    float dG = -dot( refrG , norm );
    float dB = -dot( refrB , norm );


    col = vec3( dR , dG , dB );



  }


  if( abs(vUV.x - .5) > .49 ||  abs(vUV.y - .5) > .49 ){
    col = vec3(1.);
  }


  color = vec4( col , 1. );



}
