#version 330 core


const float MAX_TRACE_DISTANCE = 5.;           // max trace distance
const float INTERSECTION_PRECISION = 0.008;        // precision of the intersection
const int NUM_OF_TRACE_STEPS = 40;
const float PI  = 3.14159;

uniform float uTime;

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

vec3 opCheapBend( vec3 p , float v )
{
    float c = cos(v*p.y);
    float s = sin(v*p.y);
    mat2  m = mat2(c,-s,s,c);
    vec3  q = vec3(m*p.xy,p.z);
    return q;
}

float sdCappedCylinder( vec3 p, vec2 h )
{
  vec2 d = abs(vec2(length(p.xz),p.y)) - h;
  return min(max(d.x,d.y),0.0) + length(max(d,0.0));
}

// ROTATION FUNCTIONS TAKEN FROM
//https://www.shadertoy.com/view/XsSSzG
mat3 xrotate(float t) {
  return mat3(1.0, 0.0, 0.0,
                0.0, cos(t), -sin(t),
                0.0, sin(t), cos(t));
}

mat3 yrotate(float t) {
  return mat3(cos(t), 0.0, -sin(t),
                0.0, 1.0, 0.0,
                sin(t), 0.0, cos(t));
}

mat3 zrotate(float t) {
    return mat3(cos(t), -sin(t), 0.0,
                sin(t), cos(t), 0.0,
                0.0, 0.0, 1.0);
}

float opU( float d1, float d2 ){
  return min(d1,d2);
}

vec2 opU( vec2 d1, vec2 d2 )
{
    return  d1.x < d2.x ? d1 : d2 ;
}

float opS( float d1, float d2 )
{
    return max(-d1,d2);
}
vec2 opS( vec2 d1, vec2 d2 )
{
    return  -d1.x > d2.x ? d1 : d2 ;
}


float rArc( vec3 pos , float speed, float size ){

  mat3 r = zrotate( uTime * speed);
  vec3 p = pos  + vec3( .2 * cos( uTime * speed ), .2 * sin( uTime * speed), 0.);
  p = opCheapBend( r * p , -3.);

  return sdCappedCylinder( p , vec2( size * .2 , size ) );
}

float sdHexPrism( vec3 p, vec2 h )
{
    vec3 q = abs(p);
    return max(q.z-h.y,max((q.x*0.866025+q.y*0.5),q.y)-h.x);
}

float hexRing( vec3 p , float s ){

  float r = sdHexPrism( p , vec2(s , .1 * s ) );
  return opS( sdHexPrism( p , vec2( s - .1 * s , 2. )) , r );

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

  pos -= vec3( 0. , 0. , 1. );
  vec3 ogPos = pos;
  mat3 r;
  vec2 res;

  res = vec2( sdSphere( pos - vec3( .35 , -0.32, 0.12) , .2), 1.);

  r = zrotate( uTime  * .3 );
  pos = r * (ogPos- vec3( .1 , -.1 , 0.) );
  res = smoothU( res , vec2( hexRing( pos, .3 ) , 2. ) , .02 );


  // Rotators
  r = yrotate( .5 * PI );
  pos  = r * ogPos;

  r = xrotate( .25 * PI );
  pos  = r * pos;
  pos -= vec3( .0 , 0.2 , 0.0 );

  res = smoothU( res , vec2( rArc( pos - vec3( 0. , 0. , .16 ), 1.3 , .1 ) , 3. ) , .03 );
  res = smoothU( res , vec2( rArc( pos - vec3( 0. , 0. , .14 ), 1.5 , .1 ) , 3. ) , .03 );
  res = smoothU( res , vec2( rArc( pos - vec3( 0. , 0. , .02 ), 1.7 , .1 ) , 3. ) , .03 );
  res = smoothU( res , vec2( rArc( pos - vec3( 0. , 0. , .08 ), 2.5 , .1 ) , 3. ) , .03 );


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

float calcAO( in vec3 pos, in vec3 nor )
{
  float occ = 0.0;
    float sca = 1.0;
    for( int i=0; i<5; i++ )
    {
        float hr = 0.01 + 0.612*float(i)/4.0;
        vec3 aopos =  nor * hr + pos;
        float dd = map( aopos ).x;
        occ += -(dd-hr)*sca;
        sca *= 0.5;
    }
    return clamp( 1.0 - 3.0*occ, 0.0, 1.0 );    
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
    vec3 lightDir = vEye - pos;
    lightDir = normalize( lightDir );
    float d = max( 0. , dot( lightDir,  norm ) );

    float ao = calcAO( pos , norm );

    if( res.y < 1.9){
      col =  vec3(0. , 0., 1.)* ao;
    }else if( res.y >= 1.9 && res.y < 2.5 ){
      col = vec3( 1. , .8 , .2 ) * ao ;

    }else if( res.y >= 2.5 ){
      col = vec3( 1. , 0. , 0. ) * ao * d;
    }



  }


  if( abs(vUV.x - .5) > .49 ||  abs(vUV.y - .5) > .49 ){
    col = vec3(1.);
  }


  color = vec4( col , 1. );



}

