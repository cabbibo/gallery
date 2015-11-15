#version 330 core


const float MAX_TRACE_DISTANCE = 50.;           // max trace distance
const float INTERSECTION_PRECISION = 0.001;        // precision of the intersection
const int NUM_OF_TRACE_STEPS = 100;
const float PI  = 3.14159;

uniform float uTime;

in vec3 vPos;
in vec3 vEye;
in vec3 vNorm;

in vec3 vHand1;
in vec3 vHand2;

in vec2 vUV;

out vec4 color;


float sdBox( vec3 p, vec3 b )
{
  vec3 d = abs(p) - b;
  return min(max(d.x,max(d.y,d.z)),0.0) +
         length(max(d,0.0));
}


// ROTATION FUNCTIONS TAKEN FROM
//https://www.shadertoy.com/view/XsSSzG
mat3 xrotate(float t) {
  return mat3(1.0, 0.0, 0.0,
                0.0, cos(t), -sin(t),
                0.0, sin(t), cos(t));
}

//--------------------------------
// Modelling 
//--------------------------------
vec2 map( vec3 pos ){
  pos -= vec3( 0. , 0., .2);
  mat3 r = xrotate( uTime );
  vec2 res = vec2( sdBox(r * pos , vec3( .03 , .06 , .09 ) ) , 1. );

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

  }


  if( abs(vUV.x - .5) > .49 ||  abs(vUV.y - .5) > .49 ){
    col = vec3(1.);
  }


  color = vec4( col , 1. );



}