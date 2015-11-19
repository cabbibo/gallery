#version 330 core


const float MAX_TRACE_DISTANCE = 50.;           // max trace distance
const float INTERSECTION_PRECISION = 0.001;        // precision of the intersection
const int NUM_OF_TRACE_STEPS = 100;
const float PI  = 3.14159;

in vec3 vPos;
in vec3 vEye;
in vec3 vNorm;

in vec3 vHand1;
in vec3 vHand2;
in vec3 vLight;

in vec2 vUV;

out vec4 color;




float sdSphere( vec3 p, float s ){
  return length(p)-s;
}

float opRepSphere( vec3 p, vec3 c , float r)
{
    vec3 q = mod(p,c)-0.5*c;
    return sdSphere( q  , r );
}


//--------------------------------
// Modelling 
//--------------------------------
vec2 map( vec3 pos ){
  pos -= vec3( 0. , 0., .2);

  vec2 res = vec2( opRepSphere( pos , vec3( .1 ) ,.03 ) , 1. );

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


vec3 fogColor( float dist , vec3 color , vec3 fog, float falloff ){

  float val = falloff * pow( dist , 2.);
  return mix( color , fog , val );

}



float calcLamb( in vec3 lightDir , in vec3 normal ){

  return max( 0. , dot( -lightDir , normal ));

}


vec3 render( vec3 ro , vec3 rd , in vec2 res ){

  vec3 col = vec3( 0. );

  if( res.y > -.5 ){

    vec3 pos = ro + rd  * res.x;
    vec3 norm = calcNormal( pos );
    vec3 lightDir = normalize( pos - vLight);
    vec3 refl     = reflect( lightDir , norm );

    float lamb = calcLamb( lightDir , norm);

    col = vec3( lamb );

  }

  col = fogColor( res.x , col , vec3( 1. , 0. , 0.) ,3.);

  return col;

}

void main(){

  vec3 ro = vPos;
  vec3 rd = normalize( vPos - vEye );


  vec3 handDir1 = normalize( vHand1 - ro);
  vec3 handDir2 = normalize( vHand2 - ro);

  vec2 res = calcIntersection( ro , rd );

  vec3 col = render( ro , rd , res );

  if( abs(vUV.x - .5) > .49 ||  abs(vUV.y - .5) > .49 ){
    col = vec3(1.);
  }


  color = vec4( col , 1. );



}