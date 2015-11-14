#version 330 core


const float MAX_TRACE_DISTANCE = 6.;           // max trace distance
const float INTERSECTION_PRECISION = 0.01;        // precision of the intersection
const int NUM_OF_TRACE_STEPS = 30;
const float PI  = 3.14159;

const vec3 lightPos = vec3( 0. , .4 , .3 );

uniform vec3 uDimensions;

in vec3 vPos;
in vec3 vEye;
in vec3 vNorm;

in vec3 vHand1;
in vec3 vHand2;

in vec2 vUV;

out vec4 color;

struct Material
{
  float reflectivity;
  float ambient;
  vec3  color;
};

Material glass  = Material( .9  , 0. , vec3( 0. , 0. , .2 ) );
Material absorb = Material( 0.1 , 0. , vec3( 1. , 0. , 0. ) );
Material light  = Material( 0.  , 1. , vec3( 1. , 1. , 1. ) );




vec3 hsv(float h, float s, float v){
        return mix( vec3( 1.0 ), clamp(( abs( fract(h + vec3( 3.0, 2.0, 1.0 ) / 3.0 )
                   * 6.0 - 3.0 ) - 1.0 ), 0.0, 1.0 ), s ) * v;
      }

float sdSphere( vec3 p, float s ){
  return length(p)-s;
}

float opRepSphere( vec3 p, vec3 c , float r)
{
    vec3 q = mod(p,c)-0.5*c;
    return sdSphere( q  , r  );
}
float sdHexPrism( vec3 p, vec2 h )
{
    vec3 q = abs(p);
    return max(q.z-h.y,max((q.x*0.866025+q.y*0.5),q.y)-h.x);
}

float sdTorus( vec3 p, vec2 t )
{
  vec2 q = vec2(length(p.xy)-t.x,p.z);
  return length(q)-t.y;
}
vec2 smoothU( vec2 d1, vec2 d2, float k)
{
    float a = d1.x;
    float b = d2.x;
    float h = clamp(0.5+0.5*(b-a)/k, 0.0, 1.0);
    return vec2( mix(b, a, h) - k*h*(1.0-h), mix(d2.y, d1.y, pow(h, 2.0)));
}

float sdBox( vec3 p, vec3 b )
{
  vec3 d = abs(p) - b;
  return min(max(d.x,max(d.y,d.z)),0.0) +
         length(max(d,0.0));
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

float sdPlane( vec3 p, vec4 n )
{
  // n must be normalized
  return dot(p,n.xyz) + n.w;
}

float hash( float n ) { return fract(sin(n)*753.5453123); }
float noise( in vec3 x )
{
    vec3 p = floor(x);
    vec3 f = fract(x);
    f = f*f*(3.0-2.0*f);
  
    float n = p.x + p.y*157.0 + 113.0*p.z;

    return mix(mix(mix( hash(n+  0.0), hash(n+  1.0),f.x),
                   mix( hash(n+157.0), hash(n+158.0),f.x),f.y),
               mix(mix( hash(n+113.0), hash(n+114.0),f.x),
                   mix( hash(n+270.0), hash(n+271.0),f.x),f.y),f.z);
}





float fNoise( vec3 p ){
   
    float n;
    
    n += noise( p * 20. ) * .5;
    n += noise( p * 200. ) * .1;
    n += noise( p * 60. ) * .3;
    n += noise( p * 5. );
    
    return n; 
    
}


//--------------------------------
// Modelling 
//--------------------------------
vec2 map( vec3 pos ){

  //tunnel
  vec3 p = pos;
  //p = mod( pos+1.0, 2.0 ) -1.0;

  vec2 res = vec2(-sdBox( pos  , vec3( uDimensions.x * .5 + INTERSECTION_PRECISION * 2.5 ,  uDimensions.y * .5 + INTERSECTION_PRECISION * 2.5  , 1. ) ) , 1. );

  res = opU( res , vec2( sdSphere( p - vec3(0.2 , .2 , 0.3 ), .1  ) , 2. ));
  res = opU( res , vec2( sdSphere( p - vec3(0.0 , .1 , 0.4 ), .1  ) , 2. ));
  res = opU( res , vec2( sdSphere( p - vec3(-0.4 , -.2 , 0.7 ), .1  ) , 2. ));

  res = opU( res , vec2( sdSphere( p - vEye, .04  ) , 3. ));
  //vec2 res = vec2( sdHexPrism( p , vec( .4 ) ) , 1. );
 // res.x = opS( sdBox( pos  , vec3( .26  ) ) , res.x );

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


Material getMat( float v ){

  if( v == 1. ){
    return absorb;
  }else if( v == 2. ){
    return glass;
  }else if( v == 3. ){
    return light;
  }

}


vec3 getColor( vec3 inColor , vec3 roI , vec3 rdI , int raynum ){

  
  vec3 ro = roI;
  vec3 rd = rdI;

  vec3 c = vec3( 0. );

  float rayPower = 1.;

  for( int i= 0; i < raynum; i++ ){

    vec2 res = calcIntersection( ro , rd );

    if( res.y > .5 ){

      Material mat = getMat( res.y );

      vec3 p = ro + rd * res.x;
      vec3 n = calcNormal( p );
      vec3 r = reflect( rd , n );

      float ao = calcAO( p , n );

      vec3 lightDir = vEye - p;

      float l = length( lightDir );

      rayPower -= (1. - mat.reflectivity );

      c += (1. - ao) * .1 *  vec3( .6 , .3 , .2 ) +  (.2 / l ) * (1. - mat.reflectivity) * mat.color * max( 0. , dot( normalize(lightDir), n)) + mat.color * mat.ambient; // .1 * hsv( res.y / 4. , 1. , 1. );
      
      ro = p + r * .02;
      rd = r;

      if( rayPower < 0. ){ break; }

    }else{
      break;
      //return c;
    }

  }

  return c;


}

void main(){

  vec3 ro = vPos;
  vec3 rd = normalize( vPos - vEye );

  vec3 handDir1 = normalize( vHand1 - ro);
  vec3 handDir2 = normalize( vHand2 - ro);

  vec3 col = getColor( vec3( 1. ), ro , rd , 4 );

  /*vec2 res = calcIntersection( ro , rd );


  vec3 col = vec3( 0. );
  if( res.y > .5 ){

    vec3 pos = ro + rd * res.x;

    vec3 handDir1 = normalize( vHand1 - pos);
    vec3 handDir2 = normalize( vHand2 - pos);
    vec3 norm;

    
    norm = calcNormal( pos );

    col = norm * .5 + .5;



  }*/


  if( abs(vUV.x - .5) > .49 ||  abs(vUV.y - .5) > .49 ){
    col = vec3(1.);
  }


  color = vec4( col , 1. );



}